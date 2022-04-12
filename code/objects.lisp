;;; -*- Mode: Common-lisp; Package: Guide; Readtable: Joshua -*-

(in-package :guide)

(define-planning-object-type describes-self-mixin
    )

(defgeneric describe-self (object &optional state))

(Defmethod describe-self ((thing symbol) &optional (state nil))
  (declare (ignore state))
  thing)

(define-planning-object-type can-be-typical-mixin
    :slots ((is-typical? :Initarg :is-typical?)
            (member-of :initarg :member-of)
            ))

(define-planning-object-type tile
    )

(define-planning-object-type vacinity
    :super-types (can-be-typical-mixin)
    )

(define-planning-object-type floor-tile
    :super-types (tile vacinity)
    )

(define-planning-object-type evacuation-area
    :super-types (vacinity))

(define-planning-object-type obstacle
    )

(define-planning-object-type freeze-tile
    :super-types (obstacle floor-tile)
    )

(define-planning-object-type person
    :super-types (can-be-typical-mixin)
    )

;;; this really ought to be on is-typical mixin
;;; but I need to think through the method combination
;;; more if so.
(defmethod describe-self ((thing person) &optional (state (intern-state 'initial)))
  (declare (ignore state))
  (let ((member-of nil)
        (is-typical? nil))
    (ask* `[value-of (,thing member-of) ?class]
          (setq member-of (role-name ?class)))
    (ask* `[value-of (,thing is-typical?) ?typical]
          (setq is-typical? ?typical))
    `(person :name ,(role-name thing)
             ,@(when member-of `(:member-of ,member-of))
             ,@(when is-typical? `(:typical-member yes)))))

(define-planning-object-type player
    :super-types (person)
    )

(define-planning-object-type specialist
    :super-types (player)
    :slots ((team))
    )

(define-planning-object-type medical-specialist
    :super-types (specialist)
    )

(define-planning-object-type transport-specialist
    :super-types (specialist)
    )

(define-planning-object-type victim
    :super-types (person)
    :slots ((needs-transporting :initform t :initarg :needs-transporting)
            (critical :initarg :critical :initform nil)
            (treatment-team :initarg :treatment-team))
    )

(define-planning-object-type tool
    :super-types (can-be-typical-mixin)
    )

(define-planning-object-type medkit
    :super-types (tool))

(define-planning-object-type building
    :super-types (vacinity))

(define-planning-object-type abstract-thing
    )

(define-planning-object-type collection
    :super-types (abstract-thing describes-self-mixin)
    :slots ((quantity :initarg :quantity)
            (member-type :initarg :member-type)
            (typical-member :initarg :typical-member)
            (members :initarg :members :set-valued t)))

(defmethod describe-self ((thing collection) &optional (state (intern-state 'initial)))
  (let ((size nil)
        (typical-member nil)
        (members nil))
    (ask* `[in-state [value-of (,thing quantity) ?quantity] ,state]
          (setq size `(:size ,(if (numberp ?quantity) ?quantity (printable-version ?quantity)))))
    (ask* `[in-state [value-of (,thing typical-member) ?member] ,state]
          (setq typical-member (role-name ?member)))
    (ask* `[in-state [value-of (,thing members) ?member] ,state]
          (push (start::convert-start-string-to-lisp-atom (role-name ?member)) members))
    `(collection :name ,(printable-version (role-name thing))
                 :type ,(printable-version (member-type thing))
                 ,@size
                 ,@(when typical-member `(:typical-member , typical-member))
                 ,@(when members `(:Members ,members))
                 )))

;;; Slight problem with the initform here.
;;; The way Joshua deals with this is that each
;;; subtype calls its parent's initializer
;;; which tells the initform
;;; but if the subtype also has an initform
;;; it tells it too and then there's a conflict.
;;; I guess because both are justified as assumptions
;;; automatic retraction doesn't work
(define-planning-object-type team
    :super-types (collection)
    :slots ((member-type :initform 'player)))

(defmethod describe-self ((thing planning-object) &optional state)
  (declare (ignore state))
  (role-name thing))

;;; A treatment team must have one medical specialist
;;; and at least one other player
;;; The other player can be any type
;;; so we just inherit player as the member type
(define-planning-object-type treatment-team
    :super-types (team)
    :slots ()
    )

(defun printable-version (start-symbol)
  (intern (string-upcase start-symbol)))


(defun make-typical-object (type &rest plist)
  (apply #'make-object type :name (make-name (smash "-" 'typical type)) :is-typical? t plist))


;;; It's messy if these are stateful
(defrule thead-typical-member (:forward)
  if [and [object-type-of ?collection collection]
          [value-of (?collection typical-member) ?member]]
  then [value-of (?member is-typical?) t])

(defrule thead-typical-member-collection (:forward)
  if [and [object-type-of ?collection collection]
          [value-of (?collection typical-member) ?member]]
  then [value-of (?member member-of) ?collection])

(defrule thread-typical-member-backward (:forward)
  if [and [object-type-of ?thing can-be-typical-mixin]
          [value-of (?thing is-typical?) t]
          [value-of (?thing member-of) ?collection]]
  then [value-of (?collection typical-member) ?thing])

(define-planning-object-type care
    :super-types (abstract-thing)
    :slots ((provider :initarg :provider)
            (recipient :initarg :recipient))
    )

(defmethod describe-self ((thing care) &optional (state (intern-state 'initial)))
  (let ((provider nil)
        (recipient nil))
    (ask* `[in-state [value-of (,thing provider) ?provider] ,state]
          (setq provider `(:provider ,(Role-name ?provider))))
    (ask* `[in-state [value-of (,thing recipient) ?recipient] ,state]
          (setq recipient `(:recipient ,(role-name ?recipient))))
    `(care :name ,(printable-version (role-name thing))
           ,@provider
           ,@recipient)))

(define-planning-object-type help
    :super-types (abstract-thing)
    :slots ((provider :initarg :provider)
            (recipient :initarg :recipient)))

(defmethod describe-self ((thing help) &optional (state (intern-state 'initial)))
  (let ((provider nil)
        (recipient nil))
    (ask* `[in-state [value-of (,thing recipient) ?value] ,state]
          (setq recipient ?value))
    (ask* `[in-state [value-of (,thing provider) ?value] ,state]
          (setq provider ?value))
    `(help :name ,(role-name thing)
           ,@(when provider `(:provider ,(printable-version (role-name provider))))
           ,@(when recipient `(:recipient ,(printable-version (role-name recipient)))))))


;;; things to do
;;; write a rule that figures out who the recipient of care is in sentence-2
;;; change rules to fwrd-stateful-rule using value-of-ns predicate for fields
;;; that aren't stateful.
;;; get rid of needs-help predicate.



(define-planning-object-type marker
    :slots ((marker-type))
    ;; do we want to say a marker is a tool?
    :included-object-types (tool))

;;; This is used to represent a refernt to something
;;; in an opaque content such as inside a verb of
;;; knowledge of belief
;;; E.g. He knows the type of the victim

(define-planning-object-type opaque-reference
    :super-types (abstract-thing)
    :slots ((property-name :initarg :property-name)
            (the-object :initarg :the-object)))

(defmethod describe-self ((thing opaque-reference) &optional (state (intern-state 'initial)))
  (declare (ignore state))
  `(the ,(property-name thing) of ,(describe-self (the-object thing))))

(define-planning-object-type set-size-restriction
    :super-types (abstract-thing)
    :slots ((numeric-relation :initarg :Numeric-relation)
            (limit :initarg limit)
            (collection :Initarg :collection)
            ))

(defmethod describe-self ((thing set-size-restriction) &optional (state (intern-state 'initial)))
  (declare (ignore state))
  `(size-of ,(describe-self (collection thing))
            must-be
    ,(numeric-relation thing)
    ,(limit thing)))
