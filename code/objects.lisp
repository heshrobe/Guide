;;; -*- Mode: Common-lisp; Package: Guide; Readtable: Joshua -*-

(in-package :guide)

(define-recipe-object-type describes-self-mixin
    )

(defgeneric describe-self (object &optional state))

(define-recipe-object-type can-be-typical-mixin
    :slots ((is-typical? :Initarg :is-typical?)
            (member-of :initarg :member-of)
            ))

(define-recipe-object-type tile
    )

(define-recipe-object-type vacinity
    :super-types (can-be-typical-mixin)
    )

(define-recipe-object-type floor-tile
    :super-types (tile vacinity)
    )

(define-recipe-object-type obstacle
    )

(define-recipe-object-type freeze-tile
    :super-types (obstacle floor-tile)
    )

(define-recipe-object-type person
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

(define-recipe-object-type player
    :super-types (person)
    )

(define-recipe-object-type medical-specialist
    :super-types (player)
    )

(define-recipe-object-type victim
    :super-types (person)
    :slots ((needs-transporting :initform t :initarg :needs-transporting)
            (treatment-team :initarg :treatment-team))
    )

(define-recipe-object-type tool
    :super-types (can-be-typical-mixin)
    )

(define-recipe-object-type medkit
    :super-types (tool))

(define-recipe-object-type building
    :super-types (vacinity))

(define-recipe-object-type collection
    :super-types (abstract-thing describes-self-mixin)
    :slots ((quantity :initarg :quantity)
            (member-type :initarg :member-type)
            (typical-member :initarg :typical-member)
            (members :initarg :members :set-valued t)))

(Defmethod describe-self ((thing symbol) &optional (state nil))
  (declare (ignore state))
  thing)

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
(define-recipe-object-type team
    :super-types (collection)
    :slots ((member-type :initform 'player)))

(defmethod describe-self ((thing recipe-object) &optional state)
  (declare (ignore state))
  (role-name thing))

;;; A treatment team must have one medical specialist
;;; and at least one other player
;;; The other player can be any type
;;; so we just inherit player as the member type
(define-recipe-object-type treatment-team
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

(define-recipe-object-type abstract-thing
    )

(define-recipe-object-type care
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

(define-recipe-object-type help
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
