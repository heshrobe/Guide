;;; -*- Mode: Common-lisp; Package: Guide; Readtable: Joshua -*-

(in-package :guide)

(define-recipe-object-type describes-self-mixin
    )

(defgeneric describe-self (object))

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
(defmethod describe-self ((thing person))
  (let ((member-of nil)
        (is-typical? nil))
    (ask* `[value-of (,thing member-of) ?class]
          (setq member-of (role-name ?class)))
    (ask* `[value-of (,thing is-typical?) ?typical]
          (setq is-typical? ?typical))
    `(person :name ,(role-name thing)
             ,@(when member-of `(:member-of ,member-of))
             ,@(when is-typical? `(:typical-member yes)))))

(define-recipe-object-type tool
    :super-types (can-be-typical-mixin)
    )

(define-recipe-object-type medkit
    :super-types (tool))

(define-recipe-object-type building
    :super-types (vacinity))

(define-recipe-object-type collection
    :super-types (describes-self-mixin)
    :slots ((quantity :initarg :quantity)
            (member-type :initarg :member-type)
            (typical-member :initarg :typical-member)
            (members :initarg :members)))

(Defmethod describe-self ((thing symbol)) thing)

(defmethod describe-self ((thing collection))
  (let ((size nil)
        (typical-member nil)
        (members nil))
    (ask* `[value-of (,thing quantity) ?quantity]
          (setq size `(:size ,(if (numberp ?quantity) ?quantity (printable-version ?quantity)))))
    (ask* `[value-of (,thing typical-member) ?member]
          (setq typical-member (role-name ?member)))
    (ask* `[value-of (,thing members) ?members]
          (setq members (mapcar #'role-name ?members)))
    `(collection :name ,(printable-version (role-name thing))
                 :type ,(printable-version (member-type thing))
                 ,@size
                 ,@(when typical-member `(:typical-member , typical-member))
                 ,@(when members `(:Members ,members))
                 )))

(defmethod describe-self ((thing recipe-object)) (role-name thing))

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

(defmethod describe-self ((thing care))
  (let ((provider nil)
        (recipient nil))
    (ask* `[value-of (,thing provider) ?provider]
          (setq provider `(:provider ,(Role-name ?provider))))
    (ask* `[value-of (,thing recipient) ?recipient]
          (setq recipient `(:recipient ,(role-name ?recipient))))
    `(care :name ,(printable-version (role-name thing))
           ,@provider
           ,@recipient)))

(define-recipe-object-type help 
    :super-types (abstract-thing)
    :slots ((provider :initarg :provider)
            (recipient :initarg :recipient)))
                   
(defmethod describe-self ((thing help))
  (let ((provider nil)
        (recipient nil))
    (ask* `[value-of (,thing recipient) ?value]
          (setq recipient ?value))
    (ask* `[value-of (,thing provider) ?value]
          (setq provider ?value))
    `(help :name ,(role-name thing)
           ,@(when provider `(:provider ,(printable-version (role-name provider))))
           ,@(when recipient `(:recipient ,(printable-version (role-name recipient)))))))


;;; things to do
;;; write a rule that figures out who the recipient of care is in sentence-2
;;; change rules to fwrd-stateful-rule using value-of-ns predicate for fields
;;; that aren't stateful.
;;; get rid of needs-help predicate.