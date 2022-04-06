;;; -*- Mode: Common-lisp; Package: Guide; Readtable: Joshua -*-

(in-package :guide)

(defstory study-3-1
    :utterances ("At the start of the game, the evacuation specialist didn't have any victims that needed to be transported."
                 "He took the opportunity to scout out and mark victim locations."))

(defdecoder have
    :handler-name ?the-right-handler
    :pattern `[and [texp-match ?main :subject ?subject :object ?object]
                   [all-properties ?object ?object-properties]
                   [unify ?the-right-handler
                          ,(if (clause-is-negated ?main)
                               'does-not-have-handler
                             'has-handler)]]
    :return-args (?subject ?object ?object-properties))

(defdecoder take-the-opportunity
    :pattern [and [texp-match ?main :subject ?subject :object ?object]
                  [all-properties ?subject ?suject-properties]
                  [all-properties ?object ?object-properties]]
    :return-args (?subject ?suject-properties ?object ?object-properties))

;;; This handles negated "have" statements
;;; The object could be quantified or not
;;; In the non-quantified case it's probably something like:
;;; "He doesn't have a car" or "he doesn't have the car"
;;; Notice that if it's an indefinite, it's actually quantified

(defhandler does-not-have (actor object object-properties)
  (multiple-value-bind (relation point-in-time point-of-what) (get-temporal-property-of-clause main)
    (let* ((the-subject (intern-object (full-name actor) 'person))
           (object-type (find-singular-of-noun (name object)))
           (the-thing-possessed (intern-object (full-name object) 'person))
           (number (second (assoc '|has_number| object-properties)))
           (determiner (second (assoc '|has_det| object-properties)))
           (quantifier (is-quantified object))
           (relative-clause (has-relative-clause object)))
      (declare (ignore relation the-thing-possessed point-in-time point-of-what))
      (cond
       ;; there will be other cases, but this is the one I care about now
       ;; the object is quantified by any
       ((and (eql determiner '|any|)
             (eql quantifier '|any|)
             (eql number '|plural|)
             (not (null relative-clause)))
        ;; So this is of the form doesn't have any xxx such that yyy
        (ask* `[texp-match ,relative-clause :subject ,object :object ?object-of-rel-clause :relation-name ?relation-name]
              ;; There are two cases here:
              ;; 1) The object is just an object
              ;; 2) The object is a texp
              (cond
               ((typep ?object-of-rel-clause 'start::texp)
                (ask* `[texp-match ?object-of-rel-clause :subject ,object :relation-name ?rel-name :object-name ?object-of-clause]
                      (tell `[in-state [for-all (x) [not [and [instance-of x ,object-type] [has ,the-subject x] [needs x ,(find-infinitive-of-verb ?object-of-clause)]]]] initial])))))))))
  *current-state*)

(defrule deduce-freedom-1 (:forward)
  :if [in-state [for-all (x) [not [and [instance-of x victim] [has ?who x] [needs x transport]]]] ?state]
  :then [in-state [free-to-scout ?who] ?state])

(defhandler take-the-opportunity (actor actor-properties object object-properties)
  (declare (ignore main actor-properties object-properties object-properties))
  (let ((actor (intern-object (full-name actor) 'person)))
    (block do-it
      (ask* `[texp-match ,object :relation ?to-do-what :object-name ?to-whom]
            (let* ((real-object (intern-object ?to-whom 'vacinity))
                   (action-name (start::convert-start-string-to-lisp-atom (name ?to-do-what)))
                   (before-state-name (smash "-" 'before action-name))
                   (before-state (intern-state before-state-name *current-state*))
                   (after-state-name (smash "-" 'after action-name))
                   (after-state (intern-state after-state-name before-state)))
              (do-action action-name *current-state* before-state after-state actor real-object)
              ;; remember to return the new state
              (return-from do-it after-state))))))
