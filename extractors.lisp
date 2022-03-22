;;; -*- Mode: Common-lisp; Package: Guide; Readtable: Joshua -*-

(in-package :guide)

(defun get-entitys-vacinity (entity &optional (state *current-state*))
  (ask* `[in-state [is-in-vacinity ,entity ?vacinity] ,state]
        (return-from get-entitys-vacinity ?vacinity)))

(defun create-new-typical-vacinity ()
  (intern-object (role-name (make-typical-object 'vacinity)) 'vacinity))

(defun get-or-create-entitys-vacinity (entity &optional (state *current-state*))
  (let ((current-vacinity (get-entitys-vacinity entity state)))
    (unless current-vacinity
      (setq current-vacinity (create-new-typical-vacinity))
      (tell `[in-state [is-in-vacinity ,entity ,current-vacinity] ,state]))
    current-vacinity))

(defun is-not-in-execution-context (relation-properties)
  (eql (second (assoc '|has_tense| relation-properties)) '|to|))

(defun conditonally-tell (predication relation-properties)
  (let ((not-main? (is-not-in-execution-context relation-properties)))
    (if not-main?
        (tell predication :justification :none)
      (tell predication)))
  )

(defun clause-is-negated (main)
  (ask* `[texp-match ?clause :subject ,main :relation-name |is_negative| :object-name |yes|]
        (return-from clause-is-negated t))
  nil)

(defun get-temporal-property-of-clause (main)
  (ask* `[texp-match ?clause :subject ,main :relation-name ?time-relation :object ?property]
        (when (member ?time-relation '(|at| |before| |after| |during|))
          (ask* [texp-match ? :subject ?property :relation-name |related-to| :object ?time-point]
                (return-from get-temporal-property-of-clause (values ?time-relation ?property ?time-point)))
          (return-from get-temporal-property-of-clause (values ?time-relation ?property))))
  nil)

(defun is-anonymous-with-referent (object properties)
  (declare (ignore object))
  (let ((determiner (second (assoc '|has_det| properties)))
        (referent (third (assoc '|related-to| properties))))
    (when (and (eql determiner '|null|)
               (not (null referent)))
      (follow-path (list (full-name referent))))))

(defun is-quantified (instance)
  (ask* `[texp-match ? :subject ,instance :relation-name |has_quantifier| :object-name ?quantification]
        (return-from is-quantified ?quantification))
  nil)

(defun has-relative-clause (thing)
  (ask* `[texp-match ? :subject ,thing :relation-name |has_rel_clause| :object ?rel-clause]
        (return-from has-relative-clause ?rel-clause))
  nil)
