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

(defun clause-is-negated (main &optional (all-properties nil properties-supplied-p))
  (let ((answer nil))
    (if properties-supplied-p
        (setq answer (second (assoc '|is_negative| all-properties)))
      (block do-query
        (ask* `[texp-match ?clause :subject ,main :relation-name |is_negative| :object-name ?negative]
              (setq answer ?negative)
              (return-from do-query t))))
      (when answer (convert-start-string-to-lisp-atom answer))))

(defun get-temporal-property-of-clause (main)
  (ask* `[texp-match ?clause :subject ,main :relation-name ?time-relation :object ?property]
        (when (member ?time-relation '(|at| |before| |after| |during|))
          (ask* [texp-match ? :subject ?property :relation-name |related-to| :object ?time-point]
                (return-from get-temporal-property-of-clause (values ?time-relation ?property ?time-point)))
          (return-from get-temporal-property-of-clause (values ?time-relation ?property))))
  )

(defun is-anonymous-with-referent (object properties)
  (declare (ignore object))
  (let ((determiner (second (assoc '|has_det| properties)))
        (referent (third (assoc '|related-to| properties))))
    (when (and (eql determiner '|null|)
               (not (null referent)))
      (follow-path (list (full-name referent))))))

(defun has-quantification (instance &optional (all-properties nil properties-supplied-p))
  (let ((answer nil))
    (if properties-supplied-p
        (setq answer (second (assoc '|has_quantifier| all-properties)))
      (block do-query
        (ask* `[texp-match ? :subject ,instance :relation-name |has_quantifier| :object-name ?quantification]
              (setq answer ?quantification)
              (return-from do-query))))
    (when answer (convert-start-string-to-lisp-atom answer))))

(defun has-quantity (instance &optional (all-properties nil properties-supplied-p))
  (let ((answer nil))
    (if properties-supplied-p
        (setq answer (second (assoc '|has_quantity| all-properties)))
      (block do-query
        (ask* `[texp-match ? :subject ,instance :relation-name |has_quantity| :object-name ?quantification]
              (setq answer ?quantification)
              (return-from do-query))))
    (when answer (convert-start-string-to-lisp-atom answer))))

(defun has-number (instance &optional (all-properties nil properties-supplied-p))
  (let ((answer nil))
    (if properties-supplied-p
        (setq answer (second (assoc '|has_number| all-properties)))
      (block do-query
        (ask* `[texp-match ? :subject ,instance :relation-name |has_number| :object-name ?quantification]
              (setq answer ?quantification)
              (return-from do-query))))
    (when answer (convert-start-string-to-lisp-atom answer))))

(defun has-determiner (instance &optional (all-properties nil properties-supplied-p))
  (let ((answer nil))
    (if properties-supplied-p
        (setq answer (second (assoc '|has_det| all-properties)))
      (block do-query
        (ask* `[texp-match ? :subject ,instance :relation-name |has_det| :object-name ?quantification]
              (setq answer ?quantification)
              (return-from do-query))))
    (when answer (convert-start-string-to-lisp-atom answer))))

(defun has-modifier (instance &optional (all-properties nil properties-supplied-p))
  (let ((answer nil))
    (if properties-supplied-p
        (setq answer (second (assoc '|has_modifier| all-properties)))
      (block do-query
        (ask* `[texp-match ? :subject ,instance :relation-name |has_modifier| :object-name ?quantification]
              (setq answer ?quantification)
              (return-from do-query))))
    (when answer (convert-start-string-to-lisp-atom answer))))

(defun has-property (instance &optional (all-properties nil properties-supplied-p))
  (let ((answer nil))
    (if properties-supplied-p
        (setq answer (second (assoc '|has_property| all-properties)))
      (block do-query
        (ask* `[texp-match ? :subject ,instance :relation-name |has_property| :object-name ?property]
              (setq answer ?property)
              (return-from do-query))))
    (when answer (convert-start-string-to-lisp-atom answer))))

(defun related-to (instance &optional (all-properties nil properties-supplied-p))
  (let ((answer nil))
    (if properties-supplied-p
        (setq answer (second (assoc '|relaed-to| all-properties)))
      (block do-query
        (ask* `[texp-match ? :subject ,instance :relation-name |related-to| :object-name ?property]
              (setq answer ?property)
              (return-from do-query))))
    (when answer (convert-start-string-to-lisp-atom answer))))

(defun has-relative-clause (thing)
  (ask* `[texp-match ? :subject ,thing :relation-name |has_rel_clause| :object ?rel-clause]
        (return-from has-relative-clause ?rel-clause))
  nil)

(defun short-name (thing)
  (if (symbolp thing)
      (convert-start-string-to-lisp-atom thing)
    (convert-start-string-to-lisp-atom (name thing))))

(defun long-name (thing)
  (if (symbolp thing)
      (convert-start-string-to-lisp-atom thing)
    (convert-start-string-to-lisp-atom (full-name thing))))


(defun value-in-state (object property &optional (state *current-state*))
  (block got-it
    (ask* `[in-state [value-of (,object ,property) ?the-value] ,state]
          (return-from got-it ?the-value))
    nil))
