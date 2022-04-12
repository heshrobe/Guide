;;; -*- Mode: Common-lisp; Package: Guide; Readtable: Joshua -*-

(in-package :guide)

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *stories* (make-hash-table)))

(defclass story ()
  ((utterances :accessor utterances :Initform nil :initarg :utterances)
   (background-knowledge :accessor background-knowledge :Initform nil :initarg :background-knowledge)))

(defmacro defstory (name &key utterances background-knowledge)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *stories*)
       (make-instance 'story
         :utterances (list ,@utterances)
         :background-knowledge (list ,@background-knowledge)))))

(defun fetch-story (name) (utterances (gethash name *stories*)))

(defun story-background-knowledge (name) (background-knowledge (gethash name *stories*)))

(defun check-story-parse (name)
  (loop for sentence in (fetch-story name)
      do (print sentence)
         (let ((parse (parse sentence)))
           (print (viz parse)))))


;;; This just has different defaults from parse-text
(defun parse (string &key (server "asist") (kb "yes") (format :xml))
  (parse-text string :server server :kb kb :format format))

;;; just different defaults
(defun flush (&key (kb "asist")) (flush-knowledge-base kb))

;;; just a shorter
(defun viz (parser) (string-for parser :viz))

;;; Only want to do this at load time, not compile time
;;; Story #1 the freeze tile

(defparameter *all-decoders* nil)

(defmacro defdecoder (verb &key return-args pattern handler-name decoder-name new-main)
  (let ((handler-name (or handler-name (smash "-"  verb 'handler)))
        (decoder-name (or decoder-name (smash "-" verb 'decoder)))
        (new-main-lv (or new-main (ji:make-logic-variable-maker (intern (string-upcase "?new-main"))))))
    (pushnew decoder-name *all-decoders*)
    `(defrule ,decoder-name (:backward)
       then [is-appropriate-response ,verb ?main ,handler-name ,new-main-lv ,return-args]
       if ,pattern)))

(defmacro defhandler (name args &body body)
  (let ((name (smash "-" name "handler")))
  `(defun ,name (main ,@args) ,@body)))



;;; A piece of Joshua internals
;;; This meta-predicate checks if its first argument can be derived and succeeds if so.
;;; If it can't it then binds the second argument (a logic variable) to the third argument (a value)
;;;    and then succeeds.
;;; This is useful for checking possible properties of some object
;;; Might it be better to collect all properties and then to succeed with a list of all of them?

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-predicate ifknown (predicate variable value) (tell-error-model default-predicate-model)))

(define-predicate-method (ask ifknown) (truth-value continuation do-backward-rules do-questions)
  (with-statement-destructured (predication variable-to-bind value-to-bind) self
    (unless (predicationp predication)
      (error
       'ji:model-cant-handle-query
       :query self
       :model 'ifknown))
    (let ((query-won nil))
      (ji:ask-internal predication truth-value
                       #'(lambda (just)
                           (setq query-won t)
                           (with-stack-list (winning-derivation self truth-value 'ifknown just)
                                            (funcall continuation winning-derivation)))
                       do-backward-rules
                       do-questions)
      (unless query-won
        (with-unification
         (unify variable-to-bind value-to-bind)
         (with-stack-list (missing-derivation self truth-value 'unknown)
                          (funcall continuation missing-derivation)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-predicate all-properties (entity answer &rest exluded-relationships) (tell-error-model default-predicate-model)))

(define-predicate-method (ask all-properties) (truth-value continuation do-backward-rules do-questions)
  (declare (ignore do-backward-rules do-questions))
  (unless (eql truth-value +true+)
    (error 'ji:model-can-only-handle-positive-queries
           :query self
           :model 'all-properties))
  (with-statement-destructured (entity answer &rest excluded-relations) self
    (with-unification
     (Unify answer (all-properties entity excluded-relations))
     (with-stack-list (derivation self truth-value 'all-properties)
                      (Funcall continuation derivation)))))

(defparameter *syntactic-connectives* '(|is_main| |is_negative| |has_clause_type| |has_tense| |has_person| |has_rel_clause| |has_quantifier| |has_quantity| |has_modifier|))

(defun all-properties (entity &optional rels-to-exclude)
  (let ((answers nil))
    (ask* `[texp-match ?property-clause :subject ,entity :relation-name ?rel-name :object ?obj :object-name ?obj-name]
          (unless (member ?rel-name rels-to-exclude)
            (Push (list ?rel-name ?obj-name ?obj) answers)))
    (copy-object-if-necessary answers)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-predicate all-syntactic-qualifiers (texp answer &rest exluded-relationships) (tell-error-model default-predicate-model)))

(define-predicate-method (ask all-syntactic-qualifiers) (truth-value continuation do-backward-rules do-questions)
  (declare (ignore do-backward-rules do-questions))
  (unless (eql truth-value +true+)
    (error 'ji:model-can-only-handle-positive-queries
           :query self
           :model 'all-syntactic-qualifiers))
  (with-statement-destructured (texp answer &rest excluded-relations) self
    (with-unification
     (Unify answer (all-syntactic-qualifiers texp excluded-relations))
     (with-stack-list (derivation self truth-value 'all-properties)
                      (Funcall continuation derivation)))))

(defun all-syntactic-qualifiers (texp &optional rels-to-exclude)
  (let ((answers nil))
    (ask* `[texp-match ?property-clause :subject ,texp :relation-name ?rel-name :object ?object :object-name ?obj-name]
          (unless (member ?rel-name rels-to-exclude)
            (Push (list ?rel-name ?obj-name ?object) answers)))
    (copy-object-if-necessary answers)))




;;; This code should be moved into planning Core
;;; And appropriate symbols exported

;;; A tester for this
(defmethod  get-handler-main-and-args ((parser core-parser-mixin) continuation)
  (get-mains-and-verb parser
                      #'(lambda (texp verb)
                          ;; (format t "~%doing ~a ~a" verb texp)
                          (ask* `[is-appropriate-response ,verb ,texp ?handler ?args]
                                (funcall continuation ?handler texp (copy-object-if-necessary ?args)))))
  (values))

(defun test-parse (parse) (get-handler-main-and-args parse #'print-all-args))
(defun print-all-args (handler main &rest stuff) (format t "~%Handler: ~a~%Main: ~a~% Args: ~{~a~^,~}" handler main stuff))

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *current-state* (intern-state 'initial))
  (defvar *actors* nil)
  (defvar *tools* nil)
  (defvar *vacinities* nil)
  (defvar *obstacles* nil)
  (defvar *other-objects* nil)
  (defvar *speaker-stack* nil)
  (defvar *current-utterance-stack* nil)
  (defvar *current-vacinity* nil)
  ;; Keeps track of whether we're inside a construction like "he announced that he's coming"
  ;; whatever handles he's coming needs to know that it's inside an utterance context.
  (defvar *embedding-context* nil))

;;; The ask* in handle-sentence below can succeed multiple times.
;;; For flexibility, we can allow the decoder to not bind the handler
;;; and we'll ignore that one.  Of course, it could also just not succeed in such
;;; cases.  Both will work given the code below.
;;; However, if it binds the handler then we should throw out of handle-sentence.

(defparameter *trace-decoder* nil)

(defun enact-story (story-name &key (initial-state *initial-state*) (clear-first t) (file nil))
  (when clear-first
    (clear)
    (setq initial-state *initial-state*))
  (flush-knowledge-base "asist")
  (let ((*current-state* initial-state)
        (*actors* nil)
        (*tools* nil)
        (*vacinities* nil)
        (*other-objects* nil)
        (*current-vacinity* nil)
        (*obstacles* nil)
        (*speaker-stack* nil)
        (*current-utterance-stack* nil)
        (*embedding-context* nil))
    (labels ((handle-sentence (main verb)
               (setq verb (start::convert-start-string-to-lisp-atom verb))
               (block find-one
                 (ask* `[is-appropriate-response ,verb ,main ?handler ?new-main ?args]
                       (when *trace-decoder* (format t "~%Response is ~a ~a ~a ~a" verb main ?handler ?args))
                       (when (not (unbound-logic-variable-p ?handler))
                         (let ((putative-next-state (apply (joshua-logic-variable-value ?handler)
                                                           (if (unbound-logic-variable-p ?new-main) main ?new-main)
                                                           (copy-object-if-necessary ?args))))
                           ;; make sure that the handler returned a state like it's supposed to
                           ;; But it's possible that the handler didn't actually make a state change
                           ;; in which case it should return the previous current state to indicate that
                           (assert (typep putative-next-state 'state))
                           ;; but it's possible that something asserted by an action could
                           ;; cause another action to follow on.  For example, if an announcement is
                           ;; made that someone intends to do something, then you might want to take
                           ;; that action
                           (setq *current-state* (end-of-state-chain putative-next-state)))
                         (return-from find-one)))))
             (do-it (*standard-output*)
               (let ((background-knowledge (story-background-knowledge story-name)))
                 (when background-knowledge
                   (format t "~%(Background knowledge:")
                   (loop for sentence in background-knowledge
                       do (print sentence))
                   (format t "~%)")))
               (loop for utterance in (fetch-story story-name)
                   for parse = (parse utterance)
                   for number from 1
                   do (format t "~2%(Sentence ~d: ~:a" number utterance)
                      (format t "~%(Ternary Expressions:~%")
                      (print (viz parse))
                      (format t "~%))")
                      (get-mains-and-verb parse #'handle-sentence)
                      ;; We're requiring that each utterance be contained
                      ;; within a single multi-main parser
                      (pop *speaker-stack*)
                      (pop *current-utterance-stack*)
                      (pop *embedding-context*))
               (dump-story story-name *current-state*)
               ))
      (if file
          (with-open-file (stream file :direction :output :if-does-not-exist :create :if-exists :supersede)
            (do-it stream))
        (do-it *standard-output*))
      (values *current-state* *actors* *tools* *vacinities* *obstacles* *other-objects*))))

(defmethod accumulate-roles ((thing t)) (values))
(defmethod accumulate-roles ((thing tool)) (pushnew thing *tools*) (values))
(defmethod accumulate-roles ((thing vacinity)) (pushnew thing *vacinities*) (values))
(defmethod accumulate-roles ((thing person)) (pushnew thing *actors*) (values))
(defmethod accumulate-roles ((thing obstacle)) (pushnew thing *obstacles*) (values))
(defmethod accumulate-roles ((thing abstract-thing)) (pushnew thing *other-objects*))

(defmethod intern-object :around ((name symbol) (type symbol) &rest arglist)
  (let ((thing (apply #'call-next-method name type arglist)))
    (accumulate-roles thing)
    thing))

(defun dump-story (story-name final-state &optional (stream *standard-output*))
  (format stream "~2%(Story:  ~a" story-name)
  (format stream "~%(Actors: ~{~s~^~% ~})" (mapcar #'describe-self *actors*))
  (format stream "~%(Tools: ~{~s~^~% ~})" (mapcar #'describe-self *tools*))
  (format stream "~%(obstacles: ~{~s~^~% ~})" (mapcar #'describe-self *obstacles*))
  (format stream "~%(vacinities: ~{~s~^~% ~})" (mapcar #'describe-self *vacinities*))
  (format stream "~%(Other-objects: ~{~s~^~% ~})" (mapcar #'describe-self *other-objects*))
  (format stream "~%(Actions: ~{~s~^~% ~})" (mapcar #'role-name (action-sequence final-state)))
  (terpri stream)
  (format stream "~%(Initial-state: ~%~{~a~^~%~}~%)" (mapcar #'predication-in-dump-format (predications-newly-in-state *initial-state*)))
  (loop for action in (action-sequence final-state)
      for prior-state = (prior-state action)
      for next-state = (next-state action)
      for prior-state-predications = (mapcar #'predication-in-dump-format (predications-newly-in-state prior-state))
      for next-state-predications = (mapcar #'predication-in-dump-format (predications-newly-in-state next-state))
      do (format stream "~2%(Action: ~a type: ~a ~%Operands: ~a ~%Previous-State: ~a ~%Next State: ~a"
                 (role-name action) (name action)
                 (loop for argument in (arguments action)
                       collect (if (predicationp argument)
                                   (predication-in-dump-format argument)
                                 (role-name argument)))
                 (state-name prior-state) (state-name next-state))
      when prior-state-predications
      do (format stream "~%Previous-State-New-Assertion ~%(~{~a~^~%~})" prior-state-predications)
      do (format stream "~%Next-State-New-Assertions: ~%(~{~a~^~%~})" next-state-predications)
         (format stream ")"))
  (format stream "~2%(Depedencies")
  (loop for state in (state-trace final-state)
      do (loop for dependency in (dependency-trace-in-state state)
             do (format stream "~% (~{~a~^~%  ~})" dependency)))
  (loop for action-dependency in (dependency-trace-of-actions final-state)
      do (format stream "~% (~{~a~^~%  ~})" action-dependency))
  (Format stream "~%)")
  (format stream "~%)")
  )

(defun predication-in-dump-format (predication)
  (labels ((do-a-level (thing)
             (etypecase thing
               (symbol thing)
               (number thing)
               (predication (do-a-level (predication-statement thing)))
               (ji::basic-slot (do-a-level (ji::path-name  thing)))
               (list (loop for thing in thing collect (do-a-level thing)))
               (state (state-name thing))
               (action (name thing))
               (opaque-reference (describe-self thing))
               (planning-object (role-name thing)))))
    (if (eql (predication-truth-value predication) +false+)
        `(not ,(do-a-level predication))
      (do-a-level predication))))

(defun dependency-trace-in-state (state)
  (setq state (intern-state state))
  (let ((new-stuff nil))
    (flet ((grabber (just)
             (let ((database-pred (ask-database-predication just)))
               (when database-pred
                 (with-statement-destructured (internal-pred retrieved-state) database-pred
                   (declare (ignore internal-pred))
                   (when (eql retrieved-state state)
                     (push database-pred new-stuff)))))))
      (ask `[in-state ? ,state] #'grabber)
      (ask `[not [in-state ? ,state]] #'grabber))
    (loop for stateful-predication in new-stuff
        for justification = (current-justification stateful-predication)
        collect (cons (predication-in-dump-format stateful-predication)
                      (multiple-value-bind (mnemonic ignore true-support false-support unknown-support certainty)
                          (destructure-justification justification)
                        (declare (ignore ignore unknown-support certainty))
                        (if (or true-support false-support)
                            (mapcar #'predication-in-dump-format (append true-support false-support))
                          (list mnemonic)))))
    ))

(defun dependency-trace-of-actions (final-state)
  (let ((stuff nil))
    (loop for action in (action-sequence final-state)
        collect (ask `[action-taken ,action ? ?]
                     #'(lambda (just)
                         (let* ((predication (ask-database-predication just))
                                (justification (current-justification predication)))
                           (multiple-value-bind (mnemonic ignore true-support false-support unknown-support certainty)
                               (destructure-justification justification)
                             (declare (ignore mnemonic ignore unknown-support certainty))
                             (push (cons (predication-in-dump-format predication)
                                         (mapcar #'predication-in-dump-format (append true-support false-support)))
                                   stuff))))))
  (nreverse stuff)))


(defparameter *Number-words* '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "eleven" "twelve"))

(defun decode-quantity (word)
  (let ((answer ))
    (cond
     ((numberp word) word)
     ((null word) nil)
     ((setq answer (position (string word) *Number-words* :test #'string-equal)) (1+ answer))
     ((setq answer (read-from-string (string word)))
      (when (numberp answer) answer)))))

(defmacro tell-if-new (predication &key (justification nil justification-supplied-p))
  `(block get-out
     (ask ,predication
          (lambda (just) (return-from get-out (values (ask-database-predication just)))))
     (if ,justification-supplied-p
         (tell ,predication :justification ,justification)
       (tell ,predication))))
