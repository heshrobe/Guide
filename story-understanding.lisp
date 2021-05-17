;;; -*- Mode: Common-lisp; Package: Guide; Readtable: Joshua -*-

(in-package :guide)

(eval-when (:compile-toplevel :load-toplevel)
(defvar *stories* (make-hash-table)))

(defun fetch-story (name) (gethash name *stories*))

(defmacro defstory (name &body utterances)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *stories*) (list ,@utterances))))

;;; This just has different defaults from parse-text
(defun parse (string &key (server "asist") (kb "yes") (format :xml))
  (parse-text string :server server :kb kb :format format))

;;; just different defaults
(defun flush (&key (kb "asist")) (flush-knowledge-base kb))

;;; just a shorter
(defun viz (parser) (string-for parser :viz))

;;; Only want to do this at load time, not compile time
(defstory rescue-frozen-user
  "Person1 is moving around in the building and accidentally stands on a freeze tile"
  "Person1 calls out for help.  I'm frozen.  Please come and unfreeze me."
  "Person2 has the medkit. Person2 arrives. Person2 unfreezes Person1.  Person1 resumes working."
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Extractors
;;;
;;; Backward chaining rules that figure out who to call and what with arguments
;;; Given a parse of a sentence
;;;
;;; T-exp is a more compact way of matching a parse
;;; it Takes 7 args:
;;; The texp being destructured
;;; The object and its name of the subject
;;; The object and its name of the relation
;;; The object and its name of the object
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule moving-around-decoder (:backward)
  then [is-appropriate-response |move_around| ?main moving-around-handler (?person ?place ?progressive ?tense)]
  if [and
      [texp-and-names ?main ? ?person ? ? ? |null|]
      [texp-and-names ?in-relation ?main ? ? |in| ? ?place]
      ;; bind the tense
      [texp-and-names ?tense-relation ?main ? ? |has_tense| ? ?tense]
      ;; bind whether it's progressive or not.
      ;; This is a really ugly Joshua feature
      [or [texp-and-names ?progressive-relation ?main ? ?  |is_progressive| ? ?progressive]
          [and [not [known [texp-and-names ?progressive-relation ?main ? ?  |is_progressive| ? ?progressive]]]
               (unify ?progressive nil)]]
      ;; make sure it's third peron
      [texp-and-names ?person-relation ?main ? ? |has_person| ? |3|]
     ]
  )

(defrule standing-on-decoder (:backward)
  then [is-appropriate-response |stand| ?main standing-on-freeze-tile-handler (?person ?thing ?tense)]
  if [and
      ;; Make sure that the object is null and bind the subject as a "return value"
      [texp-and-names ?main ? ?person ? ? ? |null|]
      ;; Make sure the relation is "on" and bind ?thing as a return value
      [texp-and-names ?standing-on ?main ? ? |on| ? ?thing]
      ;; bind the tense
      [texp-and-names ?tense-relation ?main ? ? |has_tense| ? ?tense]]
  )

(defrule call-for-help-decoder (:backward)
  then [is-appropriate-response |call_out| ?main call-for-help-handler (?person ?tense)]
  if [and
      ;; Make sure that the object is null and bind the subject as a "return value"
      [texp-and-names ?main ? ?person ? ? ? |null|]
      [texp-and-names ?for-clause ?main ? ? |for| ? |help|]
      ;; bind the tense
      [texp-and-names ?tense-relation ?main ? ? |has_tense| ? ?tense]
      ;; make sure it's third peron
      [texp-and-names ?person-relation ?main ? ? |has_person| ? |3|]]
  )

(defrule frozen-statement-decoder (:backward)
  then [is-appropriate-response |freeze| ?main frozen-handler (?person ?tense)]
  if [and
      [texp-and-names ?main ? |somebody| ? ? ? ?person]
      ;; bind the tense
      [texp-and-names ?tense-relation ?main ? ? |has_tense| ? ?tense]
      ;; make sure it's passive
      [texp-and-names ?passive-relaton ?main ? ? |has_voice| ? |passive|]
      ]
  )

(defrule come-request-decoder (:backward)
  then [is-appropriate-response |come|  ?main come-request-handler (?person ?tense)]
  if [and
      [texp-and-names ?main ? ?person ? |come| ? |null|]
      [texp-and-names ?imperative ?main ? ? |is_imperative| ? |yes|]
      ;; bind the tense
      [texp-and-names ?tense-relation ?main ? ? |has_tense| ? ?tense]
      ;; make sure it's imperative
      [texp-and-names ?imperative-clause ?main ? ? |is_imperative| ? |yes|]
      ;; make sure it's second person
      [texp-and-names ?person-relation ?main ? ? |has_person| ? |2|]
      ]
  )

;;; Could also check for person
(defrule unfreeze-request-decoder (:backward)
  then [is-appropriate-response |unfreeze| ?main unfreeze-request-handler (?person ?tense)]
  if [and
      [texp-and-names ?main ? ?person ? |unfreeze| ? |I|]
      ;; bind the tense
      [texp-and-names ?tense-relation ?main ? ? |has_tense| ? ?tense]
      ;; make sure it's imperative
      [texp-and-names ?imperative-clause ?main ? ? |is_imperative| ? |yes|]
      ;; make sure it's second person
      [texp-and-names ?person-relation ?main ? ? |has_person| ? |2|]
      ]
  )

(defrule decode-has (:backward)
  then [is-appropriate-response |have| ?main has-event-handler (?person ?object ?tense)]
  if [and
      [texp-and-names ?main ? ?person ? |have| ? ?object]
      ;; make sure it's 3rd person
      [texp-and-names ?person-relation ?main ? ? |has_person| ? |3|]
      ;; bind the tense
      [texp-and-names ?tense-relation ?main ? ? |has_tense| ? ?tense]
      ])

(defrule arrive-decoder (:backward)
  then [is-appropriate-response |arrive| ?main arrive-event-handler (?person ?where ?tense)]
  if [and
      [texp-and-names ?main ? ?person ? |arrive| ? |null|]
      ;; insist that it's 3rd person
      [texp-and-names ?person-relation ?main ? ? |has_person| ? |3|]
      ;; bind the tense
      [texp-and-names ?tense-relation ?main ? ? |has_tense| ? ?tense]
      ;; This never says where he arrived (should check that)
      (unify ?where nil)
      ])

(defrule unfreeze-action-decoder (:backward)
  then [is-appropriate-response |unfreeze| ?main do-unfreeze-handler (?actor ?object ?tense)]
  if [and
      [texp-and-names ?main ? ?actor ? |unfreeze| ? ?object]
      ;; make sure it's 3rd person
      [texp-and-names ?person-relation ?main ? ? |has_person| ? |3|]
      ;; bind the tense
      [texp-and-names ?tense-relation ?main ? ? |has_tense| ? ?tense]
      ])

(defrule resumes-decocder (:backward)
  then [is-appropriate-response |resume| ?main resumes-handler (?resumed-action-clause ?person ?resumed-action ?worked-on-object ?tense)]
  if [and
      [texp-and-names ?main ? ?person ? |resume| ?resumed-action-clause ?]
      [texp-and-names ?resumed-action-clause ? ?person ? ?resumed-action ? ?worked-on-object]
      ;; bind the tense
      [texp-and-names ?tense-relation ?main ? ? |has_tense| ? ?tense]
      ;; (break "~{~a~^,~}" (list ?resumed-action-clause ?person ?resumed-action ?worked-on-object ?tense))
      ])




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Handlers for the actions in the story
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moving-around-handler (main who what progressive? tense)
  (declare (ignore main))
  (when (and progressive? (eql tense '|present|))
  (format t "~%Handling move around ~a ~a ~a ~a" who what progressive? tense)
    (let ((person (intern-object who 'person))
          (building (intern-object what 'building)))
      (tell `[in-state [is-in-condition ,person mobile] ,*current-state*])
      (tell `[in-state [is-in-vacinity ,person ,building] ,*current-state*])
      (do-action 'move-around *current-state* 'before-move-around 'after-move-around person building))))

(defun standing-on-freeze-tile-handler (main who thing tense)
  (declare (ignore main))
  (when (eql tense '|present|)
    (format t "~%Handling standing on freeze tile ~a ~a ~a" who thing tense)
    (let ((person (intern-object who 'person))
          (freeze-tile (intern-object thing 'freeze-tile)))
      (setq *current-vacinity* freeze-tile)
      (print *current-vacinity*)
      (do-action 'step-on *current-state* 'before-step-on 'after-step-on person freeze-tile))))

(defun call-for-help-handler (main who tense)
  (declare (ignore main))
  (when (eql tense '|present|)
    (format t "~%Handling ~a's call for help" who)
    (let ((person (intern-object who 'person)))
      (Prog1
       (do-action 'call-for-help *current-state* 'before-call-for-help 'after-call-for-help person)
       (Push person *speaker-stack*)
       (push 'call-for-help *current-utterance-stack*)))))

;;; this handles the observation that the person is frozen
(defun frozen-handler (main who tense)
  (declare (ignore main))
  (when (eql tense '|present|)
    (format t "~%Handling ~a's statement that he is frozen" who)
    (let ((person (if (and  *speaker-stack* (eql who 'I)) (first *speaker-stack*) (intern-object who 'person))))
      ;; we need a data structure for what the current utterance is that will be pushed.
      (do-action 'states-own-condition *current-state* 'before-statement 'after-statement person 'needing-help))))

(defun come-request-handler (main who tense)
  (declare (ignore main))
  (when (eql tense '|present|)
    ;; This is checking if we're in the middle of an multi-sentence utterance
    ;; If so the actor is the current speaker
    ;; Normall "who" is "you" meaning anybody
    (let ((actor (if *speaker-stack* (first *speaker-stack*) (intern-object who 'person)))
          (requestee (if (and *speaker-stack* (eql who '|you|)) 'anybody (intern-object who 'person))))
      (format t "~%Handling ~a's request for someone to come in state ~a" (role-name actor) (state-name *current-state*))
      (do-action 'request-somebody-to-come *current-state* 'before-request-come 'after-request-come actor requestee))))

(defun unfreeze-request-handler (main who tense)
  (declare (ignore main))
  (when (eql tense '|present|)
    (let ((actor (if *speaker-stack* (first *speaker-stack*) (intern-object who 'person)))
          (requestee (if (and *speaker-stack* (eql who '|you|)) 'anybody (intern-object who 'person))))
      (format t "~%Handling ~a's request for someone to unfreeze it in state ~a" (role-name actor) (state-name *current-state*))
      (do-action 'request-somebody-to-unfreeze *current-state* 'before-request-unfreeze 'after-request-unfreeze actor requestee))))

(defun has-event-handler (main who what tense)
  (declare (ignore main))
  (when (eql tense '|present|)
    (let ((actor (if *speaker-stack* (first *speaker-stack*) (intern-object who 'person)))
          (object (intern-object what (if (eql what '|medkit|) 'medkit 'recipe-bject))))
      (format t "~%Handling the fact that ~a has a ~a in state ~a" (role-name actor) (role-name object) (state-name *current-state*))
      (tell `[in-state [is-in-possession-of ,actor ,object] ,*current-state*])))
  *current-state*)

(defun arrive-event-handler (main who place tense)
  (declare (ignore main))
  (when (eql tense '|present|)
    ;; if the place wasn't specified in the utterance then we use the most recently mentioned
    ;; place.
    (let ((vacinity (if (null place) *current-vacinity* (intern-object place 'vacinity)))
          (person (intern-object who 'person)))
      (Format t "~%Handling ~a arrives~@[ at ~a~] in state ~a" (role-name person) (if (null place) () vacinity) (state-name *current-state*))

      ;; The implication of saying he arrives is that he was mobile before the action
      (tell `[in-state [is-in-condition ,person mobile] ,*current-state*])
      (do-action 'arrive *current-state* 'before-arrive 'after-arrive person vacinity))))

(defun  do-unfreeze-handler (main actor object tense)
  (declare (ignore main))
  (when (eql tense '|present|)
    (let ((savior (intern-object actor 'person))
          (victim (intern-object object 'person))
          (vacinity (intern-object '|freeze_tile| 'vacinity)))
      (do-action 'unfreeze *current-state* 'before-unfreeze 'after-unfreeze savior victim vacinity))))

(defun resumes-handler (main resumed-action-clause person resumed-action worked-on-object tense)
  (declare (ignore main resumed-action-clause))
  (when (eql tense '|present|)
    (let ((person (intern-object person 'person))
          ;; note in this case it's always null
          (worked-on-object (when (not (eql worked-on-object '|null|)) (intern-object worked-on-object 'task))))
      (when (and (null worked-on-object) (eql resumed-action '|work|))
        ;; (break "~a ~a" resumed-action worked-on-object)
        (do-action 'works *current-state* 'before-resumes-working 'after-resumes-working person resumed-action)))))




;;; This code should be moved into Recipes Core
;;; And appropriate symbols exported

;;; A tester for this
(defmethod  get-handler-main-and-args ((parser core-parser-mixin) continuation)
  (get-mains-and-verb parser
                      #'(lambda (texp verb)
                          ;; (format t "~%doing ~a ~a" verb texp)
                          (ask* `[is-appropriate-response ,verb ,texp ?handler ?args]
                                (funcall continuation ?handler texp ?args))))
  (values))

(defun test-parse (parse) (get-handler-main-and-args parse #'print-all-args))
(defun print-all-args (handler main &rest stuff) (format t "~%Handler: ~a~%Main: ~a~% Args: ~{~a~^,~}" handler main stuff))

(eval-when (:compile-toplevel :load-toplevel)
(defvar *current-state* (intern-state 'initial))
(defvar *actors* nil)
(defvar *tools* nil)
(defvar *vacinities* nil)
(defvar *obstacles* nil)
(defvar *speaker-stack* nil)
(defvar *current-utterance-stack* nil)

(defvar *current-vacinity* nil))

(defun enact-storypar (story-name &key (initial-state *initial-state*) (clear-first t) (file nil))
  (when clear-first (clear) (setq initial-state *initial-state*))
  (let ((*current-state* initial-state)
        (*actors* nil)
        (*tools* nil)
        (*vacinities* nil)
        (*current-vacinity* nil)
        (*obstacles* nil)
        (*speaker-stack* nil)
        (*current-utterance-stack* nil))
    (labels ((handle-sentence (main verb)
               (ask* `[is-appropriate-response ,verb ,main ?handler ?args]
                     (setq *current-state* (apply ?handler main
                                                  (loop for arg in ?args collect (joshua-logic-variable-value arg)))))))
      (loop for utterance in (fetch-story story-name)
          for parse = (parse utterance)
          do (format t "~2% ~:a" utterance)
             (get-mains-and-verb parse #'handle-sentence)
             ;; We're requiring that each utterance be contained
             ;; within a single multi-main parser
             (pop *speaker-stack*)
             (pop *current-utterance-stack*))
      (if file
          (with-open-file (stream file :direction :output :if-does-not-exist :create :if-exists :supersede)
            (dump-story story-name *current-state* stream))
        (dump-story story-name *current-state*))
      (values *current-state* *actors* *tools* *vacinities* *obstacles*))))

(defmethod accumulate-roles ((thing t)) (values))
(defmethod accumulate-roles ((thing tool)) (pushnew thing *tools*) (values))
(defmethod accumulate-roles ((thing vacinity)) (pushnew thing *vacinities*) (values))
(defmethod accumulate-roles ((thing person)) (pushnew thing *actors*) (values))
(defmethod accumulate-roles ((thing obstacle)) (pushnew thing *obstacles*) (values))

(defmethod intern-object :around ((name symbol) (type symbol) &rest arglist)
  (let ((thing (apply #'call-next-method name type arglist)))
    (accumulate-roles thing)
    thing))

(defun dump-story (story-name final-state &optional (stream *standard-output*))
  (format stream "~2%(Story:  ~a" story-name)
  (format stream "~%(Actors: ~{~:a~^ ~})" (mapcar #'role-name *actors*))
  (format stream "~%(Tools: ~{~:a~^ ~})" (mapcar #'role-name *tools*))
  (format stream "~%(obstacles: ~{~:a~^ ~})" (mapcar #'role-name *obstacles*))
  (format stream "~%(vacinities: ~{~:a~^ ~})" (mapcar #'role-name *vacinities*))
  (format stream "~%(Actions: ~{~:a~^ ~})" (mapcar #'name (action-sequence final-state)))
  (terpri stream)
  (format stream "~%(Initial-state: ~%~{~a~^~%~}~%)" (mapcar #'predication-in-dump-format (predications-newly-in-state *initial-state*)))
  (loop for action in (action-sequence final-state)
      for prior-state = (prior-state action)
      for next-state = (next-state action)
      when (not (null (prior-action prior-state)))
      do (let ((new-predications (mapcar #'predication-in-dump-format (predications-newly-in-state prior-state))))
           (format stream "~2%(Action: ~a ~%Previous-State: ~a ~%Assertions:"
                   (name action) (state-name prior-state) )
           (format stream "~%~{~a~^~%~})" new-predications))
      do
        (let ((new-predications (mapcar #'predication-in-dump-format (predications-newly-in-state next-state))))
          (format stream "~2%(Action ~a: ~%Next-State ~a ~%Assertions:"
                  (name action) (state-name next-state))
          (format stream "~%~{~a~^~%~})" new-predications)))
  (format stream "~%(Depedencies")
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
             (typecase thing
               (predication (do-a-level (predication-statement thing)))
               (list (loop for thing in thing collect (do-a-level thing)))
               (state (state-name thing))
               (action (name thing))
               (symbol thing)
               (ji::basic-object (role-name thing)))))
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




#|
#1 Someone lands on a freeze tile and needs help from someone holding a MEDKIT to unfreeze them.


 Person2 arrives and unfreezes Person1 who continues working.


Person2, who holds a MEDKIT responds that she is on the way.
[Or from player point of view, any of:
I am on the way.
I'm coming.
Here I come.]


[Alternative: Person2 arrives and unfreezes Person1 who continues what he was doing.
  PLEASE NOTE: If both Person1 and Person2 can be masculine, then this sentence is ambiguous and START defaultly assumes that "he" refers to "Person2" (on the grounds that Person1 is the _subject_ of "arrive" and "unfreeze", Person2 is the _object_ of "unfreeze" and we are looking for the _subject_ of "do").]

#2 Someone discovers a critically injured victim and needs two other people who can help save the victim.

Person1 is searching for victims when she discovers a critical victim.

Person1 announces that she has discovered a critical victim and that she needs the help of two others in order to provide care.
[Or from player PoV:
I've discovered a critical victim and I need the help of two others in order to provide care.]

Person 2 announces that she are on the way.

Person 3 announces that his is on the way.

Later, all three people are next to the critical victim and one person, who is holding a MEDKIT, begins to provide care.

#3 Person1, who holds a hammer, sees a collection of rubble which might hide a victim.

Person1 sees a pile of rubble.
Person1 sees that it is possible that a victim is hidden in the pile.
Person1 sufficiently clears rubble to see if there is a victim.
If a victim is found, Person1 announces the presence of the victim.

Possible ending: 3a.  Person2, who carries a stretcher, announces that he will bring out the victim to the corridor to facilitate easier care to be provided by Person3, who holds a MEDKIT.

Possible ending: 3b.  Person2 who caries a MEDKIT reports that she will be there soon to provide care.
[Alternative: Person2 who caries a MEDKIT reports that she will be there soon to provide care.]

#4 Person1, whose tool has expired, announces that he is going back to get a new tool.

While Person1 uses his tool, Person1 discovers that his tool has expired.
[Alternatives:
While Person1 uses his hammer, Person1 discovers that his hammer has expired.
etc.]

Person1 announces that he is going back to the tool room to get a new tool.
Perhaps with the help of the others, Person1 decides what he should come back with.
Person1 returns to the tool room.
Person1 gets a tool.
Person1 returns to the building to continue ongoing clearing of the building.
|#
