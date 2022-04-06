;;; -*- Mode: Common-lisp; Package: Guide; Readtable: Joshua -*-

(in-package :guide)


;;; #2 Someone discovers a critically injured victim and needs two other people who can help save the victim.
(defstory save-critically-injured-victim
    :utterances ("Person1 is searching for victims when she discovers a critical victim."
                 "Person1 announces that she has discovered a critical victim and that she needs the help of two others in order to provide care"
                 ;; [Or from player PoV:
                 ;; I've discovered a critical victim and I need the help of two other people in order to provide care.]
                 "Person2 announces that she is on the way."
                 "Person3 announces that she is on the way."
                 "Later, all three people are next to the critical victim. One of them is holding a MEDKIT and begins to provide care."
                 ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Extractors
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; When clauses connect 2 other clauses, with implied sequencing
;;; between them.

;;; Probably any time an entity is extracted we should accumulate all of its modifiers as well.
;;; same for verbs.
;;; Another approach is to pass the objects on the semantic interpretation routines
;;; and let them extract the properties.

(defdecoder when
    :pattern [and [texp-match ?main :subject ?texp-1 :relation ?when :object ?texp-2]
                  [texp-match ?clausal-clause :subject ?when :relation-name |is_clausal| :object-name |yes|]
                  [texp-match ?texp-1 :relation-name ?texp-1-relation-name]
                  [is-appropriate-response ?texp-1-relation-name ?texp-1 ?texp-1-handler ?texp-1-args]
                  [texp-match ?texp-2 :relation-name ?texp-2-relation-name]
                  [is-appropriate-response ?texp-2-relation-name ?texp-2 ?texp-2-handler ?texp-2-args]
                  ]
    :return-args ((?texp-1-handler ?texp-1-args) (?texp-2-handler ?texp-2-args)))

;;; Begin clause connects an actor and a clause
(defdecoder begin
    :pattern [and [texp-match ?main :subject ?actor :object ?texp-2]
                  [texp-match ?texp-2 :relation-name ?texp-2-relation-name]
                  [is-appropriate-response ?texp-2-relation-name ?texp-2 ?texp-2-handler ?texp-2-args]
                  [all-properties ?actor ?actor-properties]
                  ]
    :return-args (?actor ?actor-properties (?texp-2-handler ?texp-2 ?texp-2-args)))

(defdecoder be
    :pattern [and [texp-match ?main :subject ?subject-object :relation-name ?relation-name]
                  [all-properties ?subject-object ?subject-properties |be|]
                  [all-syntactic-qualifiers ?main ?relation-properties]]
    :return-args (?subject-object ?subject-properties ?Relation-name ?relation-properties))

;;; Has purpose connects two clauses.  The first is an action and the second is the reason for it
;;; This just returns two lists which are the handler and args for the two clauses
(defdecoder has_purpose
    :pattern [and [texp-match ?main :subject ?subject :object ?object]
                  [texp-match ?subject :relation-name ?subject-verb]
                  [is-appropriate-response ?subject-verb ?subject ?subject-handler ?subject-args]
                  [texp-match ?object :relation-name ?object-verb]
                  [is-appropriate-response ?object-verb ?object ?object-handler ?object-args]]
    :return-args ((?subject-handler ?subject-args) (?object-handler ?object-args)))

(defdecoder announce
    :pattern [and [texp-match ?main :subject ?actor :object ?announcement]
                  [texp-match ?announcement :relation-name ?annoucement-verb]
                  [is-appropriate-response ?annoucement-verb ?announcement ?annoucement-handler ?annoucement-args]]
    :return-args (?actor (?annoucement-handler ?annoucement-args)))

(defdecoder search
    :pattern  [and [texp-match ?main :subject ?actor :object-name |null|]
                   [texp-match ?for-clause :subject ?main :relation-name |for| :object ?victim]]
    :return-args (?actor ?victim))

(defdecoder discover
    :pattern [and [texp-match ?main :subject ?actor-object :object ?victim-object]
                  [all-properties ?actor-object ?actor-properties]
                  [all-properties ?victim-object ?victim-properties]
                  [all-syntactic-qualifiers ?main ?syntactic-quaifiers]]
    :return-args (?actor-object ?actor-properties ?victim-object ?victim-properties ?syntactic-quaifiers))

(defdecoder need
    :pattern [and [texp-match ?main :subject ?actor-object :object ?whats-needed-object :object-name ?whats-needed-name]
                  [all-properties ?actor-object ?actor-properties |need|]
                  [all-properties ?whats-needed-object ?whats-needed-properties]]
    :return-args (?actor-object ?actor-properties ?whats-needed-name ?whats-needed-properties))

(defdecoder provide
    :pattern [and [texp-match ?main :subject ?actor-object :object ?victim-object :relation ?relation]
                  [all-properties ?actor-object ?actor-properties |provide|]
                  [all-properties ?victim-object ?victim-properties |provide|]
                  [all-syntactic-qualifiers ?main ?relation-properties]]
    :return-args (?actor-object ?actor-properties ?relation-properties ?victim-object ?victim-properties))

(defdecoder hold
    :pattern [and [texp-match ?main :subject ?actor-object :object ?thing-object]
                  [all-properties ?actor-object ?actor-properties]
                  [all-properties ?thing-object ?thing-properties]]
    :return-args ((?actor-object ?actor-properties) (?thing-object ?thing-properties)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Semantic handlers invoked by the above decoders
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; When connects two clauses, both of which are assumed to be initiated in the
;;; current (initial in this story) state

(defun when-handler (main clause-1 clause-2)
  ;; just interpret the two clauses
  ;; Maybe want to assert something about the states that result?
  (destructuring-bind (handler args) clause-1
    (apply handler main args))
  (destructuring-bind (handler args) clause-2
    (apply handler main args)))

(defun search-handler (main actor victim)
  (declare (ignore main))
  (let ((actor (intern-object (full-name actor) 'person))
        (victim (intern-object (full-name victim) 'person)))
    (tell `[in-state [is-searching-for ,actor ,victim] ,*current-state*])
    *current-state*))

(defun discover-handler (main actor actor-properties victim victim-properties syntactic-qualifiers)
  (declare (ignore main actor-properties))
  (let ((actor (intern-object (full-name actor) 'person))
        (victim (intern-object (full-name victim) 'person))
        (condition-of-victim (intern (string-upcase (string (second (assoc '|has_property| victim-properties))))))
        (perfective? (second (assoc '|is_perfective| syntactic-qualifiers))))
    (cond
     ((eql perfective? '|yes|)
      ;; if it's perfective (i.e. foo has discovered) then it's not saying a new action
      ;; is taking place but instead is referring to a previous state of the world in
      ;; which the actor has discovered the thing stated here.
      (ask `[in-state [has-discovered ,actor ,victim] ?state]
           #'(lambda (just)
               (return-from discover-handler (ask-database-predication just))))
      (break "WTF")
      )
     (t ;; here something new is really happening
        ;; perform the action and then update the current vacinity
      (let* ((before-state-name (make-name 'before-discover))
             (before-state (intern-state before-state-name *current-state*))
             (after-state-name (make-name 'after-discover))
             (after-state (intern-state after-state-name before-state))
             (next-state (do-action 'discover *current-state* before-state after-state actor victim))
             (victim-location nil)
             (actor-location nil)
             (current-location nil))
        (setq actor-location (get-entitys-vacinity actor))
        (setq victim-location (get-entitys-vacinity victim))
        (setq current-location (or actor-location victim-location (create-new-typical-vacinity)))
        (when (null actor-location)
          (tell `[in-state [is-in-vacinity ,actor ,current-location] ,next-state]))
        (when (null victim-location)
          (tell `[in-state [is-in-vacinity ,victim ,current-location] ,before-state]))
        (setq *current-vacinity* current-location)
        (when condition-of-victim
          (tell `[in-state [is-in-condition ,victim ,condition-of-victim] ,before-state]))
        next-state
        )))))



;;; To keep things simple here, I'm assuming that "announce" means that the thing said
;;; is in fact true.  Otherwise, every fact asserted would have to check the embedding-context stack
;;; to see if it's just being said or if it's in fact true.
(defun announce-handler (main annoucer announcement)
  (let* ((actor (intern-object (full-name annoucer) 'person))
         (before-state-name (make-name 'before-announce))
         (before-state (intern-state  before-state-name *current-state*))
         (after-state-name (make-name 'after-announce))
         (after-state (intern-state after-state-name before-state))
         )
    (push actor *speaker-stack*)
    (destructuring-bind (annoucement-handler args) announcement
      (multiple-value-bind (anouncement supplemental) (let ((*current-state* after-state)) (apply annoucement-handler (object main) args))
        (push anouncement *current-utterance-stack*)
        (push 'announcemnt *embedding-context*)
        (if (typep anouncement 'purpose)
            (do-action 'announce *current-state* before-state after-state actor supplemental)
          (do-action 'announce *current-state* before-state after-state actor anouncement))))))

;;; Has purpose just relates two statements that are in the subject and object position
(defun has_purpose-handler (main statement-1-description statement-2-description)
  (let ((statement-1 (apply (first statement-1-description) (subject main) (second statement-1-description)))
        (statement-2 (apply (first statement-2-description) (object main) (second statement-2-description))))
    (destructuring-bind (handler-1 &rest args-1) statement-1-description
      (declare (ignore args-1))
      (if (eql handler-1 'need-handler)
          (with-statement-destructured (inner state) statement-1
            (declare (ignore state))
            (with-statement-destructured (who-needs whats-needed) inner
              (declare (ignore who-needs))
              (values (tell `[purpose ,whats-needed ,statement-2]) inner)))
        (tell `[purpose ,statement-1 ,statement-2])))))

(defgeneric need-sub-handler (thing subject thing-properties))

(defun need-handler (main subject subject-properties object object-properties)
  (declare (ignore subject-properties main))
  (let ((person (intern-object (full-name subject) 'person)))
    (need-sub-handler object person object-properties)))

(defmethod need-sub-handler ((thing (eql '|help|)) subject thing-properties)
  (let* ((of-what (assoc '|related-to| thing-properties))
         (of-what-object (third of-what))
         (of-what-name (full-name of-what-object)))
    (when of-what-object
      (let* ((of-whats-properties (all-properties of-what-object))
             (proper? (eql (second (assoc '|is_proper| of-whats-properties)) '|yes|)))
        (when (not proper?)
          (let* ((quantity (second (assoc '|has_quantity| of-whats-properties)))
                 (collection (intern-object of-what-name 'collection :quantity (decode-quantity quantity) :member-type 'person))
                 (help-object (intern-object (make-name 'help) 'help :recipient subject :provider collection)))
            (tell `[in-state [needs ,subject , help-object] ,*current-state*])))))))

(defrule critical-implies-needs-care (:forward)
    if [and [in-state [is-in-condition ?who critical] ?state] :support ?F1]
    then (let ((its-new t))
           (ask* [in-state [needs ?who care] ?state] (setq its-new nil))
           (when its-new
             (let ((care-object (intern-object (make-name 'care) 'care :recipient ?who)))
               (tell `[in-state [needs ?who ,care-object] ?state] :justification `(critical-implies-needs-care (,?f1)))))))

(defun provide-handler (main actor actor-properties relation-properties whats-provided whats-provided-properties)
  (declare (ignore main whats-provided-properties))
  (let* ((actor (intern-object (full-name actor) 'person))
         (whats-provided-type (intern (string-upcase (name whats-provided))))
         ;; (whats-provided (intern-object (full-name whats-provided) whats-provided-type))
         (referent (is-anonymous-with-referent actor actor-properties)))
    (when (and referent (typep referent 'collection))
      ;; value-of doesn't interact well with forward rules in stateful preds
      (tell `[value-of (,referent typical-member) ,actor]))
    ;; Note: I'm assuming that the object's name here is its type
    (ask* `[in-state [needs ?recipient ?whats-needed] ,*current-state*]
          (when (typep ?whats-needed whats-provided-type)
            (return-from provide-handler
              (conditonally-tell `[in-state [provides ,actor ?whats-needed ?recipient] ,*current-state*]
                                 relation-properties))))))

(define-fwrd-stateful-rule provide-assertion-means-provide-action
    if [and [provides ?actor ?whats-provided ?recipient]
            [is-in-condition ?recipient |critical|]
            [is-in-vacinity ?recipient ?vacinity]
            ;; some other person is in the same vacinity
            [is-in-vacinity ?actor ?vacinity]
            [object-type-of ?actor person]
            (not (eql ?actor ?recipient))
            ;; and there's a group of in the vacinity
            [is-in-vacinity ?others ?vacinity]
            [object-type-of ?others collection]]
    then (when (and (>= (quantity ?others) 3)  (eql (member-of ?actor) ?others))
           ;; and the actor is a typical member of the group and the group has at least 3 people
           (do-action 'provide-care-to-critical-victim ?final-state 'before-provide-care 'after-provide-care ?actor ?recipient ?others ?vacinity)))



(defrule begun-means-do-it (:forward)
  if [and [in-state [has-begun ?actor ?predication] ?state] :support ?f1]
  then (tell ?predication :justification `(begun-means-enact (,?f1))))


;;; Be needs to sub-dispatch to a handler for whatever be is connected to
(defgeneric be-dispatcher (connective value value-object actor actor-properties verb-properties))

(defun be-handler (main actor actor-properties verb verb-properties)
  (declare (ignore main verb))
  (loop for (connective value value-object) in verb-properties
      when (not (member connective *syntactic-connectives*))
      return (be-dispatcher connective value value-object actor actor-properties verb-properties)))

(defmethod be-dispatcher ((be-what (eql '|on|)) (value (eql '|way|)) value-object actor actor-properties verb-properties)
  (declare (ignore actor-properties verb-properties value-object))
  (let ((actor (intern-object (Full-name actor) 'person))
        (current-location *current-vacinity*))
    (tell `[in-state [is-on-way ,actor ,current-location] ,*current-state*])))

;;; Note: Is this asserting a truth or testing it
(defmethod be-dispatcher ((be-what (eql '|next_to|)) value value-object actor actor-properties verb-properties)
  (declare (ignore verb-properties value))
  (let ((actor-quantity (decode-quantity (second (assoc '|has_quantity| actor-properties))))
        ;;; (actor-quantifier (second (assoc '|has_quantifier| actor-properties)))
        (victim (Intern-object (full-name value-object) 'person)))
    (let* ((vacinity (block foo (ask* `[in-state [is-in-vacinity ,victim ?vacinity] ,*current-state*]
                                      (return-from foo ?vacinity))))
           (people-in-vacinity nil)
           (justifying-preds nil))
      ;; Note: this should collect the predications
      ;; and use them to justify the tell below
      (ask `[in-state [is-in-vacinity ?who ,vacinity] ,*current-state*]
           #'(lambda (just)
               (when (not (eql ?who victim))
                 (push ?who people-in-vacinity)
                 (push (ask-database-predication just) justifying-preds))))
      (let ((collection (intern-object (full-name actor) 'collection :member-type 'person :quantity actor-quantity :members people-in-vacinity)))
        (tell `[in-state [is-in-vacinity ,collection ,vacinity] ,*current-state*]
              :justification `(collection-vacinity ,justifying-preds)))
      *current-state*)))

;;; Note: Maybe the correct thing is to go from announce plus he's not lying
;;; to he's actually doing what's announced and then from that to applying the action.
;;; But we're keeping it simple here.
(defrule on-way-means-move (:forward)
  if [in-state [has-announced ?speaker [in-state [is-on-way ?actor ?destination] ?state]] ?final-state]
  then (let* ((his-vacinity (get-or-create-entitys-vacinity ?actor ?final-state))
             (before-state-name (make-name 'before-move))
             (before-state (intern-state before-state-name ?final-state))
             (after-state-name (make-name 'after-move))
             (after-state (intern-state after-state-name before-state)))
         ;; This is necessary or else the planner will go off into never never land trying
         ;; to achieve the mobillity prereq of move-to for which it has no actions
         (assume `[in-state [is-in-condition ?actor mobile] ,before-state])
         (do-action 'move-to ?final-state before-state after-state ?actor his-vacinity ?destination)))


(defun begin-handler (main actor actor-args embedded-clause-args)
  (declare (ignore main actor-args))
  (destructuring-bind (handler sub-main args) embedded-clause-args
    (let ((result (apply handler sub-main args))
          (actor (intern-object (Full-name actor) 'person)))
      (tell `[in-state [has-begun ,actor ,result] ,*current-state*])
      *current-state*)))

(defrule typical-member-is-where-collection-is (:forward)
    if [and [in-state [is-in-vacinity ?collection ?vacinity] ?state]
            [object-type-of ?collection collection]
            ;; value-of doesn't seem to interact well with in-state in forward rules
            [value-of (?collection typical-member) ?person]]
    then [in-state [is-in-vacinity ?person ?vacinity] ?state])

(defun hold-handler (main actor-description victim-description)
  (declare (ignore main))
  (destructuring-bind (actor-object actor-properties) actor-description
    (declare (ignore actor-properties))
    (destructuring-bind (victim-object victim-properties) victim-description
      (declare (ignore victim-properties))
      (let ((actor (intern-object (full-name actor-object) 'person))
            (victim (intern-object (full-name victim-object) (Intern (string-upcase (name victim-object))))))
        (tell `[in-state [is-holding ,actor ,victim] ,*current-state*])
        *current-state*))))


;;; Notes:
;;; Need to keep track of the current vacinity.  In this case it's never identified (i.e. it's not something like the freeze tile) so we need
;;; to create a typical location for the place where the speaker discovers a critical victim and assert that both the speaker and the victim are there
;;; and that it's the current vacinity.
;;; Then when someone says that they're on the way, it's to the current vacinity.
;;;
;;; We need to keep track of the current speaker also for when we have things like announce
;;;
;;;
;;; All-properties returns triples of the relation-name and then the object-name and the actual object
;;; The name of foo+1 and foo+2 are both foo, but the full name of the object is foo-1 and foo-2.
;;; So we should be careful when extracting information to use the object's full-name.
;;;
;;; The Provide handler probably shouldn't just assert that  care is being provided when it's invoked within some enclosing context
;;; like begins to provide care or that it's the object of a purpose relationship.
;;; I need to figure out how to tell that context.
