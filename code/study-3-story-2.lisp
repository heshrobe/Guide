;;; -*- Mode: Common-lisp; Package: Guide; Readtable: Joshua -*-

(in-package :guide)

(defstory study-3-2
    "At the start of the game all three participants shared their unique information with the others."
  "This enabled the team to go to an area that contained many victims that can be given care."
  )


;;; I suppose that this could try to decide whether it is
;;; sharnig information or something tanglible and call different
;;; handlers if so.  But I'm punting that to the handler
(defdecoder share
    :pattern [texp-match ?main :subject ?subject :object-name ?object]
    :return-args (?subject ?object)
    )

(defhandler share (actor object)
  (ask* `[texp-match ?clause :subject ,main :object ?with-whom :relation-name |with|]
        (let* ((actor-properties (all-properties actor))
               (quantifier (has-quantification actor actor-properties))
               (quantity (decode-quantity (has-quantity actor actor-properties))))
          (cond
           ((and quantity (eql quantifier 'all) (eql object '|information|))
            (let* ((players (loop for i from 1 upto quantity
                            collect (intern-object (intern (format nil "player-~d" i)) 'person)))
                   (collection (intern-object 'participants 'collection))
                   (before-state (intern-state 'before-share-information *current-state*))
                   (after-state (intern-state 'after-share-information before-state)))
              (loop for player in players do (tell `[value-of (,collection members) ,player]))
              (tell `[value-of (,collection quantity) ,quantity])
              (tell `[value-of (,collection member-type) player])
              (do-action 'mutually-share-information *current-state* before-state after-state collection)
              (return-from share-handler after-state)
              ))))))

;;; note that there are many other possible enables sentences
;;; really should figure out what it's saying rather than hard coding
;;; for go to
(defdecoder enable
    :pattern [and ;; this enable team1
              [texp-match ?main :object ?subject]
              ;; <enable> to <action>
              [texp-match ? :subject ?main :relation-name |to| :object ?action-clause]
              ;; destructure action-clause, mae sure the verb is "go" and bind the subject
              [texp-match ?action-clause :subject ?subject :relation-name |go|]
              ;; make sure it's go to and get where
              [texp-match ? :subject ?action-clause :relation-name |to| :object ?place]
              ]
    :handler-name go-to-handler
    :return-args (?subject ?place))

;;; There are lots of other branches that a go-to type verb might take, but I'm cutting corners
;;; Also resolving the referent the-team is hokey
(defhandler go-to (who where)
  (declare (ignore main))
  (let* ((where-qualifier (has-relative-clause where))
         (who-name (convert-start-string-to-lisp-atom (name who)))
         (who-referent nil))
    (when (member who-name '(they them team group))
      (ask* `[in-state [object-type-of ?collection collection] initial]
            (ask* `[in-state [value-of (?collection member-type) player] initial]
                  (setq who-referent ?collection))))
    (ask* `[texp-match ,where-qualifier :relation-name |contain| :object ?contained-stuff]
          (let* ((contained-stuff-properties (all-properties ?contained-stuff))
                 (quantity-of-contained-stuff (second (assoc '|has_quantity| contained-stuff-properties)))
                 (contained-stuff-relative-clause (has-relative-clause ?contained-stuff))
                 (contained-stuff-relative-clause-properties (all-properties contained-stuff-relative-clause))
                 (modality (convert-start-string-to-lisp-atom (second (assoc '|has_modal| contained-stuff-relative-clause-properties)))))
            (ask* `[texp-match ,contained-relative-clause :subject-name ?what :relation-name ?what-verb :object-name ?what-object]
                  ;; if we've gotten here then we know who is going where
                  ;; what is contained at the place
                  ;; how many of them there are
                  ;; and what qualifies them
                  ;; so now tell that in the original state there is such a place
                  (let* ((the-place (intern-object (full-name where) 'vacinity))
                         (type-of-contained-stuff (find-singular-of-noun (name ?contained-stuff)))
                         (the-contained-stuff (intern-object (make-name (name ?contained-stuff)) 'collection
                                                             :quantity quantity-of-contained-stuff :member-type type-of-contained-stuff)))
                    (tell `[in-state [contains ,the-place ,the-contained-stuff] initial])
                    (when (eql modality 'can)
                      (tell `[in-state [can-be-given-at ,the-place ,(convert-start-string-to-lisp-atom ?what)] initial]))
                    (let* ((before-state (intern-state 'before-go-to-care-giving-place *current-state*))
                           (after-state (intern-state 'after-go-to-care-giving-place before-state)))
                      (do-action 'go-to-care-giving-site *current-state* before-state after-state who-referent the-place)
                      (return-from go-to-handler after-state))))))))



;;; Study 3,, type 2 stories

;;; some observations: These stories state facts about the state of the world and facts about beliefs
;;; They also contain descriptions of actions taken.
;;; They also make reference to background knowledge that is probably best represented as forward chainging rules.
;;; These comments also should be applied to the previous story where we have a specialized go-to-care-giving-site action
;;; rather than a go-to action and a rule that would triger off the post-conditions.

;;; #1
(defstory study-3-2-1
    "The medical specialist began to give care to a victim."
  "The medical specialist stopped prematurely."
  "The victim was not saved and did not turn green."
  "The specialist must continue to apply the medical kit until the victim turns green."
  "Unfortunately, the medical specialist's efforts will not result in points even if the transport specialist moves the victim to the evacuation area.")

;;;
(defstory study-3-2-2
    "The medical specialist began to give care to a critical victim"
  "he did not realize that two people are needed to save a critical victim."
  "Unfortunately, the victim cannot be saved by a single specialist"
  "the specialist's effort will be wasted.")

;;; The fact that two people are required can be represented as an universally quantified implication
;;; [for-all (?victim) [implies [and [value-of (?victim treatment-team) ?team] [value-of (?team quantity) ?number] (< ?number 2)]
;;;                             [not [saved ?victim]]]
;;; But it is made active through the rule below

;;; This says that if a single medical specialist is treating a patient
;;; then the patient won't be saved.
(define-fwrd-stateful-rule cant-treat-alone
    :if [and [object-type-of ?victim victim]
             [value-of (?victim treatment-team) ?team]
             [object-type-of ?team treatment-team]
             [value-of (?team members) ?medical-specialist]
             [value-of (?team quantity) 1]
             [is-providing-care ?team ?victim]]
    :then [not [saved ?victim]])

(define-fwrd-stateful-rule cant-treat-alone-2
  :if [and [object-type-of ?victim victim]
           [value-of (?victim treatment-team) ?team]
           [object-type-of ?team treatment-team]
           [value-of (?team members) ?medical-specialist]
           [value-of (?team quantity) 1]
           [is-providing-care ?team ?victim]]
  :then [wasted (effort ?medical-specialist)])

;;; The realize predicate is pretty hokey
;;; But this says that if there's a team treating a victim
;;; and the member of the team doesn't realize that you need
;;; 2 people to treat a patient, then there's only one person in the team
(define-fwrd-stateful-rule doesnt-realize-need-means-didnt-do-it
    :if [and [object-type-of ?team treatment-team]
             [value-of (?team members) ?medical-specialist]
             [is-providing-care ?team ?victim]
             [not-realize ?medical-specialist (needs (at-least 2 people) (save ?victim))]]
    :then [value-of (?team quantity) 1])



;;; I put in features that allow the decoder to determine the appropriate handler
;;; as well as the texp to serve as the main for that handler
;;; It's up to the decoder to decide whether to also pass the original main
;;; or some information gathered from it to the handler
(defdecoder begin
    :new-main ?new-main
    :handler-name ?begin-what
    :return-args (?subject)
    :pattern [and [texp-match ?main :subject ?subject :object ?new-main]
                  [texp-match ?new-main :relation-name ?verb]
                  (unify ?begin-what (smash "-" ?verb "handler"))])

(defhandler give (giver)
  (let ((giver-properties (all-properties giver)))
    (ask* `[texp-match ,main :subject ,giver :object-name ?what-given]
          (ask* `[texp-match ? :subject ,main :relation-name |to| :object ?receiver]
                (cond
                 ((eql ?what-given '|care|)
                  ;; this is the special case we care about giving care
                  ;; In a more complete coverage the give handler might dispatch off to
                  ;; different handlers for different word senses (e.g. give care, give a tinkers dam, give a present, give way)
                  (let* ((singular (eql 'singular (has-number giver giver-properties)))
                         (is-definite (eql (has-determiner giver giver-properties) 'definite))
                         (giver-type (short-name giver))
                         (interned-giver (intern-object (full-name giver) giver-type))
                         (receiver-type (short-name ?receiver))
                         (interned-receiver (intern-object (full-name ?receiver) receiver-type))
                         (locale (intern-object 'victim-vacinity 'vacinity))
                         )
                    (when (and singular is-definite)
                      (let* ((the-team (intern-object 'the-treatment-team 'treatment-team))
                             (before-state (intern-state 'before-give-care  *current-state*))
                             (after-state (intern-state 'after-give-care before-state)))
                        (tell `[in-state [value-of (,the-team members) ,interned-giver] ,before-state])
                        (tell `[in-state [is-in-vacinity ,interned-receiver ,locale] ,before-state])
                        (tell `[in-state [is-in-vacinity ,the-team ,locale] ,before-state])
                        (tell `[in-state [value-of (,interned-receiver treatment-team) ,the-team] ,before-state])
                        (do-action 'give-care *current-state* before-state after-state the-team interned-receiver)
                        (return-from give-handler after-state))))))))))

(defdecoder realize
    :handler-name ?the-right-handler
    :new-main ?what-was-realized
    :pattern [and (unify ?the-right-handler
                         (if (clause-is-negated ?main)
                             'did-not-realize-handler
                           'did-realie-handler))
                  [texp-match ?main :subject ?subject :object ?what-was-realized]]
    :return-args (?subject))

(defhandler did-not-realize (who)
  ;; very special case of realize that people are needed to ...
  (ask* `[texp-match ,main :subject ?what-is-needed :relation-name |is| :object-name |needed|]
        (ask* `[texp-match ? :subject ,main :relation-name |to| :object ?do-what]
              (ask* `[texp-match ?do-what :relation-name ?relation-name :object ?the-final-object]
                    (let* ((how-many (decode-quantity (has-quantity ?what-is-needed)))
                           (non-realizer (intern-object (full-name who) (short-name who)))
                           (object (intern-object (full-name ?the-final-object) (short-name ?the-final-object))))
                      (tell `[in-state [not-realize ,non-realizer (needs (at-least ,how-many ,(short-name ?what-is-needed))
                                                                         (,(short-name ?relation-name) ,object))]
                            ,*current-state*])))))
  *current-state*)


#|

This is a test case that shows that when there's an object-type-of
predicate in the if part, it justifies the result with not just their
object-type-of predicate but also the named-part-of predicate tying the object
to its parent, even when the parent is *root*.  After examining the Joshua
code I don't see any reason why that's the case.  I commented out the line in
the rete.lisp code that did that and it doesn't seem to cause any problem.
In case it really was needed for some reason I don't remember, then the fix is
not to include recipes::named-part-of predicates when the parent is *root*
when dumping out the output trace in enact-story

(define-recipe-predicate foo (a b) ())
(define-recipe-predicate bar (a b c) ())
(define-recipe-object-type thingy
    )

(define-recipe-object-type thingy-holder
  :parts ((piece thingy)))

(defrule test (:forward)
  :if [and [object-type-of ?x thingy]
           [foo ?a ?b]]
  :then [bar ?x ?a ?b])

|#
