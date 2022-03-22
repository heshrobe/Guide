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
        (let* ((quantifier (is-quantified actor))
               (actor-properties (all-properties actor))
               (quantity (decode-quantity (second (assoc '|has_quantity| actor-properties)))))
          (cond
           ((and quantity (eql quantifier '|all|) (eql object '|information|))
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
         (who-name (start::convert-start-string-to-lisp-atom (name who)))
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
                 (modality (start::convert-start-string-to-lisp-atom (second (assoc '|has_modal| contained-stuff-relative-clause-properties)))))
            (ask* `[texp-match ,contained-stuff-relative-clause :subject-name ?what :relation-name ?what-verb :object-name ?what-object]
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
                      (tell `[in-state [can-be-given-at ,the-place ,(start::convert-start-string-to-lisp-atom ?what)] initial]))
                    (let* ((before-state (intern-state 'before-go-to-care-giving-place *current-state*))
                           (after-state (intern-state 'after-go-to-care-giving-place before-state)))
                      (do-action 'go-to-care-giving-site *current-state* before-state after-state who-referent the-place)
                      (return-from go-to-handler after-state))))))))
