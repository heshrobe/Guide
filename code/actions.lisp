;;; -*- Mode: Common-lisp; Package: Guide; Readtable: Joshua -*-

(in-package :guide)

(define-action move-around (?person ?vacinity)
  :typing ((?vacinity vacinity)
           (?person person))
  :prerequisites ([is-in-condition ?person mobile]
                  [is-in-vacinity ?person ?vacinity]
                  )
  :post-conditions ([is-moving-around ?person ?vacinity])
  )

(define-action move-to (?person ?current-vacinity ?destination)
  :typing ((?current-vacinity vacinity)
           (?destination vacinity)
           (?person person))
  :prerequisites ([is-in-vacinity ?person ?current-vacinity]
                  [is-in-condition ?person mobile])
  :post-conditions ([not [is-in-vacinity ?person ?current-vacinity]]
                    [is-in-vacinity ?person ?destination]))

(define-action take-to (?actor ?object ?current-vacinity ?destination)
  :typing ((?current-vacinity vacinity)
           (?destination vacinity)
           (?object person)
           (?actor person))
  :prerequisites ([is-in-vacinity ?actor ?current-vacinity]
                  [is-in-vacinity ?object ?current-vacinity]
                  [is-in-condition ?actor mobile])
  :post-conditions ([not [is-in-vacinity ?actor ?current-vacinity]]
                    [not [is-in-vacinity ?object ?current-vacinity]]
                    [has-taken-to ?actor ?object ?destination]
                    [is-in-vacinity ?actor ?destination]
                    [is-in-vacinity ?object ?destination]))

(define-fwrd-stateful-rule moving-means-not-on-freeze-tile
    :if [and [is-moving-around ?person ?someplace]
             [object-type-of ?person person]
             [object-type-of ?freeze-tile freeze-tile]]
  :then [not [is-standing-on ?person ?freeze-tile]])

(define-action step-on (?person ?place)
  :typing ((?place floor-tile)
           (?person person))
  :prerequisites ([is-in-condition ?person mobile]
                  [not [is-standing-on ?person ?place]]
                  )
  :post-conditions ([is-standing-on ?person ?place])
  )

(define-fwrd-stateful-rule standing-on-freeze-means-immobile
    :If [and [is-standing-on ?person ?floor-tile]
             [object-type-of ?person person]
             [object-type-of ?floor-type freeze-tile]]
    :then [not [is-in-condition ?person mobile]])

(define-fwrd-stateful-rule immobile-means-not-working
  :if [and [not [is-in-condition ?person mobile]]
          [object-type-of ?person person]]
  :then [not [is-working ?person]]
  )

(define-fwrd-stateful-rule immobile-implies-needing-help
  :if [and [not [is-in-condition ?person mobile]]
          [object-type-of ?person person]]
  :then [is-in-condition ?person needing-help])

(define-fwrd-stateful-rule standing-on-implies-is-in-vacinity
    :if [and [is-standing-on ?x ?y]
             [object-type-of ?x person]
             [object-type-of ?y vacinity]]
    :then [is-in-vacinity ?x ?y])

(define-action call-for-help (?person)
  :typing ((?person person))
  :prerequisites ([is-in-condition ?person needing-help])
  :post-conditions ([is-calling-for ?person help]))

(define-action states-own-condition (?person ?condition)
  :typing ((?person person))
  :prerequisites ([is-in-condition ?person ?condition])
  :post-conditions ([states-condition ?person ?condition])
  )

(define-action request-somebody-to-come (?person ?requestee)
  :typing ((?person person))
  :prerequisites ([is-calling-for ?person help])
  :post-conditions ([requests-someone-to-come ?person ?requestee])
  )

(define-action request-somebody-to-unfreeze (?person ?requestee)
  :typing ((?person person))
  :prerequisites ([is-calling-for ?person help])
  :post-conditions ([requests-someone-to-unfreeze ?person ?requestee]))


(define-action arrive (?person ?place)
  :typing ((?person person)
           (?place vacinity))
  :prerequisites ([is-in-condition ?person mobile])
  :post-conditions ([is-in-vacinity ?person ?place]))

(define-action unfreeze (?savior ?victim ?place)
  :typing ((?savior person)
           (?victim person))
  :prerequisites ([is-in-condition ?savior mobile]
                  [is-in-vacinity ?savior ?place]
                  [is-in-vacinity ?victim ?place]
                  [not [is-in-condition ?victim mobile]])
  :post-conditions ([is-in-condition ?victim mobile]))

(define-action works (?person)
  :typing ((?person person))
  :prerequisites ([is-in-condition ?person mobile])
  :post-conditions ([is-in-condition ?person working]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From Story-2
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-action discover (?person ?victim)
  :typing ((?person person)
           (?victim person))
  :prerequisites ([is-searching-for ?person ?])
  :post-conditions ([has-discovered ?person ?victim]))

(define-action announce (?person ?announcement)
  :typing ((?person person))
  :prerequisites ()
  :post-conditions ([has-announced ?person ?announcement]))


(define-action provide-care-to-critical-victim (?actor ?victim ?helpers ?vacinity)
  :typing ((?actor person)
           (?victim person)
           (?helpers collection)
           (?vacinity vacinity))
  :bindings ([is-in-vacinity ?victim ?vacinity])
  :prerequisites (;; [value-of (?helpers member-type) person]
                  ;; [value-of (?actor member-of) ?helpers]
                  ;; these aren't stateful but action compiler makes them so
                  ;; make sure that check one prerequisite handles lisp calls correctly
                  (>= (quantity ?helpers) 3)
                  (eql (typical-member ?helpers) ?actor)
                  [is-in-vacinity ?helpers ?vacinity]
                  [is-in-vacinity ?actor ?vacinity]
                  ;; MEDKIT?
                  )
  :post-conditions ([has-provided-care ?actor ?victim]))



;;; from Study-3
;;; Story-1

(define-action scout-out (?actor ?location)
  :typing ((?actor person)
           (?location vacinity))
  :prerequisites ([free-to-scout ?actor])
  :post-conditions ([scouted ?actor ?location]))

(define-action mark (?actor ?location)
  :typing ((?actor person)
           (?location vacinity))
  :prerequisites ([free-to-scout ?actor])
  :post-conditions ([marked ?actor ?location]))

;;; from story-2

(define-action mutually-share-information (?collection)
  :typing ((?collection collection))
  :prerequisites ([value-of (?collection member-type) player])
  :post-conditions ([has-shared-information ?collection])
  )

(define-action go-to-care-giving-site (?actors ?place)
  :typing ((?actors collection)
           (?place vacinity))
  :prerequisites ([value-of (?actors member-type) player]
                  [can-be-given-at ?place care]
                  [has-shared-information ?actors])
  :post-conditions ([is-in-vacinity ?actors ?place])
  )

(define-action give-care (?giver ?victim)
  :typing ((?giver medical-specialist)
           (?victim victim))
  :bindings ([is-in-vacinity ?victim ?victim-place])
  :prerequisites ([is-in-vacinity ?giver ?victim-place])
  :post-conditions ([is-providing-care ?giver ?victim]))


#|

;;; a test case for the macro expander

(define-action foo (?actors ?place)
  :typing ((?actors collection)
           (?place vacinity))
  :prerequisites ([value-of ?actors.member-type player]
                  [can-be-given-at ?actors.member-type care]
                  [has-shared-information ?actors])
  :post-conditions ([is-in-vacinity ?actors ?place])
  )

|#
