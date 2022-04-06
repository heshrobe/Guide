;;; -*- Mode: Common-lisp; Package: Guide; Readtable: Joshua -*-

(in-package :guide)

(define-planning-predicate is-in-condition (person consition) ())

(define-planning-predicate is-in-vacinity (person vacinity) ())

(define-planning-predicate is-moving-around (person vacinity) ())

(define-planning-predicate is-standing-on (person thing) ())

(define-planning-predicate is-working (person) ())

(define-planning-predicate is-calling-for (person what) ())

(define-planning-predicate states-condition (person state) ())

(define-planning-predicate requests-someone-to-come (person requestee) ())

(define-planning-predicate requests-someone-to-unfreeze (person requestee) ())

(define-planning-predicate is-in-possession-of (agent object) ())

(define-planning-predicate is-searching-for (person victims) ())

(define-planning-predicate has-discovered (actor victim) ())

(define-planning-predicate has-announced (person predication) ())

(define-planning-predicate has-begun (person predication) ())

;;; The things needed is an object like help or clear
(define-planning-predicate needs (actor thing-needed &rest other-stuff) ())

(define-planning-predicate purpose (thing1 thing2) ())

(define-planning-predicate helps (person thing) ())

(define-planning-predicate provides (actor thing recipient) ())

(define-planning-predicate is-on-way (actor destination) ())

(define-planning-predicate is-holding (actor thing) ())

(define-planning-predicate has-provided-care (actor victim) ())



;;; from study 3

(define-planning-predicate have (subject object) ())
(define-planning-predicate there-is (variable-list predicate) ())
(define-planning-predicate for-all (variable-list predicate) ())
(define-planning-predicate implies (premise conclusion) ())
(define-planning-predicate instance-of (thing type) ())
(define-planning-predicate free-to-scout (person) ())
(define-planning-predicate scouted (person place) ())
(define-planning-predicate marked (person place) ())
(define-planning-predicate has-shared-information (collection) ())
(define-planning-predicate contains (thing stuff) ())
(define-planning-predicate can-be-given-at (thing place) ())
(define-planning-predicate is-providing-care (actor victim) ())
(define-planning-predicate saved (victim) ())
(define-planning-predicate realize (who what) ())
(define-planning-predicate not-realize (who what) ())
(define-planning-predicate wasted (what) ())
(define-planning-predicate has-placed (who what where) ())
;; for example "know the name of the teacher"
(define-planning-predicate know-referent (who reference) ())
(define-planning-predicate colocated-at (who place) ())
(define-planning-predicate has-taken-to (actor object location) ())
(define-planning-predicate incorrect-area (evacuation-area victim) ())
