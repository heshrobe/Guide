;;; -*- Mode: Common-lisp; Package: Guide; Readtable: Joshua -*-

(in-package :guide)

(define-recipe-predicate is-in-condition (person consition) ())

(define-recipe-predicate is-in-vacinity (person vacinity) ())

(define-recipe-predicate is-moving-around (person vacinity) ())

(define-recipe-predicate is-standing-on (person thing) ())

(define-recipe-predicate is-working (person) ())

(define-recipe-predicate is-calling-for (person what) ())

(define-recipe-predicate states-condition (person state) ())

(define-recipe-predicate requests-someone-to-come (person requestee) ())

(define-recipe-predicate requests-someone-to-unfreeze (person requestee) ())

(define-recipe-predicate is-in-possession-of (agent object) ())

(define-recipe-predicate is-searching-for (person victims) ())

(define-recipe-predicate has-discovered (actor victim) ())

(define-recipe-predicate has-announced (person predication) ())

(define-recipe-predicate has-begun (person predication) ())

;;; The things needed is an object like help or clear
(define-recipe-predicate needs (actor thing-needed &rest other-stuff) ())

(define-recipe-predicate purpose (thing1 thing2) ())

(define-recipe-predicate helps (person thing) ())

(define-recipe-predicate provides (actor thing recipient) ())

(Define-recipe-predicate is-on-way (actor destination) ())

(define-recipe-predicate is-holding (actor thing) ())

(define-recipe-predicate has-provided-care (actor victim) ())



;;; from study 3

(define-recipe-predicate have (subject object) ())
(define-recipe-predicate there-is (variable-list predicate) ())
(define-recipe-predicate for-all (variable-list predicate) ())
(define-recipe-predicate implies (premise conclusion) ())
(define-recipe-predicate instance-of (thing type) ())
(define-recipe-predicate free-to-scout (person) ())
(define-recipe-predicate scouted (person place) ())
(define-recipe-predicate marked (person place) ())
(define-recipe-predicate has-shared-information (collection) ())
(define-recipe-predicate contains (thing stuff) ())
(define-recipe-predicate can-be-given-at (thing place) ())
