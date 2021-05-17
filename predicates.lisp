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

