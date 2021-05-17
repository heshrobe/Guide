;;; -*- Mode: Common-lisp; Package: Guide; Readtable: Joshua -*-

(in-package :guide)

(define-recipe-object-type tile
    )

(define-recipe-object-type vacinity
    )

(define-recipe-object-type floor-tile
    :super-types (tile vacinity)
    )

(define-recipe-object-type obstacle
    )

(define-recipe-object-type freeze-tile
    :super-types (obstacle floor-tile)
    )

(define-recipe-object-type person
    )

(define-recipe-object-type tool
    )

(define-recipe-object-type medkit
    :super-types (tool))

(define-recipe-object-type building
    :super-types (vacinity))

    