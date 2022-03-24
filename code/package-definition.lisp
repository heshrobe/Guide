;;; -*- Mode: Common-lisp; Package: common-lisp-user -*-


(defpackage guide
  (:use recipes start joshua common-lisp)
  (:shadow room)
  (:import-from ltms assume)
  (:shadowing-import-from recipes object-type-of value-of))