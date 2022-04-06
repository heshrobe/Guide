;;; -*- Mode: Common-lisp; Package: common-lisp-user -*-


(defpackage guide
  (:use planning-core start joshua common-lisp)
  (:shadowing-import-from planning-core part-of value-of object-type-of named-part-of)
  (:shadow room)
  (:import-from ltms assume))
