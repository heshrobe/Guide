;;; -*- Syntax: Ansi-common-lisp; Package: cl-USER; Base: 10; Mode: LISP -*- 

(in-package :cl-user)

;;; Make swank aware of the right readtable
#+swank
(pushnew (cons "GUIDE" ji::*joshua-readtable*)
	 swank:*readtable-alist*
	 :key #'first
	 :test #'string=)

(defvar *guide-home-directory* :not-yet)
(defvar *guide-wild-directory* :not-yet)

(eval-when (:execute :load-toplevel)
  (let* ((loading-file *load-truename*)
         (host (pathname-host loading-file))
         (device (pathname-device loading-file))
         (home-dir (pathname-directory loading-file))
         (wild-dir (append (butlast home-dir) (list :wild-inferiors))))
    (setq *guide-home-directory* (make-pathname :directory home-dir
                                                :host host 
                                                :device device)
          *guide-wild-directory* (make-pathname :directory wild-dir
                                                :host host 
                                                :device device
                                                :type :wild
                                                :name :wild
                                                :version :unspecific))
    (setf (logical-pathname-translations "guide")
      `(("home;*.*"	,*guide-home-directory*)
        ("code;*.*"     ,*guide-home-directory*)
        ("**;*.*"	,*guide-wild-directory*)
        ))
    (with-open-file (F #P"guide:home;my-logical-pathnames.lisp" :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format f "~%;;; guide")
      (format f "~2%~s" "guide")
      (loop for (a b) in (logical-pathname-translations "guide")
          do (format f "~%'(~s ~s)" (namestring a) (namestring b)))
      (terpri f)
      ))
  )

;;; I guess I could include start-interface and recipes in the defsystem and
;;; a load file for their defsystems

#+asdf
(asdf:defsystem guide
  :name "Guide"
  :description "Story Parsing and Interpretation Guide"
  :depends-on (:planning-interface :joshua)
  :maintainer "Howie Shrobe"
  :pathname "."
  :serial t
  :components
  ((:file "package-definition")
   (:joshua-file "objects")
   (:joshua-file "preliminaries")
   (:joshua-file "predicates")
   (:joshua-file "actions")
   (:joshua-file "extractors")
   (:joshua-file "study-3-stories")
   ))
