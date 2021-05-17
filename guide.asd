;;; -*- Syntax: Ansi-common-lisp; Package: cl-USER; Base: 10; Mode: LISP -*- 

(in-package :cl-user)

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
  :depends-on (:Recipes/core)
  :maintainer "Howie Shrobe"
  :pathname "."
  :components
  ((:file "package-definition")
   (:joshua-file "objects" :depends-on ("package-definition"))
   (:joshua-file "predicates" :depends-on ("objects"))
   (:joshua-file "actions" :depends-on ("predicates"))
   (:joshua-file "story-understanding" :depends-on ("actions"))
   ))
