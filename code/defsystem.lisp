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
      )
    (pushnew (namestring (truename #P"guide:home;my-logical-pathnames.lisp"))
             (logical-pathname-translations-database-pathnames)
             :test #'string-equal))
  )


#+allegro
(defsystem guide
    (:default-pathname "guide:code;"
        :default-module-class separate-destination-module)
  (:serial
   ("package-definition")
   ("objects" (:module-class separate-destination-joshua-module))
   ("preliminaries" (:module-class separate-destination-joshua-module))
   ("predicates" (:module-class separate-destination-joshua-module))
   ("actions" (:module-class separate-destination-joshua-module))
   ("extractors" (:module-class separate-destination-joshua-module))
   ("study-3-stories" (:module-class separate-destination-joshua-module))
   ;; ("story-1-processing" (:module-class separate-destination-joshua-module))
   ;; ("story-2-processing" (:module-class separate-destination-joshua-module))
   ))
