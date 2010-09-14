(require :asdf)
(asdf:load-system :asdf)
(asdf:load-system :cl-fad)
(asdf:load-system :lift)
(asdf:load-system :trivial-shell)
(asdf:load-system :osicat)
(asdf:load-system :anaphora)

(defpackage :get-dependencies (:use :cl :osicat :alexandria :lift :asdf :anaphora))

(in-package :get-dependencies)

(deftestsuite get-dependencies-tests () ())

(defun get-dependencies (dir dest lisp-libs &aux dirs)
; (setq dir "/media/WORK_PARTITION/dynserv/lisp-libs/user-libs/check-links/")
;  (setq asd-file #P"/media/WORK_PARTITION/dynserv/lisp-libs/user-libs/check-links/check-links.asd")
;  (setq dirs nil)
  
(extract-dependencies-asdf-systems :check-links
				   "/home/lispuser/tmp/this-system/deps/lisp-libs/")

  (do-all-deps-files (dep (list-directory (get-dependencies-dir dir)))
    (copy-any-type-file (get-real-pathname dep)
			"tmp/this-system/deps/libs/"))
  (add-deps-description "tmp/this-system/deps/")
  
  (alexandria:copy-file (create-tar.gz "tmp/this-system/deps") dest))



(defun extract-dependencies-asdf-systems (asdf-system directory)
  (dolist (asdf-dir (mapcar #'get-asdf-dir 
		       (get-asdf-all-deps asdf-system)))
    (copy-directory asdf-dir 
		    (merge-pathnames (last-dir-name asdf-dir) 
				     directory)
		    :ignore-dirs ".git"
		    :ignore-file-types ".fasl")) 
  t)
#|
(extract-dependencies-asdf-systems :check-links
				   "/home/lispuser/tmp/this-system/deps/lisp-libs/")
|#
(defun get-all-asd-files (dir &key follow-symlinks &aux result)
  (walk-directory-without-symlinks 
   (osicat:pathname-as-directory dir)
   #'(lambda (file)
       (if (string-equal (pathname-type file) "asd")
	   (push file result)))
   :follow-symlinks follow-symlinks)
  result)

#|
(defun get-all-deps-asd-files (asd-files)
  (labels ((run (asd-file &optional result)
	     (aif (get-asdf-deps asd-file)
		  (progn 
		    ;(push it result)
		    (setq result (append result (copy-list it)))
		    (print result)
		    (dolist (dep it)
		      ;(format t "~%asd-file: ~S~%dep: ~S~%" asd-file dep)
		      (run (get-asd-file-pathname dep) result)))
		  result)))
    (mapcar (compose #'get-asd-dir #'get-asd-file-pathname)
	    (remove-duplicates
	     (loop 
		for asd-file in asd-files	 
		for systems = (run asd-file)
		append systems)))))
(get-all-deps-asd-files (list asd-file))
|#

(defun get-asdf-deps (asdf-system)
  (rest (assoc 'load-op 
	       (component-depends-on 'load-op
				     (find-system asdf-system)))))
;(get-asdf-deps :check-links)

(defun get-asdf-all-deps (asdf-system &aux (hash (make-hash-table)))
  (labels ((walk-systems (sys)
	   (unless (gethash sys hash)
	     (setf (gethash sys hash) t)
	     (dolist (sys (get-asdf-deps sys))
	       (walk-systems sys)))))
    (walk-systems asdf-system)
    (alexandria:hash-table-keys hash)))

;(get-asdf-all-deps :check-links)

#|
  (labels ((for-asd-deps (asd-sys fn)
	     (do-all-deps-asd (asd (get-asd-file-pathname asd-sys))
	       (funcall fn asd)))
	     (do-asd-deps (get-asd-file-pathname asd)
  (when (get-asd-file-pathname
  (dolist (asd-file asd-files)
    (do-all-deps-asd (asd asd-file)      
      (push (get-asd-file-pathname asd) dirs)))
  (setq dirs (remove-duplicates dirs :test #'equal))
|#

;;;;;;;;;;;;;;;; ASDF utilities ;;;;;;;;;;;
(defun get-asdf-deps (asd-file)
  (getf (get-defsystem-form asd-file)
	:depends-on))

(defmacro do-all-deps-asd ((asd asd-file) &body body)
  `(dolist (,asd (get-asdf-deps ,asd-file))
    ,@body))

(defun get-defsystem-form (asd-file)
  (let ((*readtable* (copy-readtable)))
    (set-dispatch-macro-character #\# #\. #'(lambda (stream char arg)
					      (declare (ignore char arg))
					      (ignore-errors (read stream))))
    (with-open-file (stream asd-file) 
      (loop for form = (read stream nil)
	 while form
	 if (member (first form) '(defsystem asdf:defsystem))
	 do (return form)))))

#|
(Get-defsystem-form
"/media/WORK_PARTITION/work_dir/libs/current-lisp-libs/libs/asdf-install/site/hunchentoot-1.1.0/hunchentoot.asd")
|#

(defun get-asd-file-pathname (asdf-system)
  (asdf:system-source-file (asdf:find-system asdf-system)))

(defun get-asd-dir (asdf-system)
  (flet ((get-dir-from-pathname (pathname)
	   (make-pathname :defaults pathname
		 :name nil
		 :type nil)))
  (typecase asdf-system
    (pathname (get-dir-from-pathname asdf-system))
    (t (get-dir-from-pathname 
	(asdf:system-source-file (asdf:find-system asdf-system)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun walk-directory-without-symlinks (dir fn &key follow-symlinks)
  (labels ((walk-dir (from)
	     (dolist (pathname (osicat:list-directory from))
	       (cond
		 ((osicat:directory-pathname-p pathname)
		  (if (directory-symlink-p pathname)
		      (when follow-symlinks (walk-dir pathname))
		      (walk-dir pathname)))
		 ((not (osicat:directory-pathname-p pathname))
		  (funcall fn pathname))))))
    (walk-dir (pathname-as-directory dir))))




(defun get-real-pathname (pathname)
  (let ((file-or-dir (osicat:pathname-as-file pathname)))
    (case (osicat:file-kind file-or-dir)
      (:symbolic-link (read-link file-or-dir))
      (t file-or-dir))))

(defun create-archive-dependencies (dir &aux tmp-dir)
  (break "not implemented")
  (setq tmp-dir (get-dir-for-save))
  (ensure-directories-exist tmp-dir)
  (dolist (pathname (cl-fad:list-directory dir))
    (cond
      ((cl-fad:directory-pathname-p pathname)
       (cl-fad:copy-file "/home/lispuser/docs/" "/home/lispuser/docs-copy/")
      (t (print pathname))))))

(let ((count 0))
  (defun gen-tmp-dir ()
    (merge-pathnames (concatenate 'string  
				  "tmp-for-create-archive-deps-"
				  (princ-to-string (incf count))
				  "/"))))

(defun get-dir-for-save ()
  (merge-pathnames "dependencies/" (gen-tmp-dir)))

;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun copy-directory (from to &key ignore-dirs ignore-file-types follow-symlinks)
  (when ignore-dirs
    (setf ignore-dirs (as-list ignore-dirs)))
  (when ignore-file-types
    (setf ignore-file-types
	  (mapcar #'(lambda (file-type)
		      (declare (type string file-type))
		      (if (char= #\. (elt file-type 0))
			  (subseq file-type 1)
			  file-type))
		  (as-list ignore-file-types))))
  (labels ((copy-dir (from to)
	     (ensure-directories-exist to)
	     (dolist (pathname (osicat:list-directory from))
	       (cond
		 ((and (osicat:directory-pathname-p pathname)
		       (directory-symlink-p pathname))
		  (if follow-symlinks
		      (if-dir pathname to)
		      (if-file (osicat:pathname-as-file pathname) to)))
		 ((osicat:directory-pathname-p pathname)
		  (if-dir pathname to))
		 ((not (osicat:directory-pathname-p pathname))
		  (if-file pathname to)))))
	   (if-dir (pathname to)
	     (let ((last-dir (last-dir-name pathname)))
	       (unless (and ignore-dirs (member last-dir ignore-dirs :test #'string=))
		 (copy-dir pathname 
			   (pathname-as-directory (merge-pathnames last-dir to))))))
	   (if-file (pathname to)
	     (let* ((file-name (file-namestring pathname))
		    (file-type (pathname-type pathname))
		    (to (merge-pathnames file-name to)))
	       (unless (member file-type ignore-file-types :test #'string=)
		 (if follow-symlinks
		     (alexandria:copy-file pathname to)
		     (copy-any-type-file pathname to)))))) 
    (copy-dir (osicat:pathname-as-directory from)
	      (osicat:pathname-as-directory to))))

#|
(copy-directory "/lisp/libs/web/" "/lisp/libs/copy-web/" :ignore-dirs ".git")		 
|#

(defun as-list (obj)
  (if (atom obj)
      (list obj)
      obj))

(defun directory-symlink-p (pathname)
  (file-symlink-p (pathname-as-file pathname)))

(defun file-symlink-p (pathname)		  
  (eq :symbolic-link (file-kind pathname)))

(defun copy-any-type-file (from to)
  (case (file-kind from)
    (:symbolic-link (osicat:make-link to
				      :target (osicat:read-link from)
				      :hard nil))
    (:regular-file (alexandria:copy-file from to))
    (:directory (copy-directory from to))))

(defun last-dir-name (pathname)
  (first (last (pathname-directory pathname))))

#|

(defun file-size (pathname)
  (with-open-file (s pathname)
    (file-length s)))
|#
