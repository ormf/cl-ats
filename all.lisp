;;; -*- syntax: common-lisp; package: clm; base: 10; mode:lisp -*-
;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; File: all.cl
;;; ============
;;; Defines paths and files to load the system


;;; define some globals (edit before loading this file!)
(defparameter *ats-dir* "~/ATS-1.0/")
(defparameter *ats-src-dir* (concatenate 'string *ats-dir* "src/"))
(defparameter *ats-bin-dir* (concatenate 'string *ats-dir* "bin/"))
(defparameter *ats-synth-dir* (concatenate 'string *ats-dir* "synth/"))
(defparameter *ats-snd-dir* (concatenate 'string *ats-dir* "snd/"))

;;; compile and load function (adapted from Bill's in CLM)
(defun ats-cl (name &optional (dir *ats-src-dir*) (bindir *ats-bin-dir*))
  (let* ((cname (concatenate 'string dir name ".lisp"))
	 (cname-alias (concatenate 'string dir name ".cl"))
	 (lname #-allegro-cl-lite (concatenate 'string bindir name "." excl:*fasl-default-type*)
		#+allegro-cl-lite cname))
    (when (not (setf cname (or (probe-file cname-alias)(probe-file cname)))) 
      (setf cname (concatenate 'string dir name ".ins")))
    #-allegro-cl-lite
    (if (probe-file cname)
	(if (or (not (probe-file lname))
		(> (file-write-date (truename cname)) (file-write-date (truename lname))))
	        #-cltl2 (compile-file cname :output-file lname)
		    #+cltl2 (handler-bind ((excl:compiler-no-in-package-warning 
					    #'(lambda (c) (declare (ignore c)) (muffle-warning)))
					   (excl:compiler-undefined-functions-called-warning 
					    #'(lambda (c) (declare (ignore c)) (muffle-warning)))
					   (style-warning #'(lambda (c) (declare (ignore c)) (muffle-warning))))
					  (compile-file cname :output-file lname))))
    #-cltl2 (load lname)
    #+cltl2 (handler-bind ((simple-warning #'(lambda (c) (declare (ignore c)) (muffle-warning)))) (load lname))))

;;; Compile and load Lisp files
(loop for file in '("structure" 
		    "c-fun" 
		    "do-partials" 
		    "utilities" 
		    "get-value"
		    "copy-sound"  
		    "formants" 
		    "shift-sound" 
		    "stretch-sound" 
		    "trans-sound" 
		    "ana-fun" 
		    "windows" 
		    "residual" 
		    "peak-detection" 
		    "critical-bands" 
		    "peak-tracking" 
		    "tracker" 
		    "residual-analysis" 
		    "save-load-sound"
		    )
  do
  (ats-cl file))

;;; finally compile and load the CLM instruments
(loop for file in '(
		    "sin-synth"
		    "sin-noi-synth"
		    )
  do 
  (ats-cl file *ats-synth-dir* *ats-synth-dir*))
