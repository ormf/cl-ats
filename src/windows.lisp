;;; -*- syntax: common-lisp; package: clm; base: 10; mode:lisp -*-
;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; File: windows.cl
;;; ================
;;; This file contains the implementation
;;; of ATS's analysis windows


;;; All data coming form Harris' famous paper:
;;; "On the Use Of windows For Harmonic Analysis 
;;;  With The Discrete Fourier Transform"
;;; Proceedings of the IEEE, Vol. 66, No. 1 (pg. 51 to 84)
;;; January 1978

;;; Window coeffs. (a0, a1, a2, a3) 
(defparameter ats-blackman-window-coeffs 
  (make-array 6 :initial-contents '((0.42659 -0.49656 0.07685) ;;; Exact Blackman (-51 dB)
				    (0.42 -0.5 0.08) ;;; Blackman (rounded coeffs) (-58 dB)
				    (0.42323 -0.49755 0.07922) ;;; 3-term Bkackman-Harris 1 (-67 dB)
				    (0.44959 -0.49364 0.05677) ;;; 3-term Bkackman-Harris 2 (-61 dB)
				    (0.35875 -0.48829 0.14128 -0.01168) ;;; 4-term Bkackman-Harris 1 (-92 dB)
				    (0.40217 -0.49703 0.09392 -0.00183)))) ;;; 4-term Bkackman-Harris 2 (-71 dB)
				    

;;; Window creation:
;;; we generate a short-float array with the window values for each case.
(defun make-blackman-window (type M)
  (let* ((coeffs (cond ((equal type 'exact-blackman)
			(aref ats-blackman-window-coeffs 0))
		       ((equal type 'blackman)
			(aref ats-blackman-window-coeffs 1))
		       ((equal type 'blackman-harris-3-1)
			(aref ats-blackman-window-coeffs 2))
		       ((equal type 'blackman-harris-3-2)
			(aref ats-blackman-window-coeffs 3))
		       ((equal type 'blackman-harris-4-1)
			(aref ats-blackman-window-coeffs 4))
		       ((equal type 'blackman-harris-4-2)
			(aref ats-blackman-window-coeffs 5))
		       (T (error "make-blackman-window: unknown window type~%"))))
	 (two-pi-over-M (/ +two-pi+ M))
	 (four-pi-over-M (/ (* 2 +two-pi+) M))
	 (six-pi-over-M (/ (* 3 +two-pi+) M))
	 (a0 (first coeffs))
	 (a1 (second coeffs))
	 (a2 (third coeffs))
	 (a3 (fourth coeffs))
	 (win (make-double-float-array M :initial-element 0.0)))
    (loop for i from 0 below M do
      (setf (aref win i)
	    (double-float 
	     (+ a0 
		(* a1 (cos (* two-pi-over-M i))) 
		(* a2 (cos (* four-pi-over-M i)))
		(if a3 (* a3 (cos (* six-pi-over-M i)))
		  0.0)))))
    win))
	       
	 
;;; window tools
		      
(defun window-norm (window)
"returns the norm of a window"
  (let ((acc 0)
	(M (length window)))
    (loop for i from 0 below M do
      (incf acc (abs (aref window i))))
    (/ 2.0 acc)))



