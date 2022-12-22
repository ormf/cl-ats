;;; -*- syntax: common-lisp; package: clm; base: 10; mode:lisp -*-
;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; File: ana-fun.cl
;;; ======================
;;; This file contains auxiliary functions
;;; for ATS's main analysis algorithms.

(defun compute-frames (total-samps hop st nd)
  "computes the number of frames in the analysis
we want to have an extra frame at the end to prevent
chopping the eneding"
  (labels ((get-next-frm (coarse-frames)
		       (if (> (+ (- (* coarse-frames hop) hop) st) nd) 
			   coarse-frames
			 (get-next-frm (incf coarse-frames)))))
    (get-next-frm (floor total-samps hop))))

;;; Window normalization
(defun normalize-window (window)
"normalizes the window by its mean value"
  (let* ((M (length window))
	 (norm (window-norm window)))
    (loop for i from 0 below M do
      (setf (aref window i) (double-float (* (aref window i) norm))))))


