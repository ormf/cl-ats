;;; -*- syntax: common-lisp; package: clm; base: 10; mode:lisp -*-
;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; File: structure.cl
;;; ==================
;;; this files contains the structure definitions
;;; for the main ATS data types

;;; Structure: ats-sound
;;; ====================
;;; main data abstraction
;;; amp, frq, and pha contain sinusoidal
;;; modeling information as arrays of
;;; arrays of data arranged by partial
;;; par-energy and band-energy hold
;;; noise modeling information (experimental format)
(def-clm-struct ats-sound  
  (name "new-sound")
  ;;; global sound info.
  (sampling-rate INTEGER)
  (frame-size INTEGER) 
  (window-size INTEGER)
  (partials INTEGER)
  (frames INTEGER)
  (bands array)
  ;;; Info. deduced from analysis
  (optimized NIL)
  (ampmax 0.0)
  (frqmax 0.0)
  (frq-av array) 
  (amp-av array) 
  (dur 0.0)
  ;;; Sinusoidal Data
  (time array array)
  (frq array array)
  (amp array array)
  (pha array array)
  ;;; Noise Data
  (energy array array) 
  (band-energy array array))

;;; Structure: ats-fft
;;; ==================
;;; abstraction used to handle all 
;;; fft data in a single variable		
(def-clm-struct ats-fft
  (size INTEGER)
  (rate 0.0)
  (fdr array)
  (fdi array))

;;; Structure: ats-peak
;;; ===================
;;; abstraction used to keep peak data used
;;; for peak detection and tracking
(def-clm-struct ats-peak
  (amp 0.0)
  (frq 0.0)
  (pha 0.0)
  (smr 0.0)
  (track INTEGER))


;;; Structure: ats-sieve
;;; ====================
;;; abstraction used for peak fitting
;;; by the sieve algorithm
(def-clm-struct ats-sieve
  (ctrfrq array)
  (limits array)
  (tracks array))

