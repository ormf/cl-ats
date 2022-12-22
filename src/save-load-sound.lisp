;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; save-load-sound.cl
;;;
;;; Functions for saving and loading ATS sounds in binary format

(in-package :cl-ats)

(defparameter *ats-header-size* 10)
(defparameter *ats-magic-number* (double 123.0))

#|

-ATS header consists of (all double floats):

*ats-magic-number*
sampling-rate (samples/sec)
frame-size (samples)
window-size (samples)
partials (number)
frames (number)
ampmax (max. amplitude)
frqmax (max. frequecny)
dur (duration)
type (number, see below)

-ATS frames can be of four different types:

1. without phase or noise:
==========================
time (frame starting time)
amp (par#0 amplitude)
frq (par#0 frequency)
...
amp (par#n amplitude)
frq (par#n frequency)


2. with phase but not noise:
============================
time (frame starting time)
amp (par#0 amplitude)
frq (par#0 frequency)
pha (par#0 phase)
...
amp (par#n amplitude)
frq (par#n frequency)
pha (par#n phase)


3. with noise but not phase:
============================
time (frame starting time)
amp (par#0 amplitude)
frq (par#0 frequency)
...
amp (par#n amplitude)
frq (par#n frequency)

energy (band#0 energy)
...
energy (band#n energy)

4. with phase and noise:
========================
time (frame starting time)
amp (par#0 amplitude)
frq (par#0 frequency)
pha (par#0 phase)
...
amp (par#n amplitude)
frq (par#n frequency)
pha (par#n phase)

noise (band#0 energy)
...
noise (band#n energy)

|#
 
(defun ats-save (sound file &key (save-phase T)(save-noise T))
"
saves <sound> into <file> in binary format
<file> must be string with a file name
in case the file already exists it gets overwritten
"
  ;;; check first if file already exists
  ;;; in that case erase
  (if (probe-file file)(delete-file file))
  ;;; now open output file and create data holders
  (let* ((fd (c-create-file file))
	 (sr (double (ats-sound-sampling-rate sound)))
	 (frame-size (double (ats-sound-frame-size sound)))
	 (window-size (double (ats-sound-window-size sound)))
	 (partials (double (ats-sound-partials sound)))
	 (frames (double (ats-sound-frames sound)))
	 (max-frq (double (ats-sound-frqmax sound)))
	 (max-amp (double (ats-sound-ampmax sound)))
	 (dur (double (ats-sound-dur sound)))
	 (has-pha (if (and save-phase (ats-sound-pha sound)) T NIL))
	 (has-noi (if (and save-noise (or (ats-sound-energy sound)(ats-sound-band-energy sound)))
		      T NIL))
	 (type (double 
		(cond ((and has-pha has-noi)
		       4)
		      ((and (not has-pha) has-noi)
		       3)
		      ((and  has-pha (not has-noi))
		       2)
		      (T 
		       1))))
	 (time-arr (make-double-float-array 1 :initial-element 0.0))
	 (header-arr (make-double-float-array *ats-header-size* :initial-element 0.0))
	 (hop-arr (make-double-float-array (ats-sound-frames sound) :initial-element 0.0))
	 (band-l (if has-noi (coerce (ats-sound-bands sound) 'list)))
	 (data-arr)
	 (noi-arr))
    ;;; Header:
    ;;;;[mag, sr, fs, ws, #par, #frm, MaxAmp, MaxFrq, dur, type]
    ;;; Simple mag word for now
    ;;; would read 123.0 if read with the correct byte order
    (format t "Mag: ~A SR: ~A FS: ~A WS: ~A Partials: ~A Frames: ~A  MaxAmp: ~A MaxFrq: ~A Dur: ~A Type: ~A~%"
	    (setf (aref header-arr 0) *ats-magic-number*)
	    (setf (aref header-arr 1) sr)
	    (setf (aref header-arr 2) frame-size)
	    (setf (aref header-arr 3) window-size)
 	    (setf (aref header-arr 4) partials)
 	    (setf (aref header-arr 5) frames)
	    (setf (aref header-arr 6) max-amp)
	    (setf (aref header-arr 7) max-frq)
	    (setf (aref header-arr 8) dur)
	    (setf (aref header-arr 9) type))
    ;;; create array for data 
    (setf data-arr (make-double-float-array (if has-pha 3 2) :initial-element 0.0))
    ;;; create array for noise data (25 critical bands)
    (if has-noi (setf noi-arr (make-double-float-array *ats-critical-bands* :initial-element 0.0)))
    ;;; Store all times in array.
    ;;; For now we consider all partials 
    ;;; have the same time structure 
    ;;; (need a different file format for multirate)
    (loop for h from 0 below frames do
      (setf (aref hop-arr h) (aref (aref (ats-sound-time sound) 0) h)))
    (format t "Saving sound...~%")
    ;;; now write data out
    (loop for i from 0 below frames do
       ;;; Write header
      (if (= i 0)(clm-write-floats fd header-arr *ats-header-size*))
       ;;; Write time
      (setf (aref time-arr 0) (aref hop-arr i))
       ;;; (clm-print "~A " (aref time-arr 0))
      (clm-write-floats fd time-arr 1)
       ;;; Now loop for all partials and write blocks
       ;;; of [par#, amp, frq, pha]
      (loop for j from 0 below partials do
	 ;;; Fill buffer up
	(setf (aref data-arr 0) (aref (aref (ats-sound-amp sound) j) i))
	(setf (aref data-arr 1) (aref (aref (ats-sound-frq sound) j) i))
	(if has-pha
	    (setf (aref data-arr 2) 
		  (aref (aref (ats-sound-pha sound) j) i)))
	 ;;; Write buffer
	(clm-write-floats fd data-arr(if has-pha 3 2)))
      ;;; Noise part
      (when has-noi
	;;; NOTE: 
	;;; for now critical band energy is stored as an array
	;;; of <frames> arrays of 25 elements each
	(loop for k from 0 below *ats-critical-bands* do
	  (if (and band-l (member k band-l))
	      (setf (aref noi-arr k)(double (aref (aref (ats-sound-band-energy sound)(position k band-l)) i)))
	    (setf (aref noi-arr k)(double (energy-to-band sound k i)))))
	(clm-write-floats fd noi-arr *ats-critical-bands*)))
    (c-close fd)))

(defun ats-load (file sound &key (dist-energy T))
  "
loads an ATS sound from <file>
<sound> is a symbol that will point to the new sound
" 
  ;;; check if file exists
  (if (not (probe-file file)) (error "File ~s does not exist!" file))
  (let* ((fd (c-open-input-file file))
	 (snd (set sound (make-ats-sound :name (string sound))))
	 (mag 0.0)
	 (sr 0.0)
	 (frame-size 0.0)
	 (window-size 0.0)
	 (partials 0.0)
	 (frames 0.0)
	 (max-amp 0.0)
	 (max-frq 0.0)
	 (dur 0.0)
	 (type 0.0)
	 (has-pha)
	 (has-noi)
	 (noi-arr)
	 (data-arr)
	 (header-arr (make-double-float-array *ats-header-size* :initial-element 0.0))
	 (time-arr (make-double-float-array 1 :initial-element 0.0)))
    ;;; Read header information
    (clm-read-floats fd header-arr *ats-header-size*)
    ;;; Check magic number
    (if (/= (aref header-arr 0) *ats-magic-number*)
	(error "Not a proper ATS file (may be byte swapped)~%"))
    ;;; Retrieve data from header
    (setf mag (aref header-arr 0)
	  sr (floor (aref header-arr 1))
	  frame-size (floor (aref header-arr 2))
	  window-size (floor (aref header-arr 3))
	  partials (floor (aref header-arr 4))
	  frames (floor (aref header-arr 5))
	  max-amp (aref header-arr 6)
	  max-frq (aref header-arr 7)
	  dur (aref header-arr 8)
	  type (floor (aref header-arr 9)))
    ;;; Print Header out
    (format t "mag: ~S sr: ~S fs: ~S ws: ~S partials: ~S frames: ~S MaxAmp: ~S MaxFrq: ~S Dur: ~S Type: ~S~%"
	    mag sr frame-size window-size partials frames max-amp max-frq dur type)
    ;;; set booleans
    (setf has-pha (if (evenp type) T NIL))
    (setf has-noi (if (> type 2) T NIL))
    ;;; create array for data
    (setf data-arr (make-double-float-array (if has-pha 3 2) :initial-element 0.0))
    ;;; create array for noise part if necessary
    (if has-noi (setf noi-arr (make-double-float-array *ats-critical-bands* :initial-element 0.0)))
    (format t "Loading sound...~%")
    (init-sound snd 
		:sampling-rate sr
		:frame-size frame-size
		:window-size window-size
		:frames frames
		:partials partials
		:duration dur
		:has-phase has-pha
		:has-noise has-noi)
    (setf (ats-sound-ampmax snd) max-amp)
    (setf (ats-sound-frqmax snd) max-frq)
    (loop for i from 0 below frames do
       ;;; Read frame's time
      (clm-read-floats fd time-arr 1)
       ;;; Now loop reading each partial's data
      (loop for j from 0 below partials do
            ;;; set time
	(setf (aref (aref (ats-sound-time snd) j) i)
	      (aref time-arr 0))
	    ;;; read data
	(clm-read-floats fd data-arr (if has-pha 3 2))
	      ;;; set amp
	(setf (aref (aref (ats-sound-amp snd) j) i)
	      (aref data-arr 0))
	   ;;; set frq
	(setf (aref (aref (ats-sound-frq snd) j) i)
	      (aref data-arr 1))
	   ;;; set pha
	(if has-pha
	    (setf (aref (aref (ats-sound-pha snd) j) i)
		  (aref data-arr 2))))
;      (format t "[time: ~A amp: ~A frq: ~A pha: ~A] " 
;      (aref time-arr 0)(aref data-arr 0)(aref data-arr 1)(aref data-arr 2))
	  ;;; finally set noise
      (when has-noi
	(clm-read-floats fd noi-arr *ats-critical-bands*)
	(loop for k from 0 below *ats-critical-bands* do
	  (setf (aref (aref (ats-sound-band-energy snd) k) i)(aref noi-arr k)))))
    ;;; optimize sound
    (optimize-sound snd :verbose t :get-max-values nil :fill-gaps nil :trim nil :simplify nil)
    ;;; distribute energy in partials in case we are asked to
    (when (and has-noi dist-energy)
      (format t "Transferring noise energy to partials...~%")
      (band-to-energy snd)
      (remove-bands snd))
    ;;; registrer new sound
    (pushnew (ats-sound-name snd) *ats-sounds* :test #'equal)
    (c-close fd)))

#|
;;; saving a sound in binary format 
(ats-save cl "/zap/cl.ats")

;;; checking if it was correctly saved
(ats-load "/zap/cl.ats" 'cl-new)


|#

