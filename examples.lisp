(in-package :cl-ats)

;;; Analysis

(simplify-sound)



;;; cl
(tracker "clarinet.aif"
	 'cl
	 :start 0.0
	 :hop-size 1/4
	 :lowest-frequency 100.0
	 :highest-frequency 20000.0
	 :frequency-deviation 0.05
	 :lowest-magnitude (db-amp -70)
	 :SMR-continuity 0.7
	 :track-length 6
	 :min-segment-length 3
	 :residual "/tmp/cl-res.snd"
	 :verbose nil
	 :debug nil)

(tracker "clarinet.aif"
	 'cl
	 :start 0.0
	 :hop-size 1/4
	 :lowest-frequency 100.0
	 :highest-frequency 20000.0
	 :frequency-deviation 0.05
	 :lowest-magnitude (db-amp -70)
	 :SMR-continuity 0.7
	 :track-length 6
	 :min-segment-length 3
	 :residual nil
	 :verbose nil
	 :debug nil)

;;; crt-cs6
(tracker "crt-cs6.snd" 
	 'crt-cs6
	 :start 0.1
	 :lowest-frequency 500.0
	 :highest-frequency 20000.0
	 :frequency-deviation 0.15
	 :window-cycles 4
	 :window-type 'blackman-harris-4-1
	 :hop-size 1/8
	 :lowest-magnitude (db-amp -90)
	 :amp-threshold -80
	 :track-length 6
	 :min-segment-length 3
	 :last-peak-contribution 0.5
	 :SMR-continuity 0.3
	 :residual "/tmp/crt-cs6-res.snd"
	 :verbose nil
	 :debug nil
	 :optimize t)

;;; Synthesis


;;; cl
;;; plain resynthesis (sines only)

(defun cp-cuda-ats-snd (cuda-ats-sound)
    (make-ats-sound
     :name (ats-cuda::ats-sound-name cuda-ats-sound)
     :sampling-rate (ats-cuda::ats-sound-sampling-rate cuda-ats-sound)
     :frame-size (ats-cuda::ats-sound-frame-size cuda-ats-sound)
     :window-size (ats-cuda::ats-sound-window-size cuda-ats-sound)
     :partials (ats-cuda::ats-sound-partials cuda-ats-sound)
     :frames (ats-cuda::ats-sound-frames cuda-ats-sound)
     :bands (ats-cuda::ats-sound-bands cuda-ats-sound)
     :optimized (ats-cuda::ats-sound-optimized cuda-ats-sound)
     :ampmax (ats-cuda::ats-sound-ampmax cuda-ats-sound)
     :frqmax (ats-cuda::ats-sound-frqmax cuda-ats-sound)
     :frq-av (ats-cuda::ats-sound-frq-av cuda-ats-sound)
     :amp-av (ats-cuda::ats-sound-amp-av cuda-ats-sound)
     :dur (ats-cuda::ats-sound-dur cuda-ats-sound)
     :time (ats-cuda::ats-sound-time cuda-ats-sound)
     :frq (ats-cuda::ats-sound-frq cuda-ats-sound)
     :amp (ats-cuda::ats-sound-amp cuda-ats-sound)
     :pha (ats-cuda::ats-sound-pha cuda-ats-sound)
     :energy (ats-cuda::ats-sound-energy cuda-ats-sound)
     :band-energy (ats-cuda::ats-sound-band-energy cuda-ats-sound)))


(time
 (with-sound (:play nil :output "/tmp/cl-1.snd" :srate 44100
	      :statistics t :verbose t)
   (sin-synth 0.0 (cp-cuda-ats-snd ats-cuda::cl))))

;;; plain resynthesis (sines plus noise)
(time
 (with-sound (:play nil :output "/tmp/cl-2b.snd" :srate 44100
	      :statistics t :verbose t)
   (sin-noi-synth 0.0 (cp-cuda-ats-snd ats-cuda::cl) :time-ptr '(0 0 1 1))))

(with-sound (:play nil :output "/tmp/cl-2.snd" :srate 44100
	     :statistics t :verbose t)
  (sin-noi-synth 0.0 cl :time-ptr '(0 0 1 1)))

;;; plain resynthesis (noise only)
(with-sound (:play nil :output "/tmp/cl-3.snd" :srate 44100
		   :statistics t :verbose t)
  (sin-noi-synth 0.0 cl :time-ptr '(0 0 1 1) :noise-only t))

;;; using time pointer to modify the attack
(with-sound (:play nil :output "/tmp/cl-4.snd" :srate 44100
		   :statistics t :verbose t)
  (sin-noi-synth 0.0 cl :time-ptr '(0.0 0.0 0.5 0.1 0.7 0.7 1.0 1.0)))


;;; play backwards and gradually adding noise
(with-sound (:play nil :output "/tmp/cl-5.snd" :srate 44100
		   :statistics t :verbose t)
  (sin-noi-synth 0.0 cl 
		 :time-ptr '(0.0 1.0 0.9 0.3 1.0 0.0)
		 :noise-env '(0.0 0.0 0.9 1.0 1.0 1.0)
		 :amp-env '(0 0 0.1 0 0.9 1 1 1)))


;;; crt-cs6
;;; plain resynthesis (sines only)
(with-sound (:play nil :output "/tmp/crt-cs6-1.snd" :srate 44100
		   :statistics t :verbose t)
  (sin-synth 0.0 crt-cs6))

;;; plain resynthesis (sines plus noise)
(with-sound (:play nil :output "/tmp/crt-cs6-2.snd" :srate 44100
		   :statistics t :verbose t)
  (sin-noi-synth 0.0 crt-cs6 :time-ptr '(0 0 1 1)))

;;; plain resynthesis (noise only)
(with-sound (:play nil :output "/tmp/crt-cs6-3.snd" :srate 44100
		   :statistics t :verbose t)
  (sin-noi-synth 0.0 crt-cs6 :time-ptr '(0 0 1 1) :noise-only t))

;;; transpose up an octave and expand four times keeping attack
;;; use only partials' noise
(with-sound (:play nil :output "/tmp/crt-cs6-4.snd" :srate 44100
		   :statistics t :verbose t)
  (sin-noi-synth 0.0 crt-cs6 
		 :frq-scale 2
		 :time-ptr '(0.0 0.0 0.025 0.1 0.5 0.5 1.0 1.0)
		 :duration (* (ats-sound-dur crt-cs6) 4)))

;;; saving and loading

;;; saving sound 
(ats-save cl "/tmp/cl.ats")

;;; loading sound
(ats-load "/tmp/cl.ats" 'cl-new)

;;; saving sound without phase  
(ats-save crt-cs6 "/tmp/crt-cs6.ats" :save-phase nil)

;;; loading sound
(ats-load "/tmp/crt-cs6.ats" 'crt-cs6--new)

;;; cl-new
