;;;; cl-ats.asd

(asdf:defsystem #:cl-ats
  :description "ATS file parser for Common Lisp
  and port of Juan Pampin's ats lisp code."

  :author "Orm Finnendahl"
  :license  "Public Domain"
  :version "0.0.1"
  :serial t
  :depends-on (#:clm)
  :components ((:file "package")
               (:module "src"
                :serial t
                :components
                ((:file "structure")
                 (:file "c-fun")
                 (:file "do-partials")
                 (:file "utilities")
                 (:file "get-value")
                 (:file "copy-sound")
                 (:file "formants")
                 (:file "shift-sound")
                 (:file "stretch-sound")
                 (:file "trans-sound")
                 (:file "ana-fun")
                 (:file "windows")
                 (:file "residual")
                 (:file "peak-detection")
                 (:file "critical-bands")
                 (:file "peak-tracking")
                 (:file "tracker")
                 (:file "residual-analysis")
                 (:file "save-load-sound")))
               (:module "synth"
                :serial t
                :components
                ((:file "sin-synth")
                 (:file "sin-noi-synth")))
               (:file "cl-ats" :depends-on ("package"))))
