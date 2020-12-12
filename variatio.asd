(asdf:defsystem #:variatio
  :description "Generate a pdf with automatic variations on a musical motive"
  :author "Nuno Trocado"
  :license  "GNU Lesser Public License 3.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:hunchentoot #:cl-who #:cl-ppcre #:parse-float)
  :components ((:file "package")
               (:file "heroku")
	       (:file "pitch-spelling")
	       (:file "rhythm-spelling")
	       (:file "operations")
	       (:file "variatio")))
