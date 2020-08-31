(asdf:defsystem #:variatio
  :description "Generate variations on a motive"
  :author "Nuno Trocado"
  :license  "GNU Lesser Public License 3.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot #:cl-who)
  :components ((:file "package")
               (:file "application")))
