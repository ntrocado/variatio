(defpackage #:pitch-spelling
  (:use #:cl)
  (:export note
	   letter
	   accidental
	   octave
	   rest-p
	   best-spelling
	   best-spelling-split))

(defpackage #:variatio
  (:use #:cl #:pitch-spelling))
