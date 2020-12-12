(defpackage #:pitch-spelling
  (:use #:cl)
  (:export note
	   letter
	   accidental
	   octave
	   rest-p
	   pitch-spell
	   pitch-spell-split))

(defpackage #:rhythm-spelling
  (:use #:cl)
  (:export rhythm-spell))

(defpackage #:variatio
  (:use #:cl #:pitch-spelling #:rhythm-spelling))
