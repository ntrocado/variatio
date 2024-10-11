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
  (:import-from #:pitch-spelling rest-p)
  (:export metric-subdivision
	   min-subdivision-quantize
	   rhythm-spell))

(defpackage #:variatio
  (:use #:cl #:pitch-spelling #:rhythm-spelling)
  (:export note->ly-pitch
	   #:main))
