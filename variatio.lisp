(in-package :variatio)

(defun char-pitch->value (pitch)
  "Parse the char PITCH, returning 0 for C, 2 for D, upto 11 for B."
  (case pitch
    (#\c 0)
    (#\d 2)
    (#\e 4)
    (#\f 5)
    (#\g 7)
    (#\a 9)
    (#\b 11)))

(defun text-accidental->value (accidental)
  "Parse the string ACCIDENTAL, returning the value to add or substract to the midi note."
  (alexandria:switch (accidental :test 'string=)
    ("ss" 2)
    ("s+" 1.5)
    ("s" 1)
    ("+" .5)
    ("-" -.5)
    ("b" -1)
    ("b-" -1.5)
    ("bb" -2)
    (t 0)))

(defun text-octave->value (octave &optional (default 4))
  (* (if octave
	 (+ default (- (count #\' octave)
		       (count #\, octave)))
	 default)
     12))

(defun dots (dur n)
  (if (zerop n) dur
      ;; (dots (+ dur (/ 1 (expt 2 n))) (1- n))
      (dots (+ dur (/ dur 2)) (1- n))))

(defun parse-input (input)
  "Parse text in INPUT format into a list of midi note values and a list of durations as fractions/multiples of a beat. Octaves are relative to the first one."
  (loop :with midi
	:with durations
	:for note :in (ppcre:split "\\s" input)
	;;; TODO Error handling
	:do (ppcre:register-groups-bind (pitch accidental octave (#'parse-integer dur) dot)
		("^([cdefgabr])(ss|s\\+|s|\\+|-|b|b-|bb)?('+|,+)?(\\d+)?(\\.+)?$" note)
	      (push (+ (char-pitch->value (character pitch))
		       (text-accidental->value accidental)
		       (text-octave->value octave (truncate (/ (or (first midi) 60) 12))))
		    midi)
	      (push (if dur
			(dots (/ 4 dur) (length dot))
			(or (first durations) 1))
		    durations))
	:finally (return (values (reverse midi) (reverse durations)))))

(defun midi->ly-pitch (note)
  (let* ((letter (case (mod (floor note) 12)
		   (0 "c")
		   (1 "cis")
		   (2 "d")
		   (3 "dis")
		   (4 "e")
		   (5 "f")
		   (6 "fis")
		   (7 "g")
		   (8 "gis")
		   (9 "a")
		   (10 "ais")
		   (11 "b")))
	 (octave-n (1- (floor (/ note 12))))
	 (octave-char (cond 
			((> octave-n 3) #\')
			((< octave-n 3) #\,)))
	 (octave (if octave-char
		     (make-string (abs (- 3 octave-n)) :initial-element octave-char)
		     ""))
	 (quarter-tone-up (when (plusp (mod note 1)) "ih")))
    (format nil "~{~a~}" (list letter (if quarter-tone-up quarter-tone-up "") octave))))

(defmethod note->ly-pitch ((note pitch-spelling:note))
  (format nil "~a~a~a"
	  (pitch-spelling:letter note)
	  (case (pitch-spelling:accidental note)
	    (:double-flat "eses")
	    (:flat "es")
	    (:natural "")
	    (:sharp "is")
	    (:double-sharp "isis"))
	  (let ((oct (pitch-spelling:octave note)))
	    (make-string (abs (- 4 oct))
			 :initial-element (if (> oct 4)
					      #\'
					      #\,)))))

(defun make-ly-note (pitch dur)
  (assert (> dur 1/32)
	  (dur)
	  "Duration ~S is shorter than a 128th note. Lilypond only allows such values as beamed notes." dur)
  (let* ((int-part (if (>= dur 1)
		       (loop :for i := 1 :then (* i 2)
			     :while (<= i dur)
			     :maximize i)
		       (loop :for i := 1 :then (/ i 2)
			     :minimize i
			     :until (<= i dur))))
	 (frac-part (- dur int-part)))
    (concatenate 'string 
		 (note->ly-pitch pitch)
		 (cond ((= int-part 8) "\\longa")
		       ((= int-part 16) "\\breve")
		       (t (format nil "~a" (/ 4 int-part))))
		 (cond ((= frac-part (/ int-part 2)) ".")
		       ((plusp frac-part) (concatenate 'string
						       "~ "
						       (make-ly-note pitch frac-part)))))))

(defun make-ly (pitches durations)
  "Take a list of PITCHES in midi note values and a list of DURATIONS and return a text string in Lilypond format."
  (with-output-to-string (out)
    (mapc (lambda (pitch duration)
	    (format out "~a " (make-ly-note pitch duration)))
	  (pitch-spelling:best-spelling pitches)
	  durations)))

(defparameter *lilypond*
  #+windows "C:/Program Files (x86)/LilyPond/usr/bin/lilypond.exe"
  #+linux "/app/.apt/usr/bin/lilypond.real")

(defparameter *test-score*
  "\\score{
	{
		~a
	}

	\\layout{}
	\\midi{}
}")

(hunchentoot:define-easy-handler (root :uri "/") (input)
  (setf (hunchentoot:content-type*) "application/pdf")
  (let* ((output-filename #+linux "/app/output"
			  #+windows "C:/Users/trocado/Desktop/output")
	 (output-file (make-pathname :type "pdf" :defaults output-filename)))
    (uiop:with-temporary-file (:stream stream :pathname input-file)
      ;; TODO criar ficheiro lilypond
      (format stream *test-score* (apply #'make-ly (multiple-value-list (parse-input input))))
      :close-stream
      (uiop:run-program (list *lilypond*
			      "-o"
			      output-filename
			      (namestring input-file))))
    (prog1
	(alexandria:read-file-into-byte-vector output-file)
      (uiop:delete-file-if-exists output-file))))
