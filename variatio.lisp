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

(defparameter *lilypond*
  #+windows "C:/Program Files (x86)/LilyPond/usr/bin/lilypond.exe"
  #+linux "/app/.apt/usr/bin/lilypond.real")

(defparameter *test-score*
  "\\score{
	{
		c'^~a
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
      (format stream *test-score* input)
      :close-stream
      (uiop:run-program (list *lilypond*
			      "-o"
			      output-filename
			      (namestring input-file))))
    (prog1
	(alexandria:read-file-into-byte-vector output-file)
      (uiop:delete-file-if-exists output-file))))
