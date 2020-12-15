(in-package :variatio)

;;; INPUT

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

(defparameter *input-regex*
  "^([cdefgabr])(ss|s\\+|s|\\+|-|b|b-|bb)?('+|,+)?(\\d*\\.?\\d*)?$")

(defun parse-to-note (pitch accidental octave previous-notes)
  (if (char= pitch #\r)
      'rest
      (make-instance
       'note
       :letter pitch
       :accidental (alexandria:switch (accidental :test 'string=)
				      ("bb" :double-flat)
				      ("b" :flat)
				      ("s" :sharp)
				      ("ss" :double-sharp)
				      (t :natural))
       :octave (/ (text-octave->value octave
				      (octave (or (find-if (lambda (x)
							     (eql (type-of x) 'note))
							   previous-notes)
						  (make-instance 'note))))
		  12))))

(defun parse-input (input &key (return-notes nil))
  "Parse text in INPUT format into a list of midi note values and a list of durations as fractions/multiples of a beat. Octaves are relative to the first one."
  (assert (stringp input) (input) "INPUT must be a string. ~a was provided" input)
  (loop :with midi
	:with durations
	:for note :in (ppcre:split "\\s" input)
;;; TODO Error handling
	:do (ppcre:register-groups-bind (pitch accidental octave dur)
		(*input-regex* note)
	      (push (alexandria:when-let (ch (character pitch))
		      (if return-notes
			  (parse-to-note ch accidental octave midi)
			  (if (char= ch #\r)
			      'rest
			      (+ (char-pitch->value ch)
				 (text-accidental->value accidental)
				 (text-octave->value octave
						     (truncate (/ (or (when (numberp (first midi))
									(first midi))
								      60)
								  12)))))))
		    midi)
	      (push (or (parse-float:parse-float dur :junk-allowed t)
			(or (first durations) 1))
		    durations))
	:finally (return (values (reverse midi) (reverse durations)))))

(defun original-phrase (input)
  (multiple-value-bind (pitches durations)
      (parse-input input :return-notes t)
    (multiple-value-call #'rhythm-spell (final-rest pitches durations))))

;;; PROCESS

(defun process (pitches durations)
  (let ((op
	  (alexandria:random-elt
	   (list (list #'reverse-pitches
		       (list pitches durations))
		 (list #'reverse-rhythms
		       (list pitches durations))
		 (list #'approaches
		       (list pitches durations
			     (loop :repeat (max 1 (random (ceiling (/ (length pitches)
								      3))))
				   :collect (- (random 10) 5))))
		 (list #'interval-fill (list pitches durations))
		 (list #'remove-notes (list pitches durations
					    (+ .1 (random .5))))
		 (list #'insert-rests (list pitches durations
					    (+ .1 (random .4))))
		 (list #'insert-pedal (list pitches durations
					    (+ .2 (random .5))
					    (alexandria:random-elt '(:top :mean :bottom))))
		 (list #'rotate (list pitches durations))
		 (list #'augment (list pitches durations (if (> (random 1.0) .5)
							     2
							     (alexandria:random-elt '(1.5 3)))))
		 (list #'rhythm-flatten (list pitches durations))
		 (list #'rhythm-raise-floor (list pitches durations))
		 (list #'interval-shift (list pitches durations .4 (1+ (random 13))))
		 (list #'octave-shift (list pitches durations .3))
		 (list #'bookend-mean (list pitches durations))
		 (list #'bookend-over-ambitus (list pitches durations))
		 (list #'mv-expand-compress (list pitches durations (+ .1 (random 2.4))))))))
    (apply (first op) (second op))))

(defun process-n (pitches durations n)
  (if (zerop n)
      (values pitches durations)
      (multiple-value-bind (new-pitches new-durations)
	  (process pitches durations)
	(if (some #'numberp new-pitches) ; try again if there's only rests
	    (process-n new-pitches new-durations (1- n))
	    (process-n pitches durations n)))))

(defun trim (pitches durations &optional (max-len 23))
  "Remove random notes/rests if there are more than MAX-LEN of them."
  (flet ((remove-nth-element (l n)
	   (remove-if #'identity l :start n :count 1)))
    (if (<= (length pitches) max-len)
	(values pitches durations)
	(let ((rand-elt (random (length pitches))))
	  (trim (remove-nth-element pitches rand-elt)
		(remove-nth-element durations rand-elt))))))

(defun fix-very-short-durations (pitches durations)
  (if (find-if (lambda (x) (< x 1/32)) durations)
      (fix-very-short-durations pitches (mapcar (lambda (x) (* x 2)) durations))
      (values pitches durations)))

;;; OUTPUT

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

(defgeneric note->ly-pitch (note)
    (:documentation "Generates a lilypond string for a given pitch."))

(defmethod note->ly-pitch ((note note))
  (format nil "~a~a~a"
	  (letter note)
	  (case (accidental note)
	    (:double-flat "eses")
	    (:flat "es")
	    (:natural "")
	    (:sharp "is")
	    (:double-sharp "isis"))
	  (let ((oct (octave note)))
	    (make-string (abs (- 3 oct))
			 :initial-element (if (> oct 3)
					      #\'
					      #\,)))))

(defmethod note->ly-pitch ((note integer))
  (note->ly-pitch (car (pitch-spell (list note)))))

(defmethod note->ly-pitch ((note (eql 'rest)))
  "r")
		
(defun final-rest (pitches durations &optional (beats-per-bar 4))
  (let ((total (reduce #'+ durations)))
    (if (zerop (mod total beats-per-bar))
	(values pitches durations)
	(let ((missing-dur (loop :for x :by beats-per-bar
				 :when (> x total)
				   :return (- x total))))
	  (if (rest-p (alexandria:last-elt pitches))
	      (values pitches (append (butlast durations) (list (+ (alexandria:last-elt durations)
								   missing-dur))))
	      (values (append pitches (list 'rest)) (append durations (list missing-dur))))))))

(defun make-ly (pitches durations)
  "Take a list of PITCHES in midi note values and a list of DURATIONS and return a text string in Lilypond format."
  (string-trim '(#\Space) (funcall (alexandria:multiple-value-compose #'rhythm-spell
								      #'final-rest)
				   (pitch-spell-split pitches)
				   durations)))

(defparameter *lilypond*
  #+windows "C:/Program Files (x86)/LilyPond/usr/bin/lilypond.exe"
  #+linux "/app/.apt/usr/bin/lilypond.real")

(defparameter *score-template*
  "
  \\include \"template.ly\"
  ~{
  \\score {
    \\new Staff \\with { instrumentName = #(score-number) }
    { ~a \\bar \"|.\" }~%
  }
  ~}")

(defparameter *variations-n* 18)

(hunchentoot:define-easy-handler (root :uri "/") (input complexity)
  (setf (hunchentoot:content-type*) "application/pdf")
  (let* ((output-filename #+linux (if (uiop:directory-exists-p "/app/")
				      "/app/output"
				      "output")
			  #+windows "output")
	 (output-file (make-pathname :type "pdf" :defaults output-filename)))
    (uiop:with-temporary-file (:stream stream :pathname input-file)
      (multiple-value-bind (pitches durations)
	  (parse-input input)
	(format stream *score-template*
		(append (list (original-phrase input complexity))
			(loop :repeat (1- *variations-n*)
			      :collect (apply (alexandria:multiple-value-compose
					       #'make-ly
					       #'fix-very-short-durations
					       #'trim
					       #'process-n)
					      (list pitches durations (parse-integer complexity)))))))
      :close-stream
      (uiop:run-program (list *lilypond*
			      "-o" output-filename
			      "-I" "static/"
			      "-I" "/app/static/"
			      (namestring input-file))
			:output #p"~/ly.log"
			:error-output #p"~/ly-error.log"))
    (prog1
	(alexandria:read-file-into-byte-vector output-file)
      (uiop:delete-file-if-exists output-file))))
