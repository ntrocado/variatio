(in-package :variatio)

;;; INPUT

;;; The input syntax is as follows:
;;;
;;; - Notes are separated by spaces.
;;; - Each note starts with a pitch letter (a-g), or r for a rest.
;;; - Add s for sharp, b for flat (eg, cs is C-sharp, bb is B-flat).
;;; - Add one or more quotes (') to indicate octaves above middle C, commas (,) indicate octaves
;;;   below middle C. For example: c' is the C above middle C (C5, as it is also usually called)
;;;   ab,, is the A-flat two octaves below middle C (A3).
;;; - Add the rhythmic value in number of beats. Examples: c1 is a quarter-note C; d.5 is an
;;;   eighth-note D (.5=half a beat); e1.75 is a double-dotted
;;;   quarter-note E, or a quarter-note tied to a dotted eight-note… (the exact notation is set
;;;   by the computer); fs4 is a whole-note F-sharp.
;;; - One more thing about octaves: they are all relative to the first note. If you type
;;;   g,,1 a1 b1 c1' the first three notes are two octaves below middle-C, and the last one one
;;;   octave below (G3 A3 B3 C4).
;;; - One more thing about rhythms: If you don't indicate a rhythmic value, the preceding one is
;;;   assumed. So you could write the last example like this: g,,1 a b c'.
;;;
;;; Some more examples of valid phrases:
;;;
;;; f1 r.5 f'1 d'.5 e'1 (Moose The Mooche)
;;; r.5 g g g eb2 r.5 f f f d2 (Beethoven's fifth)
;;; d2 a f d cs d1 e f2.5 g.5 f e d1 (Bach's Art of the Fugue)
;;; cs'.5 e gs b as1.5 gs.5 fs ds b,1 (Coltrane's solo on Giant Steps)
;;; d.5 e f g e1 c.5 d1.5 (The Lick)

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

(defparameter *default-octave* 4)

(defun text-octave->value (octave &optional (default *default-octave*))
  "Parse the string OCTAVE, counting quotes and commas to return the corresponding base number in midi notes."
  (* (if octave
	 (+ default (- (count #\' octave)
		       (count #\, octave)))
	 default)
     12))

(defun find-default-octave (pitches)
  (alexandria:when-let (first-note (find-if #'numberp pitches :from-end t))
    (truncate (/ first-note 12))))

(defparameter *input-regex*
  "^([cdefgabr])(ss|s\\+|s|\\+|-|b|b-|bb)?('+|,+)?(\\d*\\.?\\d*)?$")

(defun parse-to-note (pitch accidental octave previous-notes)
  "Make a note object from args in input format."
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
							   previous-notes
							   :from-end t)
						  (make-instance 'note))))
		  12))))

(define-condition malformed-input-error (error)
  ((input :initarg :input :reader input)))

(defun parse-input (input &key (return-notes nil))
  "Parse text in INPUT format into a list of midi note values and a list of durations as fractions/multiples of a beat. Octaves are relative to the first one."
  (loop :with midi
	:with durations
	:for note :in (ppcre:split "\\s" input)
	:for first-octave := (or first-octave (find-default-octave midi))
	:do (or (ppcre:register-groups-bind (pitch accidental octave dur)
		    (*input-regex* note)
		  (push (alexandria:when-let (ch (character pitch))
			  (if return-notes
			      (parse-to-note ch accidental octave midi)
			      (if (char= ch #\r)
				  'rest
				  (+ (char-pitch->value ch)
				     (text-accidental->value accidental)
				     (text-octave->value octave
							 (or first-octave
							     *default-octave*))))))
			midi)
		  (push (or (parse-float:parse-float dur :junk-allowed t)
			    (or (first durations) 1))
			durations))
		(error 'malformed-input-error :input note))
	:finally (return (values (reverse midi) (reverse durations)))))

(defun original-phrase (input)
  "Parse the input phrase, respecting the note spelling already chosen by the user."
  (multiple-value-bind (pitches durations)
      (parse-input input :return-notes t)
    (multiple-value-call #'rhythm-spell (final-rest pitches durations))))


;;; PROCESS

(defun process (pitches durations)
  (let ((op (alexandria:random-elt
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
		 (list #'augment (list pitches durations
				       (if (> (random 1.0) .5)
					   2
					   (alexandria:random-elt '(1.5 3)))))
		 (list #'rhythm-flatten (list pitches durations))
		 (list #'rhythm-raise-floor (list pitches durations))
		 (list #'interval-shift (list pitches durations
					      .4 (1+ (random 13))))
		 (list #'octave-shift (list pitches durations
					    .3))
		 (list #'bookend-mean (list pitches durations))
		 (list #'bookend-over-ambitus (list pitches durations))
		 (list #'mv-expand-compress (list pitches durations
						  (+ .1 (random 2.4))))))))
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
  "If there are durations smaller than a 32th note, double all of them. This is a solution to a limitation of Lilypond, that can't render such short notes unless they are part of a beamed group. They would be very unwieldy to read, anyway."
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
  "Fills up the last bar with rests."
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
(defparameter *frontend-uri* "https://nunotrocado.com/software/variatio.html")

(hunchentoot:define-easy-handler (root :uri "/") (input complexity)
  (setf (hunchentoot:content-type*) "application/pdf")
  (if input
      (let* ((output-filename #+linux (if (uiop:directory-exists-p "/app/")
					  "/app/output"
					  "output")
			      #+windows "output")
	     (output-file (make-pathname :type "pdf" :defaults output-filename)))
	(uiop:with-temporary-file (:stream stream :pathname input-file)
	  (multiple-value-bind (pitches durations)
	      (parse-input input)
	    (format stream *score-template*
		    (append (list (original-phrase input))
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
	  (uiop:delete-file-if-exists output-file)))

      ;; redirect when no input
      (hunchentoot:redirect *frontend-uri*)))
