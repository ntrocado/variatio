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
	      12)))

(defun parse-input (input n &key (return-notes nil))
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
				 (text-octave->value octave (truncate (/ (or (first midi)
									     60)
									 12)))))))
		    midi)
	      (push (or (parse-float:parse-float dur :junk-allowed t)
			(or (first durations) 1))
		    durations))
	:finally (return (values (reverse midi) (reverse durations) n))))

(defun original-phrase (input n)
  (multiple-value-bind (pitches durations n)
      (parse-input input n :return-notes t)
    (declare (ignore n))
    (rhythm-spell pitches durations)))

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
		 (list #'rotate (list pitches durations))
		 (list #'augment (list pitches durations (if (> (random 1.0) .75)
							     2
							     (alexandria:random-elt '(1.5 2.5 3)))))
		 (list #'rhythm-flatten (list pitches durations))
		 (list #'rhythm-raise-floor (list pitches durations))))))
    (apply (first op) (second op))))

(defun process-n (pitches durations n)
  (if (zerop n)
      (values pitches durations)
      (multiple-value-bind (pitches durations)
	  (process pitches durations)
	(process-n pitches durations (1- n)))))

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
      (fix-very-short-notes pitches (mapcar (lambda (x) (* x 2)) durations))
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
	    (make-string (abs (- 3 oct))
			 :initial-element (if (> oct 3)
					      #\'
					      #\,)))))

(defmethod note->ly-pitch ((note integer))
  (note->ly-pitch (car (pitch-spell (list note)))))

(defmethod note->ly-pitch ((note (eql 'rest)))
  "r")

(defun ends-a-beat-p (end)
  (zerop (nth-value 1 (truncate end))))

(defun rest-of-beat (onset)
  (- 1 (nth-value 1 (truncate onset))))

(defun crosses-beats-p (duration onset)
  (> (+ duration onset)
     (truncate (1+ onset))))

(defun rest-of-bar (onset &optional (beats-per-bar 4))
  (- beats-per-bar (mod onset beats-per-bar)))

(defun crosses-bars-p (duration onset &optional (beats-per-bar 4))
  (> (+ (mod onset beats-per-bar) duration)
     beats-per-bar))

(defun post-half-bar (duration onset beats-per-bar)
  (let ((r (- (+ duration (mod onset beats-per-bar))
	      (/ beats-per-bar 2))))
    (when (and (< onset (/ beats-per-bar 2))
	       (plusp r)
	       ;; some simple cases
	       ;; TODO add more?...
	       (not (or (and (= duration 4) (= onset 0) (= beats-per-bar 4))
			(and (= duration 2) (= onset 1) (= beats-per-bar 4))
			(and (>= duration 3) (= onset 0) (= beats-per-bar 4))
			(and (>= duration 3) (= onset 1) (= beats-per-bar 4)))))
      r)))

(defun int-frac (d)
  (let* ((int-part (if (>= d 1)
		       (loop :for i := 1 :then (* i 2)
			     :while (<= i d)
			     :maximize i)
		       (loop :for i := 1 :then (/ i 2)
			     :minimize i
			     :until (<= i d))))
	 (frac-part (- d int-part)))
    (values int-part frac-part)))

(defun ties-across-beats (p d tie-from &optional (beats-per-bar 4))
  (format nil "~a~@[~a~]~a"
	  (rhythm-spell (list p) (list tie-from) beats-per-bar)
	  (when (not (rest-p p)) "~")
	  (rhythm-spell (list p) (list (- d tie-from)) beats-per-bar)))

(defun rhythm-spell (pitches durations &optional (beats-per-bar 4))
  (with-output-to-string (out)
    (loop :for p :in pitches
	  :for d :in durations
	  :for end := d :then (+ end d)
	  :for onset := 0 :then (- end d)
	  :do
	     (print onset)
	     (alexandria:if-let (post-half-bar (post-half-bar d onset beats-per-bar))
	       (princ (ties-across-beats p d (- d post-half-bar) beats-per-bar)
		      out)

	       (cond ((crosses-bars-p d onset beats-per-bar)
		      (princ (ties-across-beats p d (rest-of-bar onset beats-per-bar))
			     out))
		    
		     ((or (ends-a-beat-p end)
			  (not (crosses-beats-p d onset))
			  (and (or (>= d 2)
				   (= d 1.5))
			       (zerop (nth-value 1 (truncate onset))))
			  (and (= d 1) (zerop (mod end .5))))
      		      ;; ends on the beat, doesn't extend across beats, special cases
		      (multiple-value-bind (before-tie after-tie)
			  (int-frac d)
			(format out " ~a~a~@[~a~]"
				(note->ly-pitch p)
				(cond ((= before-tie 8) "\\longa")
				      ((= before-tie 16) "\\breve")
				      (t (/ 4 before-tie)))
				(cond
				  ;; dot
				  ((and (= after-tie (/ before-tie 2))
					(or (not (rest-p p))
					    (< d 1)))
				   ".")
				  ;; tie
				  ((plusp after-tie)
				   (format nil "~@[~a~]~a"
					   (when (not (rest-p p)) "~")
					   (rhythm-spell (list p)
							 (list after-tie)
							 beats-per-bar)))))))
		     
		     ;; ties across beats
		     (t (princ (ties-across-beats p d (rest-of-beat onset))
			       out)))))))

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
  (concatenate 'string
	       (format nil "\\include \"~a\""
		       (or (uiop:file-exists-p "template.ly")
			   (uiop:file-exists-p "static/template.ly")
			   "/app/static/template.ly"))
	       "
  ~{
  \\score {
    \\new Staff \\with { instrumentName = #(score-number) }
    { ~a \\bar \"|.\" }~%
  }
  ~}"))

(defparameter *variations-n* 50)

(hunchentoot:define-easy-handler (root :uri "/") (input complexity)
  (setf (hunchentoot:content-type*) "application/pdf")
  (let* ((output-filename #+linux (if (uiop:directory-exists-p "/app/")
				      "/app/output"
				      "output")
			  #+windows "C:/Users/trocado/Desktop/output")
	 (output-file (make-pathname :type "pdf" :defaults output-filename)))
    (uiop:with-temporary-file (:stream stream :pathname input-file)
      (format stream *score-template*
	      (append (list (original-phrase input complexity))
		      (loop :repeat (1- *variations-n*)
			    :collect (apply (alexandria:multiple-value-compose
					     #'make-ly
					     #'fix-very-short-durations
					     #'trim
					     #'process-n
					     #'parse-input)
					    (list input (parse-integer complexity))))))
      :close-stream
      (uiop:run-program (list *lilypond*
			      "-o"
			      output-filename
			      (namestring input-file))
			:output #p"~/ly.log"
			:error-output #p"~/ly-error.log"))
    (prog1
	(alexandria:read-file-into-byte-vector output-file)
      (uiop:delete-file-if-exists output-file))))
