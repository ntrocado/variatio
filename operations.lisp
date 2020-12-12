;;;; operations.lisp

(in-package #:variatio)

(defun reverse-pitches (pitches durations)
  (values (reverse pitches) durations))

(defun reverse-rhythms (pitches durations)
  (values pitches (reverse durations)))

(defun approaches (pitches durations approach-formula)
  (loop :for p :in pitches
	:for d :in durations
	:for a :in (apply #'alexandria:circular-list approach-formula)
	:if (not (rest-p p))
	  :append (list (+ p a) p) :into new-pitches
	  :and :append (list (/ d 2) d) :into new-durations
	:else
	  :collect p :into new-pitches
	  :and :collect d :into new-durations
	:finally (return (values new-pitches new-durations))))

(defun interval-fill (pitches durations)
  "Chromatically fill the interval between notes, when the interval is between a major second and a major third."
  (loop :for (p1 p2) :on pitches
	:for (d1 d2) :on durations
	:while p2
	:for interval := (unless (or (rest-p p1)
				     (rest-p p2))
			   (abs (- p2 p1)))
	:if (and interval (<= 2 interval 4))
	  :append (append (let ((direction (if (plusp (- p2 p1)) 1 -1)))
			    (alexandria:iota (1- interval)
					     :start (+ p1 direction)
					     :step direction))
			  (list p2))
	    :into new-pitches
	    :and :append (append (make-list (1- interval) :initial-element (/ d1 2))
				 (list d2))
		   :into new-durations
	:else
	  :collect p2 :into new-pitches
	  :and :collect d2 :into new-durations
	:finally (return (values (push (first pitches) new-pitches)
				 (push (first durations) new-durations)))))

(defun remove-notes (pitches durations prob)
  "Randomly remove notes and rests from PITCHES and DURATIONS with probability PROB (between 0 and 1). If all notes are removed, return lists respectively with the pitch and duration of the last original note."
  (assert (<= 0 prob 1) (prob) "PROB must be between 0 and 1, but ~a was given." prob)
  (loop :for p :in pitches
	:for d :in durations
	:unless (< (random 1.0) prob)
	  :collect p :into new-pitches
	  :and :collect d :into new-durations
	:finally (return (if (null new-pitches)
			     (values (last pitches) (last durations))
			     (values new-pitches new-durations)))))

(defun insert-rests (pitches durations prob)
  "Randomly insert rests with probability PROB (between 0 and 1). The duration each inserted rest repeats another existing note or rest."
  (assert (<= 0 prob 1) (prob) "PROB must be between 0 and 1, but ~a was given." prob)
  (loop :for p :in pitches
	:for d :in durations
	:if (< (random 1.0) prob)
	  :append (list 'rest p) :into new-pitches
	  :and :append (list (alexandria:random-elt durations) d)
		 :into new-durations
	:else
	  :collect p :into new-pitches
	  :and :collect d :into new-durations
	:finally (return (values new-pitches new-durations))))

(defun rotate (pitches durations)
  (values (alexandria:rotate pitches)
	  (alexandria:rotate durations)))

(defun augment (pitches durations &optional (by 2))
  (values pitches (mapcar (lambda (x) (* x by)) durations)))

;;; from trocadolib
(defun scale-value (value orig-min orig-max dest-min dest-max &key (curve 1))
  "Scales VALUE from an original to a destination range. If VALUE, ORIG-MIN and ORIG-MAX are all the same, returns the lowest value of the destination bracket. Set CURVE to 1 for linear scaling, higher for exponential scaling."
  (assert (and (>= value orig-min)
	       (<= value orig-max))
	  (value orig-min orig-max)
	  "~S must be between ~S and ~S." value orig-min orig-max)
  (cond ((= value orig-min orig-max) dest-min)
	((= curve 1) (+ (/ (* (- value orig-min)
			      (- dest-max dest-min))
			   (- orig-max orig-min))
			dest-min))
	(t (let* ((b curve)
		  (s (/ (- dest-max dest-min) (- b 1)))
		  (r (/ (- (- dest-max dest-min)) (- b 1))))
	     (+ (* s (expt b (/ value (- orig-max orig-min))))
		r
		dest-min)))))

(defun rhythm-flatten (pitches durations)
  "Re-scale DURATIONS reducing the range between the shortest and longest values."
  (let* ((min (apply #'min durations))
	 (max (apply #'max durations))
	 (halfway (/ (- max min) 4)))
    (values pitches (mapcar (lambda (d)
			      (scale-value d min max
					   (+ min halfway)
					   (- max halfway)))
			    durations))))

(defun rhythm-raise-floor (pitches durations)
  "Raise minimum duration to twice the shortest one in DURATIONS."
  (let* ((min (apply #'min durations))
	 (max (apply #'max durations)))
    (values pitches (mapcar (lambda (d)
			      (alexandria:clamp d (* min 2) max))
			    durations))))

(defun octave-shift (pitches durations prob)
  (values (mapcar (lambda (x)
		    (if (and (< (random 1.0) prob)
			     (not (rest-p x)))
			(funcall (alexandria:random-elt '(+ -)) x 12)
			x))
		  pitches)
	  durations))

(defun bookend-mean (pitches durations)
  "Add the mean of PITCHES rounded down to the start and rounded up to the end of PITCHES."
  (values (let ((mean (alexandria:mean pitches)))
	    (append (list (floor mean))
		    pitches
		    (list (ceiling mean))))
	  durations))

(defun bookend-over-ambitus (pitches durations)
  "Add the note below the lowest one to the start and above the highest one to the end of PITCHES."
  (values (let* ((no-rests (remove 'rest pitches))
		 (1+highest (1+ (apply #'max no-rests)))
		 (1-lowest (1- (apply #'min no-rests))))
	    (append (list 1-lowest)
		    pitches
		    (list 1+highest)))
	  durations))

(defun mv-expand-compress (pitches durations amount)
  "Expand or compress the melodic vector for PITCHES; AMOUNT is >1 for expanding and <1 for compressing."
  (values (let* ((no-rests (remove 'rest pitches))
		 (first-pitch (first no-rests))
		 (mv (loop :for (a b) :on no-rests
			   :while b
			   :sum (round (* amount (- b a))) :into s
			   :collect s)))
	    (append (list first-pitch)
		    (loop :for p :in (rest pitches)
			  :if (rest-p p)
			    :collect p
			  :else
			    :collect (+ first-pitch (pop mv)))))
	  durations))
