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
