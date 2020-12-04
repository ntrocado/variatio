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
	:if p
	  :append (list (+ p a) p) :into new-pitches
	  :and :append (list (/ d 2) d) :into new-durations
	:else
	  :collect p :into new-pitches
	  :and :collect d :into new-durations
	:finally (return (values new-pitches new-durations))))

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
  (assert (< 0 prob 1) (prob) "PROB must be between 0 and 1, but ~a was given." prob)
  (loop :for p :in pitches
	:for d :in durations
	:collect p :into new-pitches
	:collect d :into new-durations
	:unless (< (random 1.0) prob)
	  :collect nil :into new-pitches
	  :and :collect (alexandria:random-elt durations)
	:finally (return (values new-pitches new-durations))))

(defun rotate (pitches durations)
  (values (alexandria:rotate pitches)
	  (alexandria:rotate durations)))
