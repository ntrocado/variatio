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
	:append (list (+ p a) p) :into new-pitches
	:append (list (/ d 2) d) :into new-durations
	:finally (return (values new-pitches new-durations))))

(defun remove-notes (pitches durations prob)
  "Randomly remove notes from PITCHES and DURATIONS with probability PROB (between 0 and 1). If all notes are removed, return lists with the pitch and duration of the last original note."
  (assert (< 0 prob 1) (prob) "PROB must be between 0 and 1, but ~a was given." prob)
  (loop :for p :in pitches
	:for d :in durations
	:unless (< (random 1.0) prob)
	  :collect p :into new-pitches
	  :and :collect d :into new-durations
	:finally (return (if (null new-pitches)
			     (values (last pitches) (last durations))
			     (values new-pitches new-durations)))))

