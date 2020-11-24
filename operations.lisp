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
