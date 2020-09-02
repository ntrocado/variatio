;; TODO: duncan@bayne.id.au: make the Buildpack aware of the app package name
(in-package #:cl-user)

(defparameter *lilypond*
  #+windows "C:/Program Files (x86)/LilyPond/usr/bin/lilypond.exe"
  #+linux "/app/.apt/usr/bin/lilypond.real")

(defparameter *test-score*
  "\\score{
	{
		c'
	}

	\\layout{}
	\\midi{}
}")

(hunchentoot:define-easy-handler (root :uri "/") ()
  (setf (hunchentoot:content-type*) "application/pdf")
  (let* ((output-filename #+linux "/app/output"
			  #+windows "C:/Users/trocado/Desktop/output")
	 (output-file (make-pathname :type "pdf" :defaults output-filename)))
    (uiop:with-temporary-file (:stream stream :pathname input-file)
      (format stream *test-score*)
      :close-stream
      (uiop:run-program (list *lilypond*
			      "-o"
			      output-filename
			      (namestring input-file))))
    (prog1
	(alexandria:read-file-into-byte-vector output-file)
      (uiop:delete-file-if-exists output-file))))

(defvar *acceptor* nil)

(defun initialize-application (&key port)
  (setf hunchentoot:*dispatch-table*
    `(hunchentoot:dispatch-easy-handlers
       ,(hunchentoot:create-folder-dispatcher-and-handler
          "/" "/app/static/")))

  (when *acceptor*
    (hunchentoot:stop *acceptor*))

  (setf *acceptor*
    (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port))))
