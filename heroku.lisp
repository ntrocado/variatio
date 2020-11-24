;; TODO: duncan@bayne.id.au: make the Buildpack aware of the app package name
(in-package #:cl-user)

(defvar *acceptor* nil)

(setf hunchentoot:*show-lisp-errors-p* t
      hunchentoot:*show-lisp-backtraces-p* t)

(defun initialize-application (&key port)
  (setf hunchentoot:*dispatch-table*
    `(hunchentoot:dispatch-easy-handlers
       ,(hunchentoot:create-folder-dispatcher-and-handler
          "/" "/app/static/")))

  (when *acceptor*
    (hunchentoot:stop *acceptor*))

  (setf *acceptor*
    (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port))))
