(in-package :cl-user)
(defpackage cl-ignition.query
  (:use :cl)
  (:export :query-ignitor
	   :make-query
	   :make-query-string
	   :with-query))
(in-package :cl-ignition.query)

(defclass query-ignitor ()
  ((query :initform nil :accessor query-string)))

(defun make-query ()
  (format nil "sample func query-make"))

(defun make-query-string ()
  (format nil "sample func"))

(defmacro with-query (var &body body)
  (declare (ignorable var body)))
