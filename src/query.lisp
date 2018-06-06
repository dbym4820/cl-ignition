(in-package :cl-user)
(defpackage ignitor/query
  (:use :cl)
  (:export :query-ignitor
	   :make-query
	   :make-query-string
	   :with-query))

(defclass query-ignitor ()
  ((query :initform nil :accessor query-string)))

(defun make-query ()
  (make-instance 'query-ignitor))

(defun make-query-string ()
  nil)

(defmacro with-query (var &body body)
  (declare (ignorable var body)))
