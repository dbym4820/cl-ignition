(in-package :cl-user)
(defpackage ignitor/request
  (:use :cl)
  (:export :request-ignitor
	   :send-request))

(defclass request-ignitor ()
  ((requst :initform nil :accessor request-string)))

(defun send-request ()
  nil)
