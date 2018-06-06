(in-package :cl-user)
(defpackage cl-ignition.request
  (:use :cl)
  (:export :request-ignitor
	   :send-request))
(in-package :cl-ignition.request)

(defclass request-ignitor ()
  ((requst :initform nil :accessor request-string)))

(defun send-request ()
  nil)
