(in-package :cl-user)
(defpackage cl-ignition
  (:nicknames :ignitor)
  (:use :cl)
  (:import-from :cl-ignition.fuseki
                :query)
  (:import-from :cl-ignition.query
		:query-ignitor
		:with-query
		:make-query
		:make-query-string)
  (:import-from :cl-ignition.request
		:request-ignitor
		:send-request)
  (:export :query-ignitor :with-query :make-query :make-query-string
	   :request-ignitor :send-request
	   :make-a))
(in-package :cl-ignition)

(defun make-a ()
  (format nil "aaa"))
