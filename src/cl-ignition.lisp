(in-package :cl-user)
(defpackage cl-ignition
  (:nicknames :ignitor)
  (:use :cl)
  (:import-from :ignitor/fuseki
                :query)
  (:import-from :ignitor/query
		:query-ignitor
		:with-query
		:make-query
		:make-query-string)
  (:import-from :ignitor/request
		:request-ignitor
		:send-request)
  (:export :query-ignitor :with-query :make-query :make-query-string
	   :request-ignitor :send-request))
(in-package :cl-ignition)

