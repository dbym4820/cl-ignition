(in-package :cl-user)
(defpackage cl-ignition.query-build
  (:use :cl)
  (:import-from :alexandria
		:destructuring-case)
  (:import-from :optima
		:match
		:property))
(in-package :cl-ignition.query-build)

#|
  API
|#
(defun string-conc (string-list)
  (apply #'concatenate 'string string-list))

#|
  Data type
|#
(defparameter *sparql-data-type-table* (make-hash-table :test #'equal))
(defun set-data-type (key type-string &optional (data-type-table *sparql-data-type-table*))
  (setf (gethash key data-type-table) type-string))
(defun get-data-type (key &optional (data-type-table *sparql-data-type-table*))
  (gethash key data-type-table))
(mapcar (lambda (key-type)
          (set-data-type (first key-type) (second key-type)))
        '((:xml "application/sparql-results+xml")
          (:json "application/sparql-results+json")
          (:bin "application/x-binary-rdf-results-table")
          (:rdfxml "application/rdf+xml")
          (:ntriple "text/plain")
          (:turtle "application/x-turtle")
          (:n3 "text/rdf+n3")
          (:nrix "application/trix")
          (:trig "application/x-trig")
          (:plaintextboolean "text/boolean")))

#|
  Prefix operator
|#
(defparameter *sparql-prefix-table*
  (make-hash-table :test #'equal))
(defun create-prefix-table (table-name)
  (setf (gethash table-name *sparql-prefix-table*)
	(make-hash-table :test #'equal)))

(defparameter *ignition-prefix-list*
  (progn
    (create-prefix-table :default)
    (gethash :default *sparql-prefix-table*))
  "default sparql query prefix list")

(defun set-prefix (key iri &optional (prefix-list *ignition-prefix-list*))
  (setf (gethash key prefix-list) iri))
(defun get-prefix (key &optional (prefix-list *ignition-prefix-list*))
  (gethash key prefix-list))
(defun get-prefix-alist (&optional (prefix-list *ignition-prefix-list*))
  (loop for key being each hash-key of prefix-list
	  using (hash-value value)
	collect (cons key value)))


#|
  Default prefixes
|#
(mapcar #'(lambda (prefix-iri)
	    (set-prefix (first prefix-iri) (second prefix-iri)))
	'((:rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
	  (:owl "http://www.w3.org/2002/07/owl#")))




#|
  Send query for server
|#
(defclass sparql-server ()
  ((:url :initarg :url :accessor url)))
(defclass fuseki-server (sparql-server)
  ())
(defclass virtuoso-server (sparql-server)
  ())
   
(defgeneric server-query-endpoint-postfix (sparql-server))
(defgeneric server-update-endpoint-postfix (sparql-server))
(defgeneric server-data-endpoint-postfix (sparql-server))
(defgeneric server-upload-endpoint-postfix (sparql-server))

(defmethod server-query-endpoint-postfix ((server fuseki-server)) "/query")
(defmethod server-update-endpoint-postfix ((server fuseki-server)) "/update")
(defmethod server-data-endpoint-postfix ((server fuseki-server)) "/data")
(defmethod server-upload-endpoint-postfix ((server fuseki-server)) "/upload")

(defmethod server-query-endpoint-postfix ((server virtuoso-server)) "/sparql")
(defmethod server-update-endpoint-postfix ((server virtuoso-server)) "/sparql")
(defmethod server-data-endpoint-postfix ((server virtuoso-server)) "/sparql")
(defmethod server-upload-endpoint-postfix ((server virtuoso-server)) "/sparql")

(defgeneric query-endpoint (sparql-server)
  (:method ((server sparql-server))
    (string-conc `(,(url server) ,(server-query-endpoint-postfix server)))))
(defgeneric update-endpoint (sparql-server)
  (:method ((server sparql-server))
    (string-conc `(,(url server) ,(server-update-endpoint-postfix server)))))
(defgeneric data-endpoint (sparql-server)
  (:method ((server sparql-server))
    (string-conc `(,(url server) ,(server-data-endpoint-postfix server)))))
(defgeneric upload-endpoint (sparql-server)
  (:method ((server sparql-server))
    (string-conc `(,(url server) ,(server-upload-endpoint-postfix server)))))

(defgeneric send-query (sparql-server string)
  (:method ((server virtuoso-server) (query-string string))
    (dex:post (query-endpoint server)
	     :content `(("format" ,(get-data-type :json))
			("query" ,query-string)))))
