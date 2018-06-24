(in-package :cl-user)
(defpackage cl-ignition.request
  (:use :cl)
  (:import-from :cl-ignition.query
		:sparql-query)
  (:import-from :cl-ignition.fuseki
                :query
		:virtuoso-repository
		:virtuoso-server)
  (:export :send-request
	   :sparql-server :server-instance
	   :make-sparql-server
	   :make-dbpedia-server
	   :request-server
	   :request-dbpedia))
(in-package :cl-ignition.request)

(defclass sparql-server ()
  ((server-instance :initarg :server :accessor server-instance)))

(defun make-sparql-server (arg)
  (make-instance 'sparql-server :server arg))

(defun send-request ()
  nil)

(defun make-dbpedia-server ()
  (make-sparql-server
   (make-instance 'virtuoso-repository
		  :name "DBpedia Japanese"
		  :server (make-instance 'virtuoso-server
					 :base-url "http://ja.dbpedia.org/sparql"))))

(defgeneric request-server (sparql-server string))
(defmethod request-server ((rdf-store-server sparql-server) (query-string string))
  (cl-ignition.fuseki::query (server-instance rdf-store-server) query-string))

(defun request-dbpedia (query)
  (request-server (make-dbpedia-server) query))
