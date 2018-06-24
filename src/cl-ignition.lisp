(in-package :cl-user)
(defpackage cl-ignition
  (:nicknames :ignitor)
  (:use :cl)
  (:import-from :cl-ignition.query
		:with-prefix
		:ensure-query
		:select :vars :distinct :?s :?p :?o :limit)
  (:import-from :cl-ignition.fuseki
                :query
		:virtuoso-repository
		:virtuoso-server)
  (:import-from :cl-ignition.request
		:request-ignitor
		:send-request)
  (:export :with-prefix
	   :ensure-query
	   :convert-query
	   :select :vars :distinct :?s :?p :?o :limit))
(in-package :cl-ignition)

(defun make-dbpedia-server ()
  (make-instance 'virtuoso-repository :name "DBpedia Japanese"
				      :server (make-instance 'virtuoso-server
							     :base-url "http://ja.dbpedia.org/sparql")))

(defun request-server (query-string rdf-store-server)
  (cl-ignition.fuseki:query rdf-store-server query-string))

(defun req-dbpedia (query)
  (request-server query (make-dbpedia-server)))

(defun sample-dbpedia ()
  (req-dbpedia "select distinct * where { <http://ja.dbpedia.org/resource/冴えない彼女の育てかた> ?p ?o } LIMIT 10"))

(defun anime-list-dbpedia ()
  (req-dbpedia "select distinct * where { ?s ?p <http://dbpedia.org/ontology/Cartoon> } LIMIT 10"))

