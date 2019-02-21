(in-package :cl-user)
(defpackage cl-ignition
  (:nicknames :ignition)
  (:use :cl)
  (:import-from :cl-ignition.query
		:with-prefix
		:ensure-query
		:convert-query
		:select :vars :distinct :?s :?p :?o :limit
                :extract-key-value :result-filter :get-single-key)
  (:import-from :cl-ignition.request
		:send-request
		:sparql-server :server-instance
		:make-sparql-server
		:make-dbpedia-server
		:request-server
		:request-dbpedia)
  (:import-from :cl-ignition.fuseki
                :query
		:virtuoso-repository
		:virtuoso-server)
  (:import-from :cl-ignition.dbpedia
		:export :get-comment
                :get-abstract
		:get-wiki-links)
  (:export
   ;; query
   :with-prefix
   :ensure-query
   :convert-query
   :select
   :vars
   :distinct
   :?s
   :?p
   :?o
   :limit
   :extract-key-value
   :result-filter
   :get-single-key
   ;; request
   :sparql-server
   :make-sparql-server
   :make-dbpedia-server
   :request-server
   :request-dbpedia
   ;; fuseki
   :virtuoso-repository
   :virtuoso-server
   :query
   ;; dbpedia
   :get-comment
   :get-abstract
   :get-wiki-links
))
(in-package :cl-ignition)

