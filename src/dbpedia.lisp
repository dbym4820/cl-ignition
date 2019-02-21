(in-package :cl-user)
(defpackage cl-ignition.dbpedia
  (:use :cl)
  (:import-from :alexandria
		:flatten)
  (:import-from :cl-ignition.query
		:convert-query
		:with-prefix
		:result-filter
		:get-single-key)
  (:import-from :cl-ignition.request
		:request-dbpedia)
  (:export :get-comment
	   :get-abstract
	   :get-wiki-links))
(in-package :cl-ignition.dbpedia)


(defun get-comment (resource-name)
  (with-prefix (val)
	       ((dbpedia-jp "http://ja.dbpedia.org/resource/")
		(rdf-schema "http://www.w3.org/2000/01/rdf-schema"))
	       ((:select (nil ?p ?o)
		 :distinct t
		 :subject (dbpedia-jp resource-name)
		 :predicate (rdf-schema "#comment")))
    (caar
     (get-single-key "O"
		     (request-dbpedia
		      (convert-query val))))))

(defun get-abstract (resource-name)
  (with-prefix (val)
	       ((dbpedia-jp "http://ja.dbpedia.org/resource/")
		(rdf-schema "http://www.w3.org/2000/01/rdf-schema")
		(ontology "http://dbpedia.org/ontology/"))
	       ((:select (nil ?p ?o)
		 :distinct t
		 :subject (dbpedia-jp resource-name)
		 :predicate (ontology "abstract")))
    (caar
     (get-single-key "O"
		     (request-dbpedia
		      (convert-query val))))))


(defun get-wiki-links (resource-name)
  (with-prefix (val)
	       ((dbpedia-jp "http://ja.dbpedia.org/resource/")
		(wiki-link "http://dbpedia.org/ontology/wikiPageWikiLink"))
	       ((:select (nil nil ?o)
		 :distinct t
		 :subject (dbpedia-jp resource-name)
		 :predicate (wiki-link "")))
    (flatten
     (get-single-key "O"
		     (request-dbpedia
		      (convert-query val))))))
