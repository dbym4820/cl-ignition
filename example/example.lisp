(in-package :cl-user)
(ql:quickload :cl-ignition)

(defun saekano-dbpedia ()
  (cl-ignition:request-dbpedia "select distinct * where { <http://ja.dbpedia.org/resource/冴えない彼女の育てかた> ?p ?o } LIMIT 10"))

(defun anime-list-dbpedia ()
  (cl-ignition:request-dbpedia "select distinct * where { ?s ?p <http://dbpedia.org/ontology/Cartoon> } LIMIT 10"))

(defun saekano ()
  (cl-ignition:with-prefix (val)
		       ((dbpedia-jp "http://ja.dbpedia.org/resource/")
			(rdf-schema "http://www.w3.org/2000/01/rdf-schema"))
		       ((:select (nil ?p ?o)
			 :distinct t
			 :subject (dbpedia-jp "冴えない彼女の育てかた")
			 :predicate (rdf-schema "#comment")))
    (cl-ignition.query::get-single-key "O"
				       (cl-ignition:request-dbpedia
					(cl-ignition:convert-query val)))))

(defun get-comment (resource-name)
  (cl-ignition:with-prefix (val)
		       ((dbpedia-jp "http://ja.dbpedia.org/resource/")
			(rdf-schema "http://www.w3.org/2000/01/rdf-schema"))
		       ((:select (nil ?p ?o)
			 :distinct t
			 :subject (dbpedia-jp resource-name)
			 :predicate (rdf-schema "#comment")))
    (caar
     (cl-ignition.query::get-single-key "O"
					(cl-ignition:request-dbpedia
					 (cl-ignition:convert-query val))))))

(defun get-abstract (resource-name)
  (cl-ignition:with-prefix (val)
		       ((dbpedia-jp "http://ja.dbpedia.org/resource/")
			(rdf-schema "http://www.w3.org/2000/01/rdf-schema")
			(ontology "http://dbpedia.org/ontology/"))
		       ((:select (nil ?p ?o)
			 :distinct t
			 :subject (dbpedia-jp resource-name)
			 :predicate (ontology "abstract")))
    (caar
     (cl-ignition.query::get-single-key "O"
					(cl-ignition:request-dbpedia
					 (cl-ignition:convert-query val))))))

