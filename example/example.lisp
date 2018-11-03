(in-package :cl-user)
(ql:quickload :cl-ignition)

(defun saekano-dbpedia ()
  (ignitor:request-dbpedia "select distinct * where { <http://ja.dbpedia.org/resource/冴えない彼女の育てかた> ?p ?o } LIMIT 10"))

(defun anime-list-dbpedia ()
  (ignitor:request-dbpedia "select distinct * where { ?s ?p <http://dbpedia.org/ontology/Cartoon> } LIMIT 10"))

(defun saekano ()
  (ignitor:with-prefix (val)
		       ((dbpedia-jp "http://ja.dbpedia.org/resource/")
			(rdf-schema "http://www.w3.org/2000/01/rdf-schema"))
		       ((:select (nil ?p ?o)
			 :distinct t
			 :subject (dbpedia-jp "冴えない彼女の育てかた")
			 :predicate (rdf-schema "#comment")))
    (cl-ignition.query::get-single-key "O"
				       (ignitor:request-dbpedia
					(ignitor:convert-query val)))))

(defun get-comment (resource-name)
  (ignitor:with-prefix (val)
		       ((dbpedia-jp "http://ja.dbpedia.org/resource/")
			(rdf-schema "http://www.w3.org/2000/01/rdf-schema"))
		       ((:select (nil ?p ?o)
			 :distinct t
			 :subject (dbpedia-jp resource-name)
			 :predicate (rdf-schema "#comment")))
    (caar
     (cl-ignition.query::get-single-key "O"
					(ignitor:request-dbpedia
					 (ignitor:convert-query val))))))
