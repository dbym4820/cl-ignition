(in-package :cl-user)
(ql:quickload :cl-ignition)

(defun saekano-dbpedia ()
  (ignitor:request-dbpedia "select distinct * where { <http://ja.dbpedia.org/resource/冴えない彼女の育てかた> ?p ?o } LIMIT 10"))

(defun anime-list-dbpedia ()
  (ignitor:request-dbpedia "select distinct * where { ?s ?p <http://dbpedia.org/ontology/Cartoon> } LIMIT 10"))

(defun saekano-2 ()
  (ignitor:with-prefix (val)
		       ((dbpedia-jp "http://ja.dbpedia.org/resource/"))
		       ((:select (nil ?p ?o)
			 :distinct t
			 :subject (dbpedia-jp "冴えない彼女の育てかた")
			 :limit 5))
    (ignitor:request-dbpedia (ignitor:convert-query val))))
