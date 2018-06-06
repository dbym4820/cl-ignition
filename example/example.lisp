(in-package :cl-user)
(ql:quickload :cl-ignition)

#| Simaple SPARQL Query
PREFIX dbpedia-jp:   <http://ja.dbpedia.org/resource/>
SELECT ?p ?o WHERE { dbpedia:東京都 ?p ?o } LIMIT 10
|#
(ignitor:with-query (?p ?o)
  ((prefixs 'dbpedia-jp "<http://ja.dbpedia.org/resource/>"
	    'example "<http://example.org/>")            
   (select (?p ?o)
	   (where (and (dbpedia-jp "東京都") ?p ?o))
	   (limit 1)))
  (format nil "~A, ~A~%" ?p ?o))
=> "\"http://www.w3.org/1999/02/22-rdf-syntax-ns#typ", \"http://www.w3.org/2002/07/owl#Thing\"

(defparameter *sparql-endpoint* (make-endpoint "http://ja.dbpedia.org/sparql"))
(defparameter *sample-query*
  (ignitor:make-query
   (select (?p ?o)
	   (where (and (dbpedia-jp "東京都") ?p ?o))
	   (limit 10))))
;;; => #<CL-IGNITION.QUERY:QUERY-IGNITOR #x30200233AFBD>
(send-query *sample-query* *sparql-endpoint*)
#| =>
(((p "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") (o "http://www.w3.org/2002/07/owl#Thing"))
 ((p "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") (o "http://dbpedia.org/ontology/%3Chttp://purl.org/dc/terms/Jurisdiction%3E"))
 ((p "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") (o "http://dbpedia.org/ontology/AdministrativeRegion"))
 ((p "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") (o "http://dbpedia.org/ontology/Location"))
 ((p "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") (o "http://dbpedia.org/ontology/Place"))
 ((p "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") (o "http://dbpedia.org/ontology/PopulatedPlace"))
 ((p "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") (o "http://dbpedia.org/ontology/Region"))
 ((p "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") (o "http://schema.org/AdministrativeArea"))
 ((p "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") (o "http://schema.org/Place"))
 ((p "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") (o "http://www.wikidata.org/entity/Q3455524")))
|#
