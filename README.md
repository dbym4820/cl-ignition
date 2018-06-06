# cl-ignition - CLOS based SPARQL query generation library

## Status

- Just launched

## Usage

Generate SPARQL query from S-expression of Common Lisp.

(Ideal state: not implemented yet)

### Query generation

```
CL-USER> (ql:quickload :cl-ignition)

#| Simaple SPARQL Query
PREFIX dbpedia-jp:   <http://ja.dbpedia.org/resource/>
SELECT ?p ?o WHERE { dbpedia:東京都 ?p ?o } LIMIT 10
|#
CL-USER> (ignitor:with-query (?p ?o)
           ((prefixs 'dbpedia-jp "<http://ja.dbpedia.org/resource/>"
	                 'example "<http://example.org/>")            
            (select (?p ?o)
	                 (where (and (dbpedia-jp "東京都") ?p ?o))
	                 (limit 1)))
           (format nil "~A, ~A~%" ?p ?o))
;; => "\"http://www.w3.org/1999/02/22-rdf-syntax-ns#typ", \"http://www.w3.org/2002/07/owl#Thing\"

CL-USER> (defparameter *sparql-endpoint* (ignitor:make-endpoint "http://ja.dbpedia.org/sparql"))
CL-USER> (defparameter *sample-query*
           (ignitor:make-query
                (select (?p ?o)
	                      (where (and (dbpedia-jp "東京都") ?p ?o))
	                      (limit 10))))
;;; => #<CL-IGNITION.QUERY:QUERY-IGNITOR #x30200233AFBD>

CL-USER> (send-query *sample-query* *sparql-endpoint*)
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
```

### 

## Installation

- Download this package from github
```
~ $ cd /path/to/quicklisp/local-projects/
~ $ git clone https://github.com/dbym4820/cl-ignition.git
```
- Install via quicklisp 

```
CL-USER> (ql:quickload :cl-ignition)
To load "cl-ignition":
  Load 1 ASDF system:
    cl-ignition
; Loading "cl-ignition"
..
(:CL-IGNITION)
```

## Author

* Tomoki Aburatnai (aburatanitomoki@gmail.com)

## Copyright

Copyright (c) 2018 Tomoki Aburatnai (aburatanitomoki@gmail.com)

## License

Licensed under the MIT License.

## Reference

[W3C SPARQL query (English)](https://www.w3.org/TR/rdf-sparql-query/)
[W3C SPARQL query (Japanese)](http://www.asahi-net.or.jp/~ax2s-kmtn/internet/rdf/rdf-sparql-query.html)
