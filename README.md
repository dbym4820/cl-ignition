# cl-ignition - Yet another Common Lisp SPARQL library

Generate SPARQL request as S-expression on Common Lisp.

## Status

- Select
    - [X] basic
    - [X] limit
    - [ ] filter
- Graph

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

## Usage

e.g.: get 5 data which has the URI "http://ja.dbpedia.org/resource/冴えない彼女の育てかた" as subject parameter from DBpedia Japan

### Query Generation

usage of with-prefix and convert-query macro

```
CL-USER> (defparameter *sample-query*
           (ignitor:with-prefix (val)
                ((dbpedia-jp "http://ja.dbpedia.org/resource/"))
                ((:select (nil ?p ?o)
                    :distinct t
                    :subject (dbpedia-jp "冴えない彼女の育てかた")
                    :limit 5))
         (ignitor:convert-query val)))
*SAMPLE-QUERY*
CL-USER> *SAMPLE-QUERY*
"select distinct ?P ?O where { <http://ja.dbpedia.org/resource/冴えない彼女の育てかた> ?P ?O limit 5 . }"
```

### Setting Server(Sparql repository)

```
CL-USER> (defparameter *dbpedia-server*
           (make-instance 'cl-ignition.fuseki:virtuoso-repository 
              :name "DBpedia Japanese"
              :server (make-instance 'cl-ignition.fuseki:virtuoso-server
                         :base-url "http://ja.dbpedia.org/sparql")))
*DBPEDIA-SERVER*
CL-USER> *dbpedia-server*
#<CL-IGNITION.FUSEKI:VIRTUOSO-REPOSITORY #x3020022966DD>
```

### Request Query

```
CL-USER> (cl-ignition.fuseki:query *dbpedia-server* *sample-query*)
;; =>
((:OBJ ("P" :OBJ ("type" . "uri") 
		     ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
	   ("O" :OBJ ("type" . "uri") 
		     ("value" . "http://www.w3.org/2002/07/owl#Thing")))
 (:OBJ ("P" :OBJ ("type" . "uri") 
             ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
	   ("O" :OBJ ("type" . "uri")
		     ("value" . "http://dbpedia.org/ontology/Anime"))) 
 (:OBJ ("P" :OBJ ("type" . "uri")
		     ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
       ("O" :OBJ ("type" . "uri")
		     ("value" . "http://dbpedia.org/ontology/Cartoon")))
 (:OBJ ("P" :OBJ ("type" . "uri") 
		     ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")) 
	   ("O" :OBJ ("type" . "uri")
		     ("value" . "http://dbpedia.org/ontology/Work")))
 (:OBJ ("P" :OBJ ("type" . "uri") 
		     ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")) 
	   ("O" :OBJ ("type" . "uri") 
		     ("value" . "http://schema.org/CreativeWork"))))
```

### Include whole process in with-prefix

```
CL-USER>  (ignitor:with-prefix (val)
			       ((dbpedia-jp "http://ja.dbpedia.org/resource/"))
			       ((:select (nil ?p ?o)
				 :distinct t
				 :subject (dbpedia-jp "冴えない彼女の育てかた")
				 :limit 5))
	    (ignitor:request-dbpedia (ignitor:convert-query val)))
;; =>
((:OBJ ("P" :OBJ ("type" . "uri") 
             ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
       ("O" :OBJ ("type" . "uri") 
             ("value" . "http://www.w3.org/2002/07/owl#Thing")))
 (:OBJ ("P" :OBJ ("type" . "uri") 
             ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
       ("O" :OBJ ("type" . "uri")
		     ("value" . "http://dbpedia.org/ontology/Anime"))) 
 (:OBJ ("P" :OBJ ("type" . "uri")
             ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
       ("O" :OBJ ("type" . "uri")
             ("value" . "http://dbpedia.org/ontology/Cartoon")))
 (:OBJ ("P" :OBJ ("type" . "uri") 
             ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")) 
       ("O" :OBJ ("type" . "uri")
             ("value" . "http://dbpedia.org/ontology/Work")))
 (:OBJ ("P" :OBJ ("type" . "uri") 
             ("value" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")) 
       ("O" :OBJ ("type" . "uri") 
             ("value" . "http://schema.org/CreativeWork"))))
```

## Author

* Tomoki Aburatnai (aburatanitomoki@gmail.com)

## Copyright

Copyright (c) 2018 Tomoki Aburatnai (aburatanitomoki@gmail.com)

## License

Licensed under the MIT License.

## Reference

-[W3C SPARQL query (English)](https://www.w3.org/TR/rdf-sparql-query/)
-[W3C SPARQL query (Japanese)](http://www.asahi-net.or.jp/~ax2s-kmtn/internet/rdf/rdf-sparql-query.html)
