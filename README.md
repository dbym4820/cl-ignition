# cl-ignition - Yet another Common Lisp SPARQL library

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

Generate SPARQL request as S-expression on Common Lisp.

### Query Generation

```
;; e.g.: get 5 data which has the URI "http://ja.dbpedia.org/resource/冴えない彼女の育てかた" as subject parameter from DBpedia Japan
CL-USER> (ql:quickload :cl-ignition :silent t)
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
CL-USER> (defparameter *dbpedia-server*
           (make-instance 'cl-ignition.fuseki:virtuoso-repository 
              :name "DBpedia Japanese"
              :server (make-instance 'cl-ignition.fuseki:virtuoso-server
                         :base-url "http://ja.dbpedia.org/sparql")))
*DBPEDIA-SERVER*
CL-USER> *dbpedia-server*
#<CL-IGNITION.FUSEKI:VIRTUOSO-REPOSITORY #x3020022966DD>
CL-USER> (cl-ignition.fuseki:query *dbpedia-server* *sample-query*)

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
