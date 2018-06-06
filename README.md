# cl-ignition - CLOS based SPARQL query generation library

## Status

- Just launched

## Usage

Generate SPARQL query from S-expression of Common Lisp.

(Ideal state: not implemented yet)

### Query generation

```
CL-USER> (in-package :cl-ignition)
CL-IGNITION> (make-sparql (select 
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
CL-USER> (ignitor:query ...)
```

## Author

* Tomoki Aburatnai (aburatanitomoki@gmail.com)

## Copyright

Copyright (c) 2018 Tomoki Aburatnai (aburatanitomoki@gmail.com)

## License

Licensed under the MIT License.
