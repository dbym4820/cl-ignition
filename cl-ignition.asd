(in-package :cl-user)
(defpackage cl-ignition-asd
  (:use :cl :asdf))
(in-package :cl-ignition-asd)

(defsystem cl-ignition
  :version "0.1"
  :author "Tomoki Aburatnai"
  :license "MIT"
  :depends-on (:optima
	       :dexador
	       :jonathan
	       :alexandria
	       :utsushiyo
	       ;;; for cl-fuseki
	       :jsown
	       :drakma
	       :cl-ppcre)
  :components ((:module "src"
                :components
                ((:file "cl-ignition" :depends-on ("query" "request" "fuseki" "dbpedia"))
		 (:file "dbpedia" :depends-on ("query" "request" "fuseki"))
		 (:file "query" :depends-on ("fuseki"))
		 (:file "request" :depends-on ("fuseki" "query"))
		 (:file "fuseki"))))
  :description "Common Lisp SPARQL library"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #P"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-ignition-test))))
