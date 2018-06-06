(in-package :cl-user)
(defpackage cl-ignition-asd
  (:use :cl :asdf))
(in-package :cl-ignition-asd)

(defsystem cl-ignition
  :version "0.1"
  :author "Tomoki Aburatnai"
  :license "MIT"
  :depends-on (:cl-ppcre
	       :dexador
	       :jonathan
	       :cl-fuseki)
  :components ((:module "src"
                :components
                ((:file "cl-ignition"))))
  :description "CLOS based SPARQL query generation library"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
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
