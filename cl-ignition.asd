#|
  This file is a part of cl-ignition project.
  Copyright (c) 2018 Tomoki Aburatnai (aburatanitomoki@gmail.com)
|#

#|
  Author: Tomoki Aburatnai (aburatanitomoki@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-ignition-asd
  (:use :cl :asdf))
(in-package :cl-ignition-asd)

(defsystem cl-ignition
  :version "0.1"
  :author "Tomoki Aburatnai"
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "cl-ignition"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
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
