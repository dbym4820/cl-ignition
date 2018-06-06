#|
  This file is a part of cl-ignition project.
  Copyright (c) 2018 Tomoki Aburatnai (aburatanitomoki@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-ignition-test-asd
  (:use :cl :asdf))
(in-package :cl-ignition-test-asd)

(defsystem cl-ignition-test
  :author "Tomoki Aburatnai"
  :license "MIT"
  :depends-on (:cl-ignition
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-ignition"))))
  :description "Test system for cl-ignition"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
