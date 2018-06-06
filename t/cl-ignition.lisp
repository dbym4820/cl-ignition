(in-package :cl-user)
(defpackage cl-ignition-test
  (:use :cl
        :cl-ignition
        :prove))
(in-package :cl-ignition-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-ignition)' in your Lisp.

(plan nil)

(setf *enable-colors* nil)

;;(subtest "query-generation test")

(finalize)
