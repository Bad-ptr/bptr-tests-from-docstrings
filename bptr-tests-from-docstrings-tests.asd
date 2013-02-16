#|
  File: bptr-tests-from-docstrings-tests.asd
  Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
  Date: 2013/02/04 15:58:36
|#


(in-package :cl-user)

(defpackage bptr-tests-from-docstrings-tests-asd
  (:use :cl :asdf))

(in-package bptr-tests-from-docstrings-tests-asd)

(defsystem bptr-tests-from-docstrings-tests
  :depends-on (:bptr-tests-from-docstrings)
  :components ((:module "src"
                        :components
                        ((:file "bptr-tests-from-docstrings-tests")))))


(defmethod operation-done-p ((o test-op) (c (eql (find-system :bptr-tests-from-docstrings-tests))))
  nil)

(defmethod perform ((o test-op) (c (eql (find-system :bptr-tests-from-docstrings-tests))))
  (load-system :bptr-tests-from-docstrings-tests)
  (flet ((run-tests (&rest args)
           (apply (intern (string '#:retest-tfd-tests) '#:bptr-tests-from-docstrings-tests) args)))
    (run-tests)))
