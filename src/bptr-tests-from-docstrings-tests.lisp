#|
  File: bptr-tests-from-docstrings-tests.lisp
  Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
  Date: 2013/02/04 16:01:11
|#


(in-package :cl-user)

(defpackage bptr-tests-from-docstrings-tests
  (:use :cl :bptr-tests-from-docstrings)
  (:export #:retest-tfd-tests))

(in-package bptr-tests-from-docstrings-tests)

(defun retest-tfd-tests ()
  (let ((tfd-tests (make-empty-testsdb)))
    (make-tests-for-package :bptr-tests-from-docstrings :testsdb tfd-tests)
    (format *error-output* "~s" (with-output-to-string (ostr)
                                  (run-testsdb :testsdb tfd-tests :ostream ostr)))))
