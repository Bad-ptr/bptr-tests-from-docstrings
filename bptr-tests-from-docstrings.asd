#|
  Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
|#

(in-package :cl-user)

(defpackage bptr-tests-from-docstrings-asd
  (:use :cl :asdf))

(in-package :bptr-tests-from-docstrings-asd)

(defsystem bptr-tests-from-docstrings
  :version "0.1"
  :author "Constantin Kulikov"
  :license "GPL v2 or any higher."
  :description "Extract tests from docstrings."
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
  :depends-on (#+sbcl :sb-rt #-sbcl :rt
                      :cl-ppcre)
  :components ((:module "src"
                        :components
                        ((:file "bptr-tests-from-docstrings")))))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :bptr-tests-from-docstrings))))
  nil)

(defmethod perform ((o test-op) (c (eql (find-system :bptr-tests-from-docstrings))))
  (operate 'load-op :bptr-tests-from-docstrings-tests)
  (operate 'test-op :bptr-tests-from-docstrings-tests))
