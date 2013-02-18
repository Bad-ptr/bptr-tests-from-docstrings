#|
  File: bptr-tests-from-docstrings.lisp
  Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
  Date: 2013/02/01 07:18:18
|#


(in-package :cl-user)

(defpackage bptr-tests-from-docstrings
  (:nicknames :bptr-tfd :cl-tests-from-docstrings)
  (:documentation
   "Make tests for package by extracting them from symbol's docstrings. Depends on rtest.")
  (:use :cl :cl-ppcre #+sbcl :sb-rt #-sbcl :rtest)
  (:import-from #+sbcl :sb-rt #-sbcl :rtest
                #:*compile-tests* #:*expected-failures*)
  (:export #:make-tests-for-symbol
           #:make-tests-for-package
           #:make-empty-testsdb
           #:clear-testsdb
           #:run-testsdb))

(in-package :bptr-tests-from-docstrings)


(defun make-empty-testsdb ()

  "Make empty testsdb.
Example:
 (make-empty-testsdb) => (nil)"

  (list nil))

(defvar *tests-entries* (make-empty-testsdb)

  "List of defined test entries.")


(defparameter *bptr-tfd-def-regexps*
  
  '(; Regexp for capturing tests region from docstrings. The first capture group treated as test region.
    :tregion-rx "\\b(?:Test|Example)s?\\:\\n((?:.+\\n*)+)(?=$)"
    ;; testforms regexp (not used by default). It's used with ppcre:all-matches-as-strings.
    :test-rx "(?<=\\[\\!)(.+\\n*)+(?=\\!\\])"
    ;; tests separator. It's used with ppcre:split.
    :tests-sep "(?:\\s*\\;{2,}\\s*|$)"
    ;; regexp that separate test expression from expected result. It's used with ppcre:split.
    :out-sep "\\s+[\\=\\-]+?\\>\\s+")

  "Default regexps for extracting tests from docstrings.")


(defun def-str-to-expr (str)

  "Make expression to test from string.
Example:
 (def-str-to-expr \"(fun 1 2)\") => (fun 1 2)"

  (multiple-value-bind (ret) (read-from-string str)
    ret))

(defun def-str-to-result (str)

  "Make expected result of test from string.
Example:
 (def-str-to-result \"(a b c) (1 2 3)\")
   => ((a b c) (1 2 3))"
  
  (with-input-from-string (s str)
    (do ((obj (read s nil :eof) (read s nil :eof))
         (ret nil))
        ((eq :eof obj) (nreverse ret))
      (push obj ret))))


(defun make-tests-for-package
    (pkg &key
           (tregion-rx (getf *bptr-tfd-def-regexps* :tregion-rx))
           (tests-sep (getf *bptr-tfd-def-regexps* :tests-sep))
           (out-sep (getf *bptr-tfd-def-regexps* :out-sep))
           (test-rx (getf *bptr-tfd-def-regexps* :test-rx))
           (operate-on 'package-symbols)
           (sym-types '(function variable type structure))
           excl-sym-rx
           (testsdb *tests-entries*)
           (str-to-expr #'def-str-to-expr)
           (str-to-result #'def-str-to-result))

  "Try to make tests for symbols in package."

  (let ((package (typecase pkg
                   (package pkg)
                   (string (find-package (string-upcase pkg)))
                   ((or symbol keyword) (find-package pkg))
                   (t nil))))
    (format *standard-output* "~&~%======= Generatin tests for package:~a =======~%~%" package)
    (if package
        (case operate-on
          (package-exports
           (do-external-symbols (s package testsdb)
             (unless (if excl-sym-rx
                         (ppcre:all-matches excl-sym-rx (symbol-name s))
                         nil)
               (make-tests-for-symbol s
                                      :tregion-rx tregion-rx
                                      :tests-sep tests-sep
                                      :out-sep out-sep
                                      :test-rx test-rx
                                      :sym-types sym-types
                                      :testsdb testsdb
                                      :str-to-expr str-to-expr
                                      :str-to-result str-to-result))))
          (package-symbols
           (do-symbols (s package testsdb)
             (when (and (eq (symbol-package s) package)
                        (not (if excl-sym-rx
                                 (ppcre:all-matches excl-sym-rx (symbol-name s))
                                 nil)))
               (make-tests-for-symbol s
                                      :tregion-rx tregion-rx
                                      :tests-sep tests-sep
                                      :out-sep out-sep
                                      :test-rx test-rx
                                      :sym-types sym-types
                                      :testsdb testsdb
                                      :str-to-expr str-to-expr
                                      :str-to-result str-to-result)))))
        (progn
          (format *error-output* "[Error]~TNo such package found: ~s~%" pkg)
          nil))
    (format *standard-output* "~&~%======= Done generating tests for package:~a =======~%~%" package)))



(defun make-tests-for-symbol
    (sym &key
           (tregion-rx (getf *bptr-tfd-def-regexps* :tregion-rx))
           (tests-sep (getf *bptr-tfd-def-regexps* :tests-sep))
           (out-sep (getf *bptr-tfd-def-regexps* :out-sep))
           (test-rx (getf *bptr-tfd-def-regexps* :test-rx))
           (sym-types '(function variable type class structure))
           (testsdb *tests-entries*)
           (str-to-expr #'def-str-to-expr)
           (str-to-result #'def-str-to-result))
  
  "Try to make tests for symbol."

  (let ((*tests-entries* testsdb))
    (loop :for tdocstr :in (get-docstrings-for-symbol sym :sym-types sym-types)
       :do (make-tests-from-sym-and-docstring sym (car tdocstr) (cadr tdocstr)
                                              :tregion-rx tregion-rx
                                              :tests-sep tests-sep
                                              :out-sep out-sep
                                              :test-rx test-rx
                                              :str-to-expr str-to-expr
                                              :str-to-result str-to-result))))



(defun get-docstrings-for-symbol
    (sym &key
           (sym-types '(function variable type class structure)))

  "Try to get docstrings for symbol."

  (let ((ret (loop :for typ :in sym-types
                :when (documentation sym typ)
                :collect (cons typ (cons (documentation sym typ) nil))
                :end)))
    (unless ret
      (let ((es (loop :for ss :being :the :external-symbol :of (symbol-package sym)
                   :when (eq ss sym)
                     :return t
                   :end
                   :finally (return nil))))
        (when (or es (boundp sym) (fboundp sym))
          (format *error-output* "[Info]~TSymbol have no docstrings:~T~a.~%"
                  (symbol-name sym)))))
    ret))



(defun get-symbol-types (sym)

  "Try to determine all types that symbol name."

  (let (ret)
    (when (boundp sym)
      (push 'variable ret))
    (when (fboundp sym)
      (push 'function ret))
    (push (type-of sym) ret)
    ret))



(defun make-tests-from-sym-and-docstring
    (sym type docstr
     &key
       (tregion-rx (getf *bptr-tfd-def-regexps* :tregion-rx))
       (tests-sep (getf *bptr-tfd-def-regexps* :tests-sep))
       (out-sep (getf *bptr-tfd-def-regexps* :out-sep))
       (test-rx (getf *bptr-tfd-def-regexps* :test-rx))
       (str-to-expr #'def-str-to-expr)
       (str-to-result #'def-str-to-result))

  "Try to build tests for given symbol and it's docstring."
  
  (loop :for n = 0 :then (1+ n)
     :and test-str :in (get-tests-strings-from-test-region
                        (get-test-region-from-docstring docstr
                                                        :tregion-rx tregion-rx
                                                        :sym sym :type type)
                        :tests-sep tests-sep
                        :test-rx test-rx)
     :do (let ((splt (split-test-expr-from-expected-result test-str
                                                           :out-sep out-sep
                                                           :sym sym :type type)))
           (when splt
             (destructuring-bind (expr-str res-str) splt
               (make-test-from-expr-and-expected-result sym type expr-str res-str
                                                        :n n
                                                        :str-to-expr str-to-expr
                                                        :str-to-result str-to-result))))))



(defun get-test-region-from-docstring
    (docstr &key
              (tregion-rx (getf *bptr-tfd-def-regexps* :tregion-rx))
              sym type)

  "Try to extract test region from string."

  (when docstr
    (let ((match (register-groups-bind (str) (tregion-rx docstr) str)))
      (if match
          match
          (progn
            (format *error-output*
                    "[Info]~T~a's docstring doesn't contain tests:~T~a~%" type sym)
            nil)))))


(defun rem-substr (str substr)
  (let ((si (search substr str)))
    (if si
        (concatenate 'string
                     (subseq str 0 si)
                     (subseq str (+ si (length substr))))
        str)))

(defun get-tests-strings-from-test-region
    (testregion &key
                  (tests-sep (getf *bptr-tfd-def-regexps* :tests-sep))
                  (test-rx (getf *bptr-tfd-def-regexps* :test-rx)))

  "Try to split tests entries."

  (if testregion
      (let ((matches (if test-rx
                         (all-matches-as-strings test-rx testregion)
                         nil)))
        (loop :for mm :in matches
           :do (setq testregion (rem-substr testregion mm)))
        
        (let ((splits (if tests-sep
                          (split tests-sep testregion)
                          nil)))
          (append splits matches)))
      nil))



(defun split-test-expr-from-expected-result
    (teststr &key
               (out-sep (getf *bptr-tfd-def-regexps* :out-sep))
               sym type)

  "Try to split string to expression for test and expected result of this expression."

  (if teststr
      (let ((ret (split out-sep teststr)))
        (if (= (length ret) 2)
            ret
            (progn
              (format *error-output* "[Maybe Error]~T~a ~a~Thave malformed test entry in docstring: ~s.~%"
                      type sym teststr)
              nil)))
      nil))



(defun make-test-from-expr-and-expected-result
    (sym type expr-str res-str &key
                                 (n 0)
                                 (str-to-expr #'def-str-to-expr)
                                 (str-to-result #'def-str-to-result)
                                 (testsdb *tests-entries*))

  "Try to parse lisp expressions from exptression string and result string for define test."
  
  (let* ((body (funcall str-to-expr expr-str))
         (result (funcall str-to-result res-str))
         (#+sbcl sb-rt::*entries* #-sbcl rtest::*entries*
                 testsdb)
         (sym-to-replace (intern
                          (subseq (symbol-name sym)
                                  (or (search  ":" (symbol-name sym) :from-end t) 0))))
         (name (make-symbol
                (concatenate 'string
                             (package-name (symbol-package sym)) "-"
                             (string sym-to-replace) "-" (string type) "-" (write-to-string n)))))
    (when body
      (setq body (nsubstitute sym sym-to-replace body))
      ;; (unintern sym-to-replace)
      (eval `(deftest ,name ,body ,@result)))))



(defun clear-testsdb (&optional (testsdb *tests-entries*))

  "Remove all tests."
  
  (let ((#+sbcl sb-rt::*entries* #-sbcl rtest::*entries*
                testsdb))
    (rem-all-tests))
  (setf (cdr testsdb) nil))



(defun run-testsdb (&key (testsdb *tests-entries*) (ostream *error-output*))

  "Run all tests."
  
  (let ((#+sbcl sb-rt::*entries* #-sbcl rtest::*entries*
                testsdb))
    (format ostream "~&~%======= Runing tests =======~%~%")
    (do-tests ostream)
    (format ostream "~&~%======= End of tests results =======~%")))
