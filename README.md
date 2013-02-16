# bptr-tests-from-docstrings

Common Lisp program for making unit tests from symbol's docstrings.

## Intro

Try to analyze docstrings of symbols to generate tests for them.  
Sample:

```common-lisp
(defpackage :tsts-package (:use :cl))
(in-package :tsts-package)

(defun test (arg)
  
  "Test function.
Tests:
 (test t) ==> t ;;
 (test nil) ==> nil"
  
  (if arg t nil))

(let ((tstdb (bptr-tfd:make-empty-testsdb)))
  (bptr-tfd:make-tests-for-package :tsts-package :testsdb tstdb)
  (bptr-tfd:run-testsdb :testsdb tstdb))
```
This will create and run two tests for `test` function.  
As another examples you could see [`bptr-plist`](https://github.com/Bad-ptr/bptr-plist) and [`bptr-html`](https://github.com/Bad-ptr/bptr-html)
