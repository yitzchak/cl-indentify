(defpackage #:cl-indentify/tests
  (:use #:cl
        #:cl-indentify
        #:rove))

(in-package #:cl-indentify/tests)

(setup
  (cl-indentify:load-default-templates))

(defun indentify-string (value)
  (with-input-from-string (input-stream value)
    (cl-indentify:indentify input-stream)))

(deftest verbatim-tokens
  (ok (outputs (indentify-string "\"
\"") "\"
\"")
    "Tabs and newlines are preserved in strings.")
  (ok (outputs (indentify-string "; ") "; ")
    "Tabs are preserved in comments.")
  (ok (outputs (indentify-string "#|
|#") "#|
|#")
    "Tabs and newlines are preserved in block comments."))


(deftest quote-style
  (ok (outputs (indentify-string "'(a b
    1 2)") "'(a b
  1 2)")))
