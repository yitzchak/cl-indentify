(defpackage #:cl-indentify/tests
  (:use #:cl
        #:rove))

(in-package #:cl-indentify/tests)

(named-readtables:in-readtable trivesc:readtable)

(setup
  (cl-indentify:load-default-templates))

(defun indentify (input &optional output)
  (string=
    (or output input)
    (with-output-to-string (output-stream)
      (with-input-from-string (input-stream input)
        (cl-indentify:indentify input-stream output-stream)))))

(defmacro with-indent-templates (templates &body body)
  `(let ((cl-indentify:*indent-templates* (make-hash-table :test #'equal)))
     (cl-indentify::load-templates ,templates)
     ,@body))

(deftest verbatim-tokens
  (ok (indentify #"\"\n\t\"")
    "Tabs and newlines are preserved in strings.")
  (ok (indentify #";\t")
    "Tabs are preserved in comments.")
  (ok (indentify #"#|\n\t|#")
    "Tabs and newlines are preserved in block comments."))

(deftest non-verbatim-tokens
  (ok (indentify #"\ta" "        a")
    "Verify that tabs are turned into spaces"))

(deftest quote-style
  (ok (indentify #"'(a b\n1 (2\n3))" #"'(a b\n  1 (2\n     3))")
    "Simple quoted indented correctly")
  (ok
    (indentify #"(quote (a b\n1 (2\n3)))" #"(quote (a b\n        1 (2\n           3)))")
    "Quote for quote form")
  (ok
    (with-indent-templates (("q" :style :quote))
      (indentify #"(q (a b\n1 (2\n3)))" #"(q (a b\n    1 (2\n       3)))"))
    "Quote style for arbitrary lambda"))
