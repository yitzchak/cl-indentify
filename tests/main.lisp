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
  (ok (indentify #"'(a b\n1 2)" #"'(a b\n  1 2)")
    "Simple quoted indented correctly"))
