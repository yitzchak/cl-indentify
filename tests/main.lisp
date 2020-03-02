(defpackage #:cl-indentify/tests
  (:use #:cl
        #:cl-indentify
        #:rove))

(in-package #:cl-indentify/tests)

(setup
  (cl-indentify:load-default-templates))

(defun indentify-string (value)
  (with-output-to-string (output-stream)
    (with-input-from-string (input-stream value)
      (cl-indentify:indentify input-stream output-stream))))

(deftest verbatim-tokens
  (testing "Verbatim Tokens"
    (ok (string= (indentify-string "\"
\"") "\"
\"")
      "Tabs and newlines are preserved in strings.")
    (ok (string= (indentify-string "; ") "; ")
      "Tabs are preserved in comments.")
    (ok (string= (indentify-string "#|
|#") "#|
|#")
      "Tabs and newlines are preserved in block comments.")))    
