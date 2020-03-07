(asdf:defsystem #:cl-indentify
  :description "A code beautifier for Common Lisp."
  :version "0.1"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on
    (#:alexandria #:uiop #:command-line-arguments #:trivial-gray-streams)
  :components
    ((:module src
      :serial t
      :components
        ((:file "package")
         (:file "verbatim-stream")
         (:file "defaults")
         (:file "indenter"))))
  :in-order-to ((test-op (test-op "cl-indentify/tests"))))

(asdf:defsystem "cl-indentify/tests"
  :description "Test system for cl-indentify"
  :version "0.1"
  :author "Tarn W. Burton"
  :depends-on ("cl-indentify" "rove" "trivial-escapes")
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
