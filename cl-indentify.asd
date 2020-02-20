(asdf:defsystem #:cl-indentify
  :description "A code beautifier for Common Lisp."
  :version "0.1"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on
    (#:alexandria #:uiop #:command-line-arguments)
  :components
    ((:module src
      :serial t
      :components
        ((:file "package")
         (:file "defaults")
         (:file "indenter")))))

