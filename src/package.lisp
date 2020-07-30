(defpackage #:indentify
  (:use :cl)
  (:import-from :alexandria
    :when-let
    :with-gensyms)
  (:export
    #:*indent-templates*
    #:indenter
    #:indentify
    #:indent-template
    #:initialize-templates
    #:load-default-templates
    #:load-template-file
    #:load-user-templates))


