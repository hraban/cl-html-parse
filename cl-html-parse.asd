(in-package :asdf)

(defsystem #:cl-html-parse
  :author "Gary Warren King <gwking@metabang.com>"
  :version "1.0"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license for the packaging, AllegroServe license for the code."
  :description "HTML Parser"
  :long-description "Franz's HTML Parser packaged up separately for ASDF."
  :depends-on (:acl-compat)
  :components
  ((:static-file "COPYING")
   (:module 
    "dev"
    :components ((:static-file "README")
		 (:static-file "examples/contacts.html")
		 (:file "cl-html-parse"))))) 


