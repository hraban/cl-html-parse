(in-package :asdf)

(defsystem #:cl-html-parse
  :author "Gary Warren King <gwking@metabang.com>"
  :version "1.0"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license for the packaging, AllegroServe license for the code."
  :description "HTML Parser"
  :long-description "Franz's HTML Parser packaged up separately for ASDF."
  :components
  #+use-acl-compat  :depends-on   #+use-acl-compat (:acl-compat)
  ((:static-file "COPYING")
   (:module
    "dev"
    :components ((:static-file "README")
		 (:static-file "examples/contacts.html")

		 (:file "package")
		 #-(or allegro use-acl-compat)
		 (:file "if-star" :depends-on ("package"))
		 (:file "cl-html-parse" :depends-on ("package" #-allegro "if-star"))))))


