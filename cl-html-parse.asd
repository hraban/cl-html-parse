(defpackage :asdf-cl-http-parse (:use #:asdf #:cl))
(in-package :asdf-cl-http-parse)

(defsystem cl-html-parse
  :author "Gary Warren King <gwking@metabang.com>"
  :version "1.0"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license for the packaging, AllegroServe license for the code."
  :description "HTML Parser"
  :long-description "Franz's HTML Parser packaged up separately for ASDF."
  :components
  ((:static-file "COPYING")
   (:module 
    "dev"
    :components ((:static-file "README")
                                     
		 (:file "package")
		 #-allegro
		 (:file "if-star" 
			:depends-on ("package"))
		 (:file "cl-html-parse" 
			:depends-on ("package" 
				     #-allegro "if-star"))))))


