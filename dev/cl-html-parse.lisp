
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA 
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the AllegroServe
;; prequel found in license-allegroserve.txt.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file 
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;; Suite 330, Boston, MA  02111-1307  USA
;;

;; $Id: cl-html-parse.lisp,v 1.1 2005/08/23 20:49:33 gwking Exp $

;; phtml.cl  - parse html

;; Change Log
;; 05/14/02 - add :parse-entities arg to parse-html. If true then
;;	   entities are converted to the character they represent.
;;
;; 02/05/01 symbols mapped to preferred case at runtime (as opposed to
;;            a compile time macro determining the case mapping)
;;
;; 10/27/00 :callbacks arg now processed correctly for tags with no body
;;
;; 10/14/00 add first-pass member to tokenbuf structure; used to remove
;;             multiple un-next-char calls in raw mode
;;          removed :script from *in-line* (incorect and led to infinite loop)
;;          char format reopen not done in :script and :style
;;          fixed :table/:th tag-auto-close-stop typo


; do character entity stuff
;

(defpackage #:net.html.parser
  (:use #:cl 
        #+MCL :ccl 
        #+Ignore :clos
        #+Ignore :excl
        #:if-star-for-cl-html-parse)
  (:export
   #:phtml-internal
   #:parse-html)
  (:nicknames #:html-parse #:cl-html-parse))

(in-package #:net.html.parser)

(defun current-case-mode ()
  (or #+allegro excl:*current-case-mode*
      *print-case*))

(defmacro tag-auto-close (tag) `(get ,tag 'tag-auto-close))
(defmacro tag-auto-close-stop (tag) `(get ,tag 'tag-auto-close-stop))
(defmacro tag-no-end (tag) `(get ,tag 'tag-no-end))

; only subelements allowed in this element, no strings
(defmacro tag-no-pcdata (tag) `(get ,tag 'tag-no-pcdata))

;; given :foo or (:foo ...) return :foo
(defmacro tag-name (expr)
  `(let ((.xx. ,expr))
     (if* (consp .xx.)
	then (car .xx.)
	else .xx.)))





(eval-when (compile load eval)
  (defconstant state-pcdata 0) ; scanning for chars or a tag
  (defconstant state-readtagfirst 1)
  (defconstant state-readtag      2)
  (defconstant state-findattribname 3)
  (defconstant state-attribname    4)
  (defconstant state-attribstartvalue 5)
  (defconstant state-attribvaluedelim 6)
  (defconstant state-attribvaluenodelim 7)
  (defconstant state-readcomment 8)
  (defconstant state-readcomment-one 9)
  (defconstant state-readcomment-two 10)
  (defconstant state-findvalue 11)
  (defconstant state-rawdata 12)
)


(defstruct collector 
  next  ; next index to set
  max   ; 1+max index to set
  data  ; string vector
  )

;; keep a cache of collectors on this list

(defparameter *collectors* (list nil nil nil nil))

(defun get-collector ()
  (declare (optimize (speed 3) (safety 1)))
  (let (col)
    (do* ((cols *collectors* (cdr cols))
          (this (car cols) (car cols)))
         ((null cols))
      (if* this
	   then (setf (car cols) nil)
           (setq col this)
           (return)))
    (if*  col
       then (setf (collector-next col) 0)
	    col
       else (make-collector
	     :next 0
	     :max  100
	     :data (make-string 100)))))

(defun put-back-collector (col)
  (declare (optimize (speed 3) (safety 1)))
  (do ((cols *collectors* (cdr cols)))
      ((null cols)
       ; toss it away
       nil)
    (if* (null (car cols))
	 then (setf (car cols) col)
         (return))))
	 


(defun grow-and-add (coll ch)
  (declare (optimize (speed 3) (safety 1)))
  ;; increase the size of the data portion of the collector and then
  ;; add the given char at the end
  (let* ((odata (collector-data coll))
	 (ndata (make-string (* 2 (length odata)))))
    (dotimes (i (length odata))
      (setf (schar ndata i) (schar odata i)))
    (setf (collector-data coll) ndata)
    (setf (collector-max coll) (length ndata))
    (let ((next (collector-next coll)))
      (setf (schar ndata next) ch)
      (setf (collector-next coll) (1+ next)))))

	 


    
  
  
;; character characteristics
(defconstant char-tagcharacter   1) ; valid char for a tag
(defconstant char-attribnamechar 2) ; valid char for an attribute name
(defconstant char-attribundelimattribvalue 4) ; valid for undelimited value
(defconstant char-spacechar 8)

(defparameter *characteristics* 
    ;; array of bits describing character characteristics
    (let ((arr (make-array 128 :initial-element 0)))
      (declare (optimize (speed 3) (safety 1)))
      (macrolet ((with-range ((var from to) &rest body)
		   `(do ((,var (char-code ,from) (1+ ,var))
			 (mmax  (char-code ,to)))
			((> ,var mmax))
		      ,@body))
		 
		 (addit (index charistic)
		   `(setf (svref arr ,index)
		      (logior (svref arr ,index)
			      ,charistic)))
		 )
	
	(with-range (i #\A #\Z)
	  (addit i (+ char-tagcharacter
		      char-attribnamechar
		      char-attribundelimattribvalue)))
	
	(with-range (i #\a #\z)
	  (addit i (+ char-tagcharacter
		      char-attribnamechar
		      char-attribundelimattribvalue)))
		      
	(with-range (i #\0 #\9)
	  (addit i (+ char-tagcharacter
		      char-attribnamechar
		      char-attribundelimattribvalue)))
	
	;; let colon be legal tag character
	(addit (char-code #\:) (+ char-attribnamechar
				  char-tagcharacter))
	
	;; NY times special tags have _
	(addit (char-code #\_) (+ char-attribnamechar
				  char-tagcharacter))
	
	; now the unusual cases
	(addit (char-code #\-) (+ char-attribnamechar
				  char-attribundelimattribvalue))
	(addit (char-code #\.) (+ char-attribnamechar
				  char-attribundelimattribvalue))
	
	;; adding all typeable chars except for whitespace and >
	(addit (char-code #\:) char-attribundelimattribvalue)
	(addit (char-code #\@) char-attribundelimattribvalue)
	(addit (char-code #\/) char-attribundelimattribvalue)
	(addit (char-code #\!) char-attribundelimattribvalue)
	(addit (char-code #\#) char-attribundelimattribvalue)
	(addit (char-code #\$) char-attribundelimattribvalue)
	(addit (char-code #\%) char-attribundelimattribvalue)
	(addit (char-code #\^) char-attribundelimattribvalue)
	(addit (char-code #\&) char-attribundelimattribvalue)
	(addit (char-code #\() char-attribundelimattribvalue)
	(addit (char-code #\)) char-attribundelimattribvalue)
	(addit (char-code #\_) char-attribundelimattribvalue)
	(addit (char-code #\=) char-attribundelimattribvalue)
	(addit (char-code #\+) char-attribundelimattribvalue)
	(addit (char-code #\\) char-attribundelimattribvalue)
	(addit (char-code #\|) char-attribundelimattribvalue)
	(addit (char-code #\{) char-attribundelimattribvalue)
	(addit (char-code #\}) char-attribundelimattribvalue)
	(addit (char-code #\[) char-attribundelimattribvalue)
	(addit (char-code #\]) char-attribundelimattribvalue)
	(addit (char-code #\;) char-attribundelimattribvalue)
	(addit (char-code #\') char-attribundelimattribvalue)
	(addit (char-code #\") char-attribundelimattribvalue)
	(addit (char-code #\,) char-attribundelimattribvalue)
	(addit (char-code #\<) char-attribundelimattribvalue)
	(addit (char-code #\?) char-attribundelimattribvalue)
	
	; i'm not sure what can be in a tag name but we know that
	; ! and - must be there since it's used in comments
	
	(addit (char-code #\-) char-tagcharacter)
	(addit (char-code #\!) char-tagcharacter)
	
	; spaces
	(addit (char-code #\space) char-spacechar)
	(addit (char-code #\tab) char-spacechar)
	(addit (char-code #\return) char-spacechar)
	(addit (char-code #\linefeed) char-spacechar)
	
	)
      
      
      
      arr))
	

(defun char-characteristic (char bit)
  (declare (optimize (speed 3) (safety 1)))
  ;; return true if the given char has the given bit set in 
  ;; the characteristic array
  (let ((code (char-code char)))
    (if* (<= 0 code 127)
       then ; in range
	    (not (zerop (logand (svref *characteristics* code) bit))))))


(defvar *html-entity-to-code* 
    (let ((table (make-hash-table :test #'equal)))
      (dolist (ent '(("nbsp" . 160)
		     ("iexcl" . 161)
		     ("cent" . 162)
		     ("pound" . 163)
		     ("curren" . 164)
		     ("yen" . 165)
		     ("brvbar" . 166)
		     ("sect" . 167)
		     ("uml" . 168)
		     ("copy" . 169)
		     ("ordf" . 170)
		     ("laquo" . 171)
		     ("not" . 172)
		     ("shy" . 173)
		     ("reg" . 174)
		     ("macr" . 175)
		     ("deg" . 176)
		     ("plusmn" . 177)
		     ("sup2" . 178)
		     ("sup3" . 179)
		     ("acute" . 180)
		     ("micro" . 181)
		     ("para" . 182)
		     ("middot" . 183)
		     ("cedil" . 184)
		     ("sup1" . 185)
		     ("ordm" . 186)
		     ("raquo" . 187)
		     ("frac14" . 188)
		     ("frac12" . 189)
		     ("frac34" . 190)
		     ("iquest" . 191)
		     ("Agrave" . 192)
		     ("Aacute" . 193)
		     ("Acirc" . 194)
		     ("Atilde" . 195)
		     ("Auml" . 196)
		     ("Aring" . 197)
		     ("AElig" . 198)
		     ("Ccedil" . 199)
		     ("Egrave" . 200)
		     ("Eacute" . 201)
		     ("Ecirc" . 202)
		     ("Euml" . 203)
		     ("Igrave" . 204)
		     ("Iacute" . 205)
		     ("Icirc" . 206)
		     ("Iuml" . 207)
		     ("ETH" . 208)
		     ("Ntilde" . 209)
		     ("Ograve" . 210)
		     ("Oacute" . 211)
		     ("Ocirc" . 212)
		     ("Otilde" . 213)
		     ("Ouml" . 214)
		     ("times" . 215)
		     ("Oslash" . 216)
		     ("Ugrave" . 217)
		     ("Uacute" . 218)
		     ("Ucirc" . 219)
		     ("Uuml" . 220)
		     ("Yacute" . 221)
		     ("THORN" . 222)
		     ("szlig" . 223)
		     ("agrave" . 224)
		     ("aacute" . 225)
		     ("acirc" . 226)
		     ("atilde" . 227)
		     ("auml" . 228)
		     ("aring" . 229)
		     ("aelig" . 230)
		     ("ccedil" . 231)
		     ("egrave" . 232)
		     ("eacute" . 233)
		     ("ecirc" . 234)
		     ("euml" . 235)
		     ("igrave" . 236)
		     ("iacute" . 237)
		     ("icirc" . 238)
		     ("iuml" . 239)
		     ("eth" . 240)
		     ("ntilde" . 241)
		     ("ograve" . 242)
		     ("oacute" . 243)
		     ("ocirc" . 244)
		     ("otilde" . 245)
		     ("ouml" . 246)
		     ("divide" . 247)
		     ("oslash" . 248)
		     ("ugrave" . 249)
		     ("uacute" . 250)
		     ("ucirc" . 251)
		     ("uuml" . 252)
		     ("yacute" . 253)
		     ("thorn" . 254)
		     ("yuml" . 255)
		     ("fnof" . 402)
		     ("Alpha" . 913)
		     ("Beta" . 914)
		     ("Gamma" . 915)
		     ("Delta" . 916)
		     ("Epsilon" . 917)
		     ("Zeta" . 918)
		     ("Eta" . 919)
		     ("Theta" . 920)
		     ("Iota" . 921)
		     ("Kappa" . 922)
		     ("Lambda" . 923)
		     ("Mu" . 924)
		     ("Nu" . 925)
		     ("Xi" . 926)
		     ("Omicron" . 927)
		     ("Pi" . 928)
		     ("Rho" . 929)
		     ("Sigma" . 931)
		     ("Tau" . 932)
		     ("Upsilon" . 933)
		     ("Phi" . 934)
		     ("Chi" . 935)
		     ("Psi" . 936)
		     ("Omega" . 937)
		     ("alpha" . 945)
		     ("beta" . 946)
		     ("gamma" . 947)
		     ("delta" . 948)
		     ("epsilon" . 949)
		     ("zeta" . 950)
		     ("eta" . 951)
		     ("theta" . 952)
		     ("iota" . 953)
		     ("kappa" . 954)
		     ("lambda" . 955)
		     ("mu" . 956)
		     ("nu" . 957)
		     ("xi" . 958)
		     ("omicron" . 959)
		     ("pi" . 960)
		     ("rho" . 961)
		     ("sigmaf" . 962)
		     ("sigma" . 963)
		     ("tau" . 964)
		     ("upsilon" . 965)
		     ("phi" . 966)
		     ("chi" . 967)
		     ("psi" . 968)
		     ("omega" . 969)
		     ("thetasym" . 977)
		     ("upsih" . 978)
		     ("piv" . 982)
		     ("bull" . 8226)
		     ("hellip" . 8230)
		     ("prime" . 8242)
		     ("Prime" . 8243)
		     ("oline" . 8254)
		     ("frasl" . 8260)
		     ("weierp" . 8472)
		     ("image" . 8465)
		     ("real" . 8476)
		     ("trade" . 8482)
		     ("alefsym" . 8501)
		     ("larr" . 8592)
		     ("uarr" . 8593)
		     ("rarr" . 8594)
		     ("darr" . 8595)
		     ("harr" . 8596)
		     ("crarr" . 8629)
		     ("lArr" . 8656)
		     ("uArr" . 8657)
		     ("rArr" . 8658)
		     ("dArr" . 8659)
		     ("hArr" . 8660)
		     ("forall" . 8704)
		     ("part" . 8706)
		     ("exist" . 8707)
		     ("empty" . 8709)
		     ("nabla" . 8711)
		     ("isin" . 8712)
		     ("notin" . 8713)
		     ("ni" . 8715)
		     ("prod" . 8719)
		     ("sum" . 8721)
		     ("minus" . 8722)
		     ("lowast" . 8727)
		     ("radic" . 8730)
		     ("prop" . 8733)
		     ("infin" . 8734)
		     ("ang" . 8736)
		     ("and" . 8743)
		     ("or" . 8744)
		     ("cap" . 8745)
		     ("cup" . 8746)
		     ("int" . 8747)
		     ("there4" . 8756)
		     ("sim" . 8764)
		     ("cong" . 8773)
		     ("asymp" . 8776)
		     ("ne" . 8800)
		     ("equiv" . 8801)
		     ("le" . 8804)
		     ("ge" . 8805)
		     ("sub" . 8834)
		     ("sup" . 8835)
		     ("nsub" . 8836)
		     ("sube" . 8838)
		     ("supe" . 8839)
		     ("oplus" . 8853)
		     ("otimes" . 8855)
		     ("perp" . 8869)
		     ("sdot" . 8901)
		     ("lceil" . 8968)
		     ("rceil" . 8969)
		     ("lfloor" . 8970)
		     ("rfloor" . 8971)
		     ("lang" . 9001)
		     ("rang" . 9002)
		     ("loz" . 9674)
		     ("spades" . 9824)
		     ("clubs" . 9827)
		     ("hearts" . 9829)
		     ("diams" . 9830)
		     ("quot" . 34)
		     ("amp" . 38)
		     ("lt" . 60)
		     ("gt" . 62)
		     ("OElig" . 338)
		     ("oelig" . 339)
		     ("Scaron" . 352)
		     ("scaron" . 353)
		     ("Yuml" . 376)
		     ("circ" . 710)
		     ("tilde" . 732)
		     ("ensp" . 8194)
		     ("emsp" . 8195)
		     ("thinsp" . 8201)
		     ("zwnj" . 8204)
		     ("zwj" . 8205)
		     ("lrm" . 8206)
		     ("rlm" . 8207)
		     ("ndash" . 8211)
		     ("mdash" . 8212)
		     ("lsquo" . 8216)
		     ("rsquo" . 8217)
		     ("sbquo" . 8218)
		     ("ldquo" . 8220)
		     ("rdquo" . 8221)
		     ("bdquo" . 8222)
		     ("dagger" . 8224)
		     ("Dagger" . 8225)
		     ("permil" . 8240)
		     ("lsaquo" . 8249)
		     ("rsaquo" . 8250)
		     ("euro" . 8364)
		     ))
	(setf (gethash (car ent) table) (cdr ent)))
      table))



(defstruct tokenbuf
  cur ;; next index to use to grab from tokenbuf
  max ;; index one beyond last character
  data ;; character array
  first-pass ;; previously parsed tokens
  )

;; cache of tokenbuf structs
(defparameter *tokenbufs* (list nil nil nil nil))

(defun get-tokenbuf ()
  (declare (optimize (speed 3) (safety 1)))
  (let (buf)
    (do* ((bufs *tokenbufs* (cdr bufs))
          (this (car bufs) (car bufs)))
         ((null bufs))
      (if* this
	   then (setf (car bufs) nil)
           (setq buf this)
           (return)))
    (if* buf
       then (setf (tokenbuf-cur buf) 0)
	    (setf (tokenbuf-max buf) 0)
	    buf
       else (make-tokenbuf
	     :cur 0
	     :max  0
	     :data (make-array 1024 :element-type 'character)))))

(defun put-back-tokenbuf (buf)
  (declare (optimize (speed 3) (safety 1)))
  (do ((bufs *tokenbufs* (cdr bufs)))
	((null bufs)
	 ; toss it away
	 nil)
      (if* (null (car bufs))
	 then (setf (car bufs) buf)
	      (return))))

(defun to-preferred-case (ch)
  (if* (eq (current-case-mode) :CASE-INSENSITIVE-UPPER)
     then (char-upcase ch)
     else (char-downcase ch)))
    
    
(defun next-token (stream ignore-strings raw-mode-delimiter
		   read-sequence-func tokenbuf parse-entities)
  (declare (optimize (speed 3) (safety 1)))
  ;; return two values: 
  ;;    the next token from the stream.
  ;; 	the kind of token (:pcdata, :start-tag, :end-tag, :eof)
  ;;
  ;; if read-sequence-func is non-nil,
  ;; read-sequence-func is called to fetch the next character
  (macrolet ((next-char (stream)
	       `(let ((cur (tokenbuf-cur tokenbuf))
		      (tb (tokenbuf-data tokenbuf)))
		  (if* (>= cur (tokenbuf-max tokenbuf))
		     then ; fill buffer
			  (if* (zerop (setf (tokenbuf-max tokenbuf)
					(if* read-sequence-func
					   then (funcall read-sequence-func tb stream)
					   else (read-sequence tb stream))))
			     then (setq cur nil) ; eof
			     else (setq cur 0)))
		  (if* cur
		     then (prog1 (schar tb cur)
			    (setf (tokenbuf-cur tokenbuf) (1+ cur))))))
			  
	     
	     (un-next-char (stream ch)
	       `(decf (tokenbuf-cur tokenbuf)))
	     
	     (clear-coll (coll)
	       `(setf (collector-next coll) 0))
		     
	     (add-to-coll (coll ch)
	       `(let ((.next. (collector-next ,coll)))
		  (if* (>= .next. (collector-max ,coll))
		     then (grow-and-add ,coll ,ch)
		     else (setf (schar (collector-data ,coll) .next.)
			    ,ch)
			  (setf (collector-next ,coll) (1+ .next.)))))
	       
	     )
    
    (let ((state (if* raw-mode-delimiter then state-rawdata else state-pcdata))
	  (coll  (get-collector))
	  (ch)

	  (value-delim)
	  
	  (tag-to-return)
	  (attribs-to-return)
	  
	  (end-tag)
	  
	  (attrib-name)
	  (attrib-value)
	  
	  (name-length 0) ;; count only when it could be a comment
	  
	  (raw-length 0)
          (xml-bailout)
	  )
    
      (loop
      
	(setq ch (next-char stream))
	;;(format t "ch: ~s state: ~s~%" ch state)
      
	(if* (null ch)
	   then (return) ; eof -- exit loop
		)
      
      
	(case state
	  (#.state-pcdata
	   ; collect everything until we see a <
	   (if* (eq ch #\<)
	      then ; if we've collected nothing then get a tag 
		   (if* (> (collector-next coll) 0)
		      then ; have collected something, return this string
			   (un-next-char stream ch) ; push back the <
			   (return)
		      else ; collect a tag
			   (setq state state-readtagfirst))
	    elseif (and parse-entities (eq ch #\&))
	      then ; reading an entity. entity ends at semicolon
		   (let (res (max 10))
		     (loop (let ((ch (next-char stream)))
			     (if* (null ch)
				then (error "End of file after & entity marker")
			      elseif (eq ch #\;)
				then (return)
			      elseif (zerop (decf max))
				then (error "No semicolon found after entity starting: &~{~a~}" (nreverse res))
				else (push ch res))))
		     (setq res (nreverse res))
		     (if* (eq (car res) #\#)
			then ; decimal entity
			     (let ((count 0))
			       (dolist (ch (cdr res))
				 (let ((code (char-code ch)))
				   (if* (<= #.(char-code #\0)
					    code
					    #.(char-code #\9))
				      then (setq count
					     (+ (* 10 count) 
						(- code
						   #.(char-code #\0))))
				      else (error "non decimal digit after &# - ~s" ch)
					   )))
			       (add-to-coll coll (code-char count)))
			else (let ((name (make-array (length res)
						     :element-type 'character
						     :initial-contents res)))
			       (let ((ch (gethash name *html-entity-to-code*)))
				 (if* ch
				    then (add-to-coll coll (code-char ch))
				    else (error "No such entity as ~s" name))))))
			     
	      else ; we will check for & here eventually
		   (if* (not (eq ch #\return))
		      then (add-to-coll coll ch))))
	
	  (#.state-readtagfirst
	   ; starting to read a tag name
	   (if* (eq #\/ ch)
	      then ; end tag
		   (setq end-tag t)
	      else (if* (eq #\! ch) ; possible comment
		      then (setf xml-bailout t)
			   (setq name-length 0))
		   (un-next-char stream ch))
	   (setq state state-readtag))
	
	  (#.state-readtag
	   ;; reading the whole tag name
	   (if* (char-characteristic ch char-tagcharacter)
	      then (add-to-coll coll (to-preferred-case ch))
		   (incf name-length)
		   (if* (and (eq name-length 3)
			     (coll-has-comment coll))
		      then (clear-coll coll)
			   (setq state state-readcomment))
			   
	      else (setq tag-to-return (compute-tag coll))
		   (clear-coll coll)
		   (if* (eq ch #\>)
		      then (return)	; we're done
		    elseif xml-bailout then 
			   (un-next-char stream ch)
			   (return)
		      else (if* (eq tag-to-return :!--)
			      then ; a comment
				   (setq state state-readcomment)
			      else (un-next-char stream ch)
				   (setq state state-findattribname)))))
	
	  (#.state-findattribname
	   ;; search until we find the start of an attribute name
	   ;; or the end of the tag
	   (if* (eq ch #\>)
	      then ; end of the line
		   (return)
	    elseif (eq ch #\=)
	      then ; value for previous attribute name
		   ; (syntax  "foo = bar" is bogus I think but it's
		   ; used some places, here is where we handle this
		   (pop attribs-to-return)
		   (setq attrib-name (pop attribs-to-return))
		   (setq state state-findvalue)
	    elseif (char-characteristic ch char-attribnamechar)
	      then (un-next-char stream ch)
		   (setq state state-attribname)
	      else nil ; ignore other things
		   ))
	  
	  (#.state-findvalue
	   ;; find the start of the value
	   (if* (char-characteristic ch char-spacechar)
	      thenret ; keep looking
	    elseif (eq ch #\>)
	      then ; no value, set the value to be the
		   ; name as a string
		   (setq attrib-value 
		     (string-downcase (string attrib-name)))
		   
		   (push attrib-name attribs-to-return)
		   (push attrib-value attribs-to-return)
		   (un-next-char stream ch)
		   (setq state state-findattribname)
	      else (un-next-char stream ch)
		   (setq state state-attribstartvalue)))
	   
	
	  (#.state-attribname
	   ;; collect attribute name

	   (if* (char-characteristic ch char-attribnamechar)
	      then (add-to-coll coll (to-preferred-case ch))
	    elseif (eq #\= ch)
	      then ; end of attribute name, value is next
		   (setq attrib-name (compute-tag coll))
		   (clear-coll coll)
		   (setq state state-attribstartvalue)
	      else ; end of attribute name with no value, 
		   (setq attrib-name (compute-tag coll))
		   (clear-coll coll)
		   (setq attrib-value 
		     (string-downcase (string attrib-name)))
		   (push attrib-name attribs-to-return)
		   (push attrib-value attribs-to-return)
		   (un-next-char stream ch)
		   (setq state state-findattribname)))
	
	  (#.state-attribstartvalue
	   ;; begin to collect value
	   (if* (or (eq ch #\")
		    (eq ch #\'))
	      then (setq value-delim ch)
		   (setq state state-attribvaluedelim)
		   ;; gobble spaces; assume since we've seen a '=' there really is a value
	    elseif (eq #\space ch) then nil
	      else (un-next-char stream ch)
		   (setq state state-attribvaluenodelim)))
	
	  (#.state-attribvaluedelim
	   (if* (eq ch value-delim)
	      then (setq attrib-value (compute-coll-string coll))
		   (clear-coll coll)
		   (push attrib-name attribs-to-return)
		   (push attrib-value attribs-to-return)
		   (setq state state-findattribname)
	      else (add-to-coll coll ch)))
	
	  (#.state-attribvaluenodelim
	   ;; an attribute value not delimited by ' or " and thus restricted
	   ;; in the possible characters
	   (if* (char-characteristic ch char-attribundelimattribvalue)
	      then (add-to-coll coll ch)
	      else (un-next-char stream ch)
		   (setq attrib-value (compute-coll-string coll))
		   (clear-coll coll)
		   (push attrib-name attribs-to-return)
		   (push attrib-value attribs-to-return)
		   (setq state state-findattribname)))
	  
	  (#.state-readcomment
	   ;; a comment ends on the first --, but we'll look for -->
	   ;; since that's what most people expect
	   (if* (eq ch #\-)
	      then (setq state state-readcomment-one)
	      else (add-to-coll coll ch)))
	  
	  (#.state-readcomment-one
	   ;; seen one -, looking for ->
	   
	   (if* (eq ch #\-)
	      then (setq state state-readcomment-two)
	      else ; not a comment end, put back the -'s
		   (add-to-coll coll #\-)
		   (add-to-coll coll ch)
		   (setq state state-readcomment)))
	  
	  (#.state-readcomment-two
	   ;; seen two -'s, looking for >
	   
	   (if* (eq ch #\>)
	      then ; end of the line
		   (return)
	    elseif (eq ch #\-)
	      then ; still at two -'s, have to put out first
		   (add-to-coll coll #\-)
	      else ; put out two hypens and back to looking for a hypen
		   (add-to-coll coll #\-)
		   (add-to-coll coll #\-)
		   (setq state state-readcomment)))
	  
	  (#.state-rawdata
	   ;; collect everything until we see the delimiter
	   (if* (eq (to-preferred-case ch) (elt raw-mode-delimiter raw-length))
	      then
		   (incf raw-length)
		   (when (= raw-length (length raw-mode-delimiter))
		     ;; push the end tag back so it can then be lexed
		     ;; but don't do it for xml stuff
		     (when (/= (length  raw-mode-delimiter) 1)
		       (push :end-tag (tokenbuf-first-pass tokenbuf))
		       (if* (equal raw-mode-delimiter "</STYLE>")
			  then (push :STYLE (tokenbuf-first-pass tokenbuf))
			elseif (equal raw-mode-delimiter "</style>")
			  then (push :style (tokenbuf-first-pass tokenbuf))
			elseif (equal raw-mode-delimiter "</SCRIPT>")
			  then (push :SCRIPT (tokenbuf-first-pass tokenbuf))
			elseif (equal raw-mode-delimiter "</script>")
			  then (push :script (tokenbuf-first-pass tokenbuf))
			  else (error "unexpected raw-mode-delimiter"))
		       )
		     ;; set state to state-pcdata for next section
		     (return))
	      else
		   ;; push partial matches into data string
		   (dotimes (i raw-length)
		     (add-to-coll coll (elt raw-mode-delimiter i)))
		   (setf raw-length 0)
		   (add-to-coll coll ch)))
		     
	  ))
      
      
      ;; out of the loop. 
      ;; if we're in certain states then it means we should return a value
      ;;
      (case state
	((#.state-pcdata #.state-rawdata)
	 ;; return the buffer as a string
	 (if* (zerop (collector-next coll))
	    then (values nil (if (eq state state-pcdata) :eof :pcdata))
	    else (values (prog1 
			     (if* (null ignore-strings)
				then (compute-coll-string coll))
			   (put-back-collector coll))
			 :pcdata)))
	
	(#.state-readtag
	 (when (null tag-to-return)
	   (error "unexpected end of input encountered"))
	 ;; we've read a tag with no attributes
	 (put-back-collector coll)
	 (values tag-to-return
		 (if* end-tag
		    then :end-tag
		    else (if* xml-bailout then :xml else :start-tag))
		 ))
	
	(#.state-findattribname
	 ;; returning a tag with possible attributes
	 (put-back-collector coll)
	 (if* end-tag
	    then ; ignore any attributes
		 (values tag-to-return :end-tag)
	  elseif attribs-to-return
	    then (values (cons tag-to-return 
			       (nreverse attribs-to-return))
			 :start-tag)
	    else (values tag-to-return :start-tag)))
	
	(#.state-readcomment-two
	 ;; returning a comment
	 (values (prog1 (if* (null ignore-strings)
			   then (compute-coll-string coll))
		   (put-back-collector coll))
		 :comment))
	
	(t 
	 (if* (null ch) then (error "unexpected end of input encountered")
	    else (error "internal error, can't be here in state ~d" state)))))))


(defvar *kwd-package* (find-package :keyword))

(defun compute-tag (coll)
  (declare (optimize (speed 3) (safety 1)))
  ;; compute the symbol named by what's in the collector
  ;; (format t "~%~S ~S ~S" (collector-data coll) (collector-next coll) *kwd-package*)
  (intern 
   (string-upcase (subseq (collector-data coll) 0 (collector-next coll)))
   *kwd-package*))



(defun compute-coll-string (coll)
  (declare (optimize (speed 3) (safety 1)))
  ;; return the string that's in the collection
  (let ((str (make-string (collector-next coll)))
	(from (collector-data coll)))
    (dotimes (i (collector-next coll))
      (setf (schar str i) (schar from i)))
    
    str))

(defun coll-has-comment (coll)
  (declare (optimize (speed 3) (safety 1)))
  ;; true if the collector has exactly "!--" in it
  (and (eq 3 (collector-next coll))
       (let ((data (collector-data coll)))
	 (and (eq #\! (schar data 0))
	      (eq #\- (schar data 1))
	      (eq #\- (schar data 2))))))
		 

;;;;;;;;;;; quick and dirty parse

; the elements with no body and thus no end tag
(dolist (opt '(:area :base :basefont :bgsound :br
	       ;;:button - buttons can totally have contents
	       :col 
	       ;;:colgroup - no, this is an element with contents
	       :embed :hr :img :frame
	       :input :isindex :keygen :link :meta 
	       :plaintext :spacer :wbr))
  (setf (tag-no-end opt) t))

(defvar *in-line* '(:tt :i :b :big :small :em :strong :dfn :code :samp :kbd
		    :var :cite :abbr :acronym :a :img :object :br :map
		    :q :sub :sup :span :bdo :input :select :textarea :label ;;:button 
		    :font))

(defvar *ch-format* '(:i :b :tt :big :small :strike :s :u
		      :em :strong :font))

(defvar *known-tags* '(:!doctype :a :acronym :address :applet :area :b :base :basefont
		       :bdo :bgsound :big :blink :blockquote :body :br :button :caption
		       :center :cite :code :col :colgroup :comment :dd :del :dfn :dir
		       :div :dl :dt :em :embed :fieldset :font :form :frame :frameset
		       :h1 :h2 :h3 :h4 :h5 :h6 :head :hr :html :i :iframe :img :input
		       :ins :isindex :kbd :label :layer :legend :li :link :listing :map
		       :marquee :menu :meta :multicol :nobr :noframes :noscript :object
		       :ol :option :p :param :plaintext :pre :q :samp :script :select
		       :small :spacer :span :s :strike :strong :style :sub :sup :table
		       :tbody :td :textarea :tfoot :th :thead :title :tr :tt :u :ul :var
		       :wbr :xmp))

; the elements whose start tag can end a previous tag

(setf (tag-auto-close :tr) '(:tr :td :th :colgroup))
(setf (tag-auto-close-stop :tr) '(:table))

(setf (tag-auto-close :td) '(:td :th))
(setf (tag-auto-close-stop :td) '(:table))

(setf (tag-auto-close :th) '(:td :th))
(setf (tag-auto-close-stop :th) '(:table))

(setf (tag-auto-close :dt) '(:dt :dd))
(setf (tag-auto-close-stop :dt) '(:dl))

(setf (tag-auto-close :li) '(:li))
(setf (tag-auto-close-stop :li) '(:ul :ol))

;; new stuff to close off tags with optional close tags
(setf (tag-auto-close :address) '(:head :p))
(setf (tag-auto-close :blockquote) '(:head :p))
(setf (tag-auto-close :body) '(:body :frameset :head))

(setf (tag-auto-close :dd) '(:dd :dt))
(setf (tag-auto-close-stop :dd) '(:dl))

(setf (tag-auto-close :dl) '(:head :p))
(setf (tag-auto-close :div) '(:head :p))
(setf (tag-auto-close :fieldset) '(:head :p))
(setf (tag-auto-close :form) '(:head :p))
(setf (tag-auto-close :frameset) '(:body :frameset :head))
(setf (tag-auto-close :hr) '(:head :p))
(setf (tag-auto-close :h1) '(:head :p))
(setf (tag-auto-close :h2) '(:head :p))
(setf (tag-auto-close :h3) '(:head :p))
(setf (tag-auto-close :h4) '(:head :p))
(setf (tag-auto-close :h5) '(:head :p))
(setf (tag-auto-close :h6) '(:head :p))
(setf (tag-auto-close :noscript) '(:head :p))
(setf (tag-auto-close :ol) '(:head :p))

(setf (tag-auto-close :option) '(:option))
(setf (tag-auto-close-stop :option) '(:select))

(setf (tag-auto-close :p) '(:head :p))

(setf (tag-auto-close :pre) '(:head :p))
(setf (tag-auto-close :table) '(:head :p))

(setf (tag-auto-close :tbody) '(:colgroup :tfoot :tbody :thead))
(setf (tag-auto-close-stop :tbody) '(:table))

(setf (tag-auto-close :tfoot) '(:colgroup :tfoot :tbody :thead))
(setf (tag-auto-close-stop :tfoot) '(:table))

(setf (tag-auto-close :thead) '(:colgroup :tfoot :tbody :thead))
(setf (tag-auto-close-stop :thead) '(:table))

(setf (tag-auto-close :ul) '(:head :p))

(setf (tag-no-pcdata :table) t)
(setf (tag-no-pcdata :tr) t)


(defmethod parse-html ((p stream) &key callback-only callbacks collect-rogue-tags
				       no-body-tags
				       parse-entities)
  (declare (optimize (speed 3) (safety 1)))
  (phtml-internal p nil callback-only callbacks collect-rogue-tags
		  no-body-tags parse-entities))

(defmacro tag-callback (tag)
  `(rest (assoc ,tag callbacks)))

(defun phtml-internal (p read-sequence-func callback-only 
		       callbacks collect-rogue-tags 
		       no-body-tags
		       parse-entities)
  (declare (optimize (speed 3) (safety 1)))
  (let ((raw-mode-delimiter nil)
	(pending nil)
	(current-tag :start-parse)
	(last-tag :start-parse)
	(current-callback-tags nil)
	(pending-ch-format nil)
	(closed-pending-ch-format nil)
	(new-opens nil)
	(tokenbuf (get-tokenbuf))
	(guts)
	(rogue-tags)
	)
    (labels ((close-off-tags (name stop-at collect-rogues once-only)
	       ;; close off an open 'name' tag, but search no further
	       ;; than a 'stop-at' tag.
	       #+ignore (format t "close off name ~s, stop at ~s, ct ~s~%"
		       name stop-at current-tag)
	       (if* (member (tag-name current-tag) name :test #'eq)
		  then ;; close current tag(s)
		       (loop
			 (when (and collect-rogues
				    (not (member (tag-name current-tag)
						 *known-tags*)))
			   (push (tag-name current-tag) rogue-tags))
			 (close-current-tag)
			 (if* (or once-only
				  (member (tag-name current-tag)
					  *ch-format*)
				  (not (member 
					(tag-name current-tag) name :test #'eq)))
			    then (return)))
		elseif (member (tag-name current-tag) stop-at :test #'eq)
		  then nil
		  else ; search if there is a tag to close
		       (dolist (ent pending)
			 (if* (member (tag-name (car ent)) name :test #'eq)
			    then ; found one to close
				 (loop
				   (when (and collect-rogues
					      (not (member (tag-name current-tag)
							   *known-tags*)))
				     (push (tag-name current-tag) rogue-tags))
				   (close-current-tag)
				   (if* (member (tag-name current-tag) name
						:test #'eq)
				      then (close-current-tag)
					   (return)))
				 (return)
			  elseif (member (tag-name (car ent)) stop-at
					 :test #'eq)
			    then (return) ;; do nothing
				 ))))
	   
	     (close-current-tag ()
	       ;; close off the current tag and open the pending tag
	       (when (member (tag-name current-tag) *ch-format* :test #'eq)
		 (push current-tag closed-pending-ch-format)
		 )
	       (let (element)
		 (if* (tag-no-pcdata (tag-name current-tag))
		    then (setq element `(,current-tag
					 ,@(strip-rev-pcdata guts)))
		    else (setq element `(,current-tag ,@(nreverse guts))))
		 (let ((callback (tag-callback (tag-name current-tag))))
		   (when callback
		     (setf current-callback-tags (rest current-callback-tags))
		     (funcall callback element)))
		 (let* ((prev (pop pending)))
		   (setq current-tag (car prev)
			 guts (cdr prev))
		   (push element guts))))
	     
	     (save-state ()
	       ;; push the current tag state since we're starting:
	       ;; a new open tag
	       (push (cons current-tag guts) pending)
	       #+ignore (format t "state saved, pending ~s~%" pending)
	       )
	     
	     
	     (strip-rev-pcdata (stuff)
	       ;; reverse the list stuff, omitting all the strings
	       (let (res)
		 (dolist (st stuff)
		   (if* (not (stringp st)) then (push st res)))
		 res))
	     (check-in-line (check-tag)
	       (setf new-opens nil)
	       (let (val kind (i 0)
		     (length (length (tokenbuf-first-pass tokenbuf))))
		 (loop
		   (if* (< i length) then
			   (setf val (nth i (tokenbuf-first-pass tokenbuf)))
			   (setf kind (nth (+ i 1) (tokenbuf-first-pass tokenbuf)))
			   (setf i (+ i 2))
			   (if* (= i length) then (setf (tokenbuf-first-pass tokenbuf)
						    (nreverse (tokenbuf-first-pass tokenbuf))))
		      else
			   (multiple-value-setq (val kind)
			     (get-next-token t))
			   (push val (tokenbuf-first-pass tokenbuf))
			   (push kind (tokenbuf-first-pass tokenbuf))
			   )
		   (when (eq kind :eof)
		     (if* (= i length) then 
			     (setf (tokenbuf-first-pass tokenbuf) 
			       (nreverse (tokenbuf-first-pass tokenbuf))))
		     (return))
		   (when (and (eq val check-tag) (eq kind :end-tag))
		     (if* (= i length) then 
			     (setf (tokenbuf-first-pass tokenbuf) 
			       (nreverse (tokenbuf-first-pass tokenbuf))))
		     (return))
		   (when (member val *ch-format* :test #'eq)
		     (if* (eq kind :start-tag) then (push val new-opens)
		      elseif (member val new-opens :test #'eq) then
			     (setf new-opens (remove val new-opens :count 1))
			else (close-off-tags (list val) nil nil nil)
			     )))))
		 
	     (get-next-token (force)
	       (if* (or force (null (tokenbuf-first-pass tokenbuf))) then
		       (multiple-value-bind (val kind)
			   (next-token p nil raw-mode-delimiter read-sequence-func
				       tokenbuf parse-entities)
			 (values val kind))
		  else
		       (let ((val (first (tokenbuf-first-pass tokenbuf)))
			     (kind (second (tokenbuf-first-pass tokenbuf))))
			 (setf (tokenbuf-first-pass tokenbuf) 
			   (rest (rest (tokenbuf-first-pass tokenbuf))))
			 (values val kind))))
	     )
      (loop
	(multiple-value-bind (val kind)
	    (get-next-token nil)
	  #+ignore (format t "val: ~s kind: ~s  last-tag ~s pending ~s~%" val kind 
		  last-tag pending)
	  (case kind
	    (:pcdata
	     (when (or (and callback-only current-callback-tags)
		       (not callback-only))
	       (if* (member last-tag *in-line*)
		  then
		       (push val guts)
		  else
		       (when (dotimes (i (length val) nil)
			       (when (not (char-characteristic (elt val i) 
							       char-spacechar))
				 (return t)))
			 (push val guts))))
	     (when (and (= (length raw-mode-delimiter) 1) ;; xml tag...
			(or (and callback-only current-callback-tags)
			    (not callback-only)))
	       (close-off-tags (list last-tag) nil nil t))
	     (setf raw-mode-delimiter nil)
	     )
	    
	    (:xml
	     (setf last-tag val)
	     (setf raw-mode-delimiter ">")
	     (let* ((name (tag-name val)))
	       (when (and callback-only (tag-callback name))
		 (push name current-callback-tags))
	       (save-state)
	       (setq current-tag val)
	       (setq guts nil)
	       ))
	    
	    (:start-tag
	     (setf last-tag val)
	     (if* (or (eq last-tag :style)
		      (and (listp last-tag) (eq (first last-tag) :style)))
		then
		     (setf raw-mode-delimiter
		       (if* (eq (current-case-mode) :CASE-INSENSITIVE-UPPER)
			  then "</STYLE>"
			  else "</style>"))
	      elseif (or (eq last-tag :script)
			 (and (listp last-tag) (eq (first last-tag) :script)))
		then
		     (setf raw-mode-delimiter
		       (if* (eq (current-case-mode) :CASE-INSENSITIVE-UPPER)
			  then "</SCRIPT>"
			  else "</script>")))
	     ; maybe this is an end tag too
	     (let* ((name (tag-name val))
		    (auto-close (tag-auto-close name))
		    (auto-close-stop nil)
		    (no-end (or (tag-no-end name) (member name no-body-tags))))
	       (when (and callback-only (tag-callback name))
		 (push name current-callback-tags))
	       (when (or (and callback-only current-callback-tags)
			 (not callback-only))
		 (if* auto-close
		    then (setq auto-close-stop (tag-auto-close-stop name))
			 (close-off-tags auto-close auto-close-stop nil nil))
		 (when (and pending-ch-format (not no-end))
		   (if* (member name *ch-format* :test #'eq) then nil
		    elseif (member name *in-line* :test #'eq) then
			   ;; close off only tags that are within *in-line* block
			   (check-in-line name)
		      else ;; close ALL pending char tags and then reopen 
			   (dolist (this-tag (reverse pending-ch-format))
			     (close-off-tags (list (if (listp this-tag) (first this-tag) this-tag)) nil nil nil))
			   ))
		 (if* no-end
		    then		; this is a singleton tag
			 (let ((callback (tag-callback (tag-name (if* (atom val)
								    then val
								    else (first val))))))
			   (when callback
			     (funcall callback (if* (atom val)
						  then val
						  else (list val)))))
			 (push (if* (atom val)
				  then val
				  else (list val))
			       guts)
		    else (save-state)
			 (setq current-tag val)
			 (setq guts nil))
		 (if* (member name *ch-format* :test #'eq)
		    then (push val pending-ch-format)
		    else (when (not
				(or (eq last-tag :style)
				    (and (listp last-tag) (eq (first last-tag) :style))
				    (eq last-tag :script)
				    (and (listp last-tag) (eq (first last-tag) :script))))
			   (dolist (tmp (reverse closed-pending-ch-format))
			     (save-state)
			     (setf current-tag tmp)
			     (setf guts nil)))
			 )
		 (when (not
			(or (eq last-tag :style)
			    (and (listp last-tag) (eq (first last-tag) :style))
			    (eq last-tag :script)
			    (and (listp last-tag) (eq (first last-tag) :script))))
		   (setf closed-pending-ch-format nil))
		 )))
	  
	    (:end-tag
	     (setf raw-mode-delimiter nil)
	     (when (or (and callback-only current-callback-tags)
		       (not callback-only))
	       (close-off-tags (list val) nil nil t)
	       (when (member val *ch-format* :test #'eq)
		 (setf pending-ch-format 
		   (remove val pending-ch-format :count 1
			   :test #'(lambda (x y) (eq x (if (listp y) (first y) y)))))
		 (setf closed-pending-ch-format 
		   (remove val closed-pending-ch-format :count 1
			   :test #'(lambda (x y) (eq x (if (listp y) (first y) y)))))
		 )
	       (dolist (tmp (reverse closed-pending-ch-format))
		 (save-state)
		 (setf current-tag tmp)
		 (setf guts nil))
	       (setf closed-pending-ch-format nil)
	       ))

	    (:comment
	     (setf raw-mode-delimiter nil)
	     (when (or (and callback-only current-callback-tags)
		       (not callback-only))
	       (push `(:comment ,val) guts)))
	    
	    (:eof
	     (setf raw-mode-delimiter nil)
	     ;; close off all tags
	     (when (or (and callback-only current-callback-tags)
		       (not callback-only))
	       (close-off-tags '(:start-parse) nil collect-rogue-tags nil))
	     (put-back-tokenbuf tokenbuf)
	     (if collect-rogue-tags
		 (return (values (cdar guts) rogue-tags))
	       (return (cdar guts))))))))))

	      

(defmethod parse-html (file &key callback-only callbacks collect-rogue-tags
				 no-body-tags parse-entities)
  (declare (optimize (speed 3) (safety 1)))
  (with-open-file (p file :direction :input)
    (parse-html p :callback-only callback-only :callbacks callbacks
		:collect-rogue-tags collect-rogue-tags
		:no-body-tags no-body-tags
		:parse-entities parse-entities
		)))	     
	     

(defmethod parse-html ((str string) &key 
		       callback-only callbacks collect-rogue-tags
		       no-body-tags parse-entities)
  (declare (optimize (speed 3) (safety 1)))
  (parse-html (make-string-input-stream str) 
	      :callback-only callback-only :callbacks callbacks
	      :collect-rogue-tags collect-rogue-tags
	      :no-body-tags no-body-tags
		:parse-entities parse-entities
	      ))

		 
	      
  
  
	
		 
			 
		 
;;;;;;;;;;;; test

;;;(defun doit (ignore-data)
;;;  (with-open-file (p "readme.htm")
;;;    (loop
;;;      (multiple-value-bind (val kind) (next-token p ignore-data)
;;;	 ;(format t "~s -> ~s~%" kind val)
;;;      
;;;	(if* (eq kind :eof) then (return))))))
;;;
;;;(defun pdoit (&optional (file "testa.html"))
;;;  (with-open-file (p file)
;;;    (parse-html p)))
;;;
;;;
;;;;; requires http client module to work
;;;(defun getparse (host path)
;;;  (parse-html (httpr-body 
;;;	  (parse-response
;;;	   (simple-get host path)))))

(provide :phtml)
