### Introduction

CL-HTTP-PARSE is a slash and burn port of Franz's phtml HTML parser.

I made a few small changes to the source mainly involving the call to
excl:intern* in collect-tag. In particular, all tags are now string-upcased
before being interned. There is probably a better solution but I'm not
very Allegro savvy.

Any questions or concerns should be directed to me, Gary Warren King at
gwking@metabang.com.

If the feature :use-acl-compat is set, then cl-http-parse will depend on
it; otherwise, it will use its own package and if* definitions.


### Example #1

(html-parse:parse-html
 "<html>
<head><title>Parsing HTML is Phun</title></head>
<body>
<h1>Why is it phun?</h1>
<p>Parsing HTML is phun because angle brackets are better than parentheses.</p>
</body>
</html>")

==>

((:HTML (:HEAD (:TITLE "Parsing HTML is Phun"))
  (:BODY (:H1 "Why is it phun?")
   (:P
    "Parsing HTML is phun because angle brackets are better than parentheses."))))

### Example #2

(html-parse:parse-html <file>)

==> ((:HTML ...))


