<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
 <!ENTITY % html "IGNORE">
 <![%html;[
  <!ENTITY % print "IGNORE">
  <!ENTITY docbook.dsl PUBLIC
   "-//Norman Walsh//DOCUMENT DocBook HTML Stylesheet//EN" CDATA dsssl>
  ]]>
 <!ENTITY % print "INCLUDE">
 <![%print;[
  <!ENTITY docbook.dsl PUBLIC
   "-//Norman Walsh//DOCUMENT DocBook Print Stylesheet//EN" CDATA dsssl>
  ]]>
]>

<style-sheet>
<style-specification use="docbook">
<style-specification-body>

;; common to both html and print

(define %default-quadding% "justify")
(define ($generate-book-lot-list$) '())

(define (book-titlepage-recto-elements)
  (list (normalize "title")
	(normalize "subtitle")
	(normalize "date")
        (normalize "author")
        (normalize "authorblurb")
	(normalize "graphic")
	(normalize "copyright")
	(normalize "legalnotice")))

(element imagedata
  (if (have-ancestor? (normalize "mediaobject"))
      ($img$ (current-node) #t)
      ($img$ (current-node) #f)))

(element emphasis
  (if (equal? (normalize "strong") (attribute-string (normalize "role")))
      ($bold-seq$)
      ($italic-seq$)))

(element literal
  (if (equal? (normalize "sexp") (attribute-string (normalize "role")))
      (make element gi: "TT"
            attributes: (list (list "CLASS" (normalize "sexp")))
            (process-children))
      ($mono-seq$)))

(element (varlistentry term)
  (make sequence
    (process-children-trim)
    (if (not (last-sibling?))
	(make empty-element gi: "BR")
	(literal ""))))

<![%print;[ ;; customize the print stylesheet here

(define %paper-type% "USletter")

]]>

<![%html;[  ;; customize the html stylesheet here

;; /usr/share/sgml/docbook/dsssl-stylesheets-1.76/html/dbparam.dsl
(define %force-chapter-toc% #t)
(define %shade-verbatim% #t)
(define biblio-citation-check #t)
(define %html-ext% ".html")
;;(define %html-header-tags% '(("META" ("NAME" "name") ("CONTENT" "content"))))
(define %html-pubid% "-//W3C//DTD HTML 4.01//EN")
;;(define html-index #t)
;;(define html-index-filename "impnotes.idx")
;;(define html-manifest #t)
;;(define html-manifest-filename "impnotes.lst")
(define %stylesheet% "impnotes.css")
(define %use-id-as-filename% #t)
(define %funcsynopsis-decoration% #t)
(define %link-mailto-url% "mailto:clisp-list@sf.net")
(define %section-autolabel% #t)
(define %graphic-extensions%
  '("gif" "png" "jpg" "jpeg" "tif" "tiff" "eps" "epsf" ))
(define %admon-graphics% #t)
(define %admon-graphics-path%
  "/usr/share/sgml/docbook/dsssl-stylesheets/images/")
(define %show-comments% #t)     ; show the REMARK element

;; (define %generate-legalnotice-link% #f) ; default

]]>

</style-specification-body>
</style-specification>
<external-specification id="docbook" document="docbook.dsl">
</style-sheet>
