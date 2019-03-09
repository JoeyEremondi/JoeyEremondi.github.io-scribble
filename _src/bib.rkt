
#lang racket
 (require (for-syntax racket/syntax))
(require scriblib/autobib scriblib/bibtex)
(require (for-syntax scriblib/bibtex))
(provide all-pubs)
(provide (all-from-out scriblib/bibtex scriblib/autobib))
(require scribble/core)

;(define (do-phony-cites cite-list)
;   (for-each ~cite cite-list)) 
(define (key-lt thehash) (lambda (key1 key2)
                        (<
                         (string->number  (hash-ref (hash-ref thehash key1) "year"))
                         (string->number  (hash-ref (hash-ref thehash key2) "year")))))

(define (reverse-part p) (begin
                           (display (part-blocks p))
                           (struct-copy part p [blocks (reverse (part-blocks p))
                                                                  ])))

(define (dret y x) (begin (displayln y) (displayln x) x))

(define-syntax-rule
  (define-bibtex-cite-unsrt bib-pth
    ~cite-id citet-id generate-bibliography-id . options)
  (begin
    (define-cite autobib-cite autobib-citet generate-bibliography-id . options)
    (define-bibtex-cite* bib-pth
      (lambda x (apply autobib-cite (first x) #:sort? #f (rest x))) autobib-citet
      ~cite-id citet-id)))




(define-for-syntax (makecite bib-file sec-title)
   (let* [
         [thebibdb (bibdb-raw (path->bibdb bib-file))]
         [mycites (hash-keys thebibdb)]
         (years 0)
         (~cite (generate-temporary '~cite))
         (citet (generate-temporary 'citet))
         (generate-bib (generate-temporary 'generate-bib))
         ]
     #`(begin
         (define-bibtex-cite-unsrt #,bib-file #,~cite #,citet #,generate-bib #:spaces 1 #:style number-style  )
         (list  (for-each #,~cite (list #,@mycites))    (#,generate-bib #:sec-title #,sec-title #:tag #,sec-title)
         
     )
     )))

(define-syntax (all-pubs stx)
  #`(begin
  #,(makecite "_src/journal_papers.bib"  "Journal Papers")
  #,(makecite "_src/conf_papers.bib"  "Conference Papers")
  ;(makecite "_src/conf_versions.bib"  "Conference Versions of Journal Papers Papers")
  ))


;(define mycites (hash-keys (bibdb-raw (path->bibdb "_src/mypubs.bib"))))
;(displayln mycites)