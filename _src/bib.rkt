
#lang racket
 (require (for-syntax racket/syntax))
(require scriblib/autobib scriblib/bibtex)
(provide all-pubs)
(provide (all-from-out scriblib/bibtex scriblib/autobib))

;(define (do-phony-cites cite-list)
;   (for-each ~cite cite-list)) 

(define-syntax (makecite stx)
  (syntax-case stx () 
  [(_ bib-file ~cite citet generate-bib sec-title)
   (let [(mycites (generate-temporary 'make-phony))]
     #`(begin
         (define-bibtex-cite bib-file ~cite citet generate-bib)
         (define #,mycites (hash-keys (bibdb-raw (path->bibdb "_src/mypubs.bib"))))
         (list (for-each ~cite #,mycites) (generate-bib #:sec-title sec-title))
         )
     )
   ]
  
  ))

(define-syntax-rule (all-pubs)
  (makecite "_src/mypubs.bib" ~cite citet define-bib "Journal Papers")
  )


(makecite "_src/mypubs.bib" ~cite citet define-bib "Journal Papers")
;(define mycites (hash-keys (bibdb-raw (path->bibdb "_src/mypubs.bib"))))
;(displayln mycites)