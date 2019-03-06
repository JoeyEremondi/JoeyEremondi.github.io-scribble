
#lang racket
 (require (for-syntax racket/syntax))
(require scriblib/autobib scriblib/bibtex)
(provide all-pubs makecite)
(provide (all-from-out scriblib/bibtex scriblib/autobib))

;(define (do-phony-cites cite-list)
;   (for-each ~cite cite-list)) 

(define-syntax (makecite stx)
  (syntax-case stx () 
  [(_ bib-file  sec-title)
   (let [(mycites (generate-temporary 'make-phony))
         (~cite (generate-temporary '~cite))
         (citet (generate-temporary 'citet))
         (generate-bib (generate-temporary 'generate-bib))
         ]
     #`(begin
         (define-bibtex-cite bib-file #,~cite #,citet #,generate-bib)
         (define #,mycites (hash-keys (bibdb-raw (path->bibdb bib-file))))
         (list (for-each #,~cite #,mycites) (#,generate-bib #:sec-title sec-title))
         )
     )
   ]
  
  ))

(define-syntax-rule (all-pubs)
  (begin
  (makecite "_src/journal_papers.bib"  "Journal Papers")
  (makecite "_src/conf_papers.bib"  "Conference Papers")
  (makecite "_src/conf_versions.bib"  "Conference Versions of Journal Papers Papers")
  
  ))


;(define mycites (hash-keys (bibdb-raw (path->bibdb "_src/mypubs.bib"))))
;(displayln mycites)