
#lang racket
(require scriblib/autobib scriblib/bibtex)
(provide ~cite citet generate-bib mycites)
(provide (all-from-out scriblib/bibtex scriblib/autobib))

(define-bibtex-cite "_src/mypubs.bib" ~cite citet generate-bib)
(define mycites (hash-keys (bibdb-raw (path->bibdb "_src/mypubs.bib"))))
(displayln mycites)