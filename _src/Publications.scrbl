#lang scribble/manual
@require["bib.rkt"]

Publications go here

@(define (allcites) (for-each
                (lambda (x) (~cite x))
                   mycites))

@allcites{}



@(generate-bib #:sec-title "")