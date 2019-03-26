
#lang racket
(require (for-syntax racket/syntax))
(require scriblib/autobib scriblib/bibtex)
(require (for-syntax scriblib/bibtex))
(provide all-pubs)
(provide (all-from-out scriblib/bibtex scriblib/autobib))
(require scribble/core)
(require (for-syntax racket/list))
(require scribble/base)

;(define (do-phony-cites cite-list)
;   (for-each ~cite cite-list)) 
(define-for-syntax (year-lt key1 key2)
                           (>
                            (string->number  key1)
                            (string->number  key2)))

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


(define (smaller-n n x)
  (if (= n 0) x (smaller (smaller-n (- n 1) x)))
  )

(define-for-syntax (makecite bib-file sec-title)
  (let* [
         [thebibdb (bibdb-raw (path->bibdb bib-file))]
         [mycites (hash-keys thebibdb)]
         [years (remove-duplicates (sort (hash-map thebibdb (lambda (k v) (hash-ref v "year"))) year-lt))]
         [year-cites (map (lambda (y)
                            (cons y (filter-map (lambda (pr)
                                          (and (equal? y (hash-ref (cdr pr) "year")) (car pr) ) )
                                        (hash->list thebibdb) )))
                          years)]
         (bib-for-year
          (lambda (pr)
            (let* ([y (car pr)]
                  [yearcites (cdr pr)]
                  (~cite (generate-temporary '~cite))
                  (citet (generate-temporary 'citet))
                  (section-title (if (equal? y (first years)) #`(smaller-n 2 (list #,sec-title (linebreak) (smaller-n 2 #,y) )) #`(smaller-n 4 #,y) ) )
                  (generate-bib (generate-temporary 'generate-bib)))
          #`(begin
              (define-bibtex-cite-unsrt #,bib-file #,~cite #,citet #,generate-bib #:spaces 1 #:disambiguate (lambda (x) "")  )
              (list  (for-each #,~cite (list #,@yearcites))    (#,generate-bib #:sec-title #,section-title #:tag (string-append #,y #,sec-title))
         
                     )
              ))))
         ] ;;end let
    #`(begin #,@(map bib-for-year year-cites))
    ))

  (define-syntax (all-pubs stx)
    #`(begin
        #,(makecite "_src/journal_papers.bib"  "Journal Papers")
        #,(makecite "_src/conf_papers.bib"  "Conference Papers")
        #,(makecite "_src/conf_versions.bib"  "Conference Versions of Journal Papers")
        ;;; #,(makecite "_src/theses.bib"  "Theses")
        ))


  ;(define mycites (hash-keys (bibdb-raw (path->bibdb "_src/mypubs.bib"))))
  ;(displayln mycites)