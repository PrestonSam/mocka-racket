#lang racket/base

(require syntax/parse/define
         sam-utils/function
         (for-syntax racket/base
                     racket/match
                     sam-utils/define
                     sam-utils/function
                     sam-utils/list
                     "../thrungle.mkg"))

(provide row-gen)

(begin-for-syntax
  #| Inserted by the macro that generates the module |#
  (define doc-gen-id-stxs
    (syntax->list #'(region transaction-volume artist album track)))

  (define doc-gen-id-datums
    (map syntax-e doc-gen-id-stxs))

  (define (def-fold-proc gen defs all-used-ids)
    (define*
      [(using-ids assign-ids def)
       (match gen
         [`(single ,assign-id ,generator)
          (values
           null
           `(,assign-id)
           #`(define #,assign-id
               (#,generator)))]
         [`(nested (,using-ids ...) (,assign-ids ...) ,generator)
          (values
           using-ids
           assign-ids
           #`(define-values #,assign-ids
               (apply values
                      (map call #,(cons generator using-ids)))))])])
    (if (member*? (append doc-gen-id-datums all-used-ids) assign-ids)
        (values
         `(,def . ,defs)
         `(,@using-ids . ,all-used-ids))
        (values
         defs
         all-used-ids))))


(define-syntax-parser get-row-gen
  [_
   #:with (def ...) (for/fold ([defs null]
                               [all-used-ids null]
                               #:result defs)
                              ([gen (in-list (reverse all-generators))])
                      (def-fold-proc gen defs all-used-ids))
   #:with (doc-gen-id-stx ...) doc-gen-id-stxs
   #`(λ ()
       def ...
       (list doc-gen-id-stx ...))])

(define row-gen get-row-gen)
