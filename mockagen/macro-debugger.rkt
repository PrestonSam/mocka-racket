#lang br/quicklang

(require syntax/parse/define
         syntax/parse
         "00.rkt"
         (for-syntax racket/base syntax/parse
                     "00.rkt"))

(define-syntax-parser make-nested-values-syntax-class
    [(_ nested-values-id:id values-key:id)
     #'(define-syntax-class nested-values-id
         (pattern ((~datum values-key) val:value/class (... ...+))
           #:with generator #'(one-of val.val (... ...))))])
  
  (define-syntax-parser make-assign-syntax-class
    [(_ this-assign-id:id max-depth:number)
     #'(begin
        (make-nested-values-syntax-class nested-values values-1)
        (make-assign-syntax-class #:next next-assign-id 2 max-depth)
        
        (define-syntax-class this-assign-id
          (pattern ((~datum this-assign-id) these-values:nested-values other-values:next-assign-id (... ...))
            #:with generator #'(cons-gen these-values.generator (one-of other-values.generator (... ...))))))]
    
    [(_ #:next this-assign-id:id depth:number max-depth:number)
     #:with next-depth (add1 (syntax-e #'depth))
     #:attr is-last-depth (>= (syntax-e #'next-depth) (syntax-e #'max-depth))
     #:with keyword (if (attribute is-last-depth) #'#:last #'#:next)
     #:attr maybe-max-depth (and (not (attribute is-last-depth)) #'max-depth)
     #:with values-key (string->symbol (format "values-~a" (syntax-e #'depth)))
     #:with assign-key (string->symbol (format "assign-~a" (syntax-e #'depth)))
     
     #'(begin
        (make-assign-syntax-class keyword next-assign-id next-depth (~? maybe-max-depth))
        (make-nested-values-syntax-class nested-values values-key)
        
        (define-syntax-class this-assign-id
          (pattern ((~datum assign-key) these-values:nested-values (~optional (~seq other-values:next-assign-id (... ...))))
            #:with generator #'(cons-gen these-values.generator
                                         ((... ~?) (one-of other-values.generator (... ...))
                                             (const null))))))]
    
    [(_ #:last this-assign-id:id depth:number)
     #:with values-key (string->symbol (format "values-~a" (syntax-e #'depth)))
     #:with assign-key (string->symbol (format "assign-~a" (syntax-e #'depth)))
     #'(begin
         (make-nested-values-syntax-class nested-values values-key)
        
         (define-syntax-class this-assign-id
           (pattern ((~datum assign-key) these-values:nested-values)
             #:with generator #'(cons-gen these-values.generator (const null)))))])

(make-assign-syntax-class assign-1 5)

(syntax-parse #'(assign-1
                 (values-1 (literal-value "Nested-1"))
                 (assign-2 (values-2 (literal-value "Nested-1-1")) (assign-3 (values-3 (literal-value "Nested-1-1-1") (literal-value "Nested-1-1-2"))))
                 (assign-2 (values-2 (literal-value "Nested-1-2")) (assign-3 (values-3 (literal-value "Nested-1-2-1") (literal-value "Nested-1-2-2")))))
  [v:assign-1 #'v])

(require macro-debugger/expand)
(expand-only
 #'(make-assign-syntax-class assign-1 5)
 (list #'make-assign-syntax-class #'make-nested-values-syntax-class))
