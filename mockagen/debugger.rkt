#lang br/quicklang

(require "../parser.rkt"
         "../tokeniser.rkt"
         macro-debugger/expand)

(define src
  #;"/home/sam/Talanta10/dfs-portal-be/codegen/Racket/Experimental/Events/generic-event-types.etd"
  "/home/sam/Talanta10/dfs-portal-be/codegen/Racket/Experimental/thrungle.etd")

(define parse-tree
  (parse src (make-tokeniser (open-input-file src) src)))

(displayln parse-tree)

(require (rename-in "01.rkt" [#%module-begin expander]))

(define (get-expanded-code)
  (expand-only
 #`(expander #,parse-tree)
 (list #'expander)))


#'(begin
    (require racket/base
             event-type-ddl/expander/generators
             "Events/generic-event-types.etd")
    (provide something
             region
             transaction-volume
             an-int
             nest-one
             nest-two
             nest-three
             (all-from-out
              "Events/generic-event-types.etd"))
    (define something
      (one-of/weighted
       (list (const "Value"))
       (list #f)))
    (parallel-definition
     (region transaction-volume)
     (one-of/weighted
      (list
       (cons-gen
        (one-of/weighted
         (list
          (const "Addis Ababa"))
         (list #f))
        (one-of/weighted
         (list
          (cons-gen
           (one-of/weighted
            (list
             (const "Small")
             (const "Larger")
             (const "Large"))
            (list 60 30 10))
           (const null))
          (cons-gen
           (one-of/weighted
            (list
             (const
              "Really rare special case"))
            (list #f))
           (const null)))
         (list 98 2)))
       (cons-gen
        (one-of/weighted
         (list (const "1VAL2"))
         (list #f))
        (one-of/weighted
         (list
          (cons-gen
           (one-of/weighted
            (list
             (const
              "Some string"))
            (list #f))
           (const null))
          (cons-gen
           (one-of/weighted
            (list
             (λ ()
               (string-append
                (~a (something))
                ", "
                (~a
                 (something)))))
            (list #f))
           (const null)))
         (list 40 60)))
       (cons-gen
        (one-of/weighted
         (list
          (random-string 8 12))
         (list #f))
        (one-of/weighted
         (list
          (cons-gen
           (one-of/weighted
            (list
             (const "2VAL4"))
            (list #f))
           (const null)))
         (list #f)))
       (cons-gen
        (one-of/weighted
         (list
          (const "1VAL3")
          (const "1VAL4")
          (const "1VAL5")
          (const "1VAL6"))
         (list #f #f #f #f))
        (one-of/weighted
         (list
          (cons-gen
           (one-of/weighted
            (list
             (const "2VAL5"))
            (list #f))
           (const null)))
         (list #f))))
      (list 60 #f #f #f)))
    (define an-int
      (one-of/weighted
       (list (integer 8 100))
       (list #f)))
    (parallel-definition
     (nest-one
      nest-two
      nest-three)
     (one-of/weighted
      (list
       (cons-gen
        (one-of/weighted
         (list
          (const "Nested-1"))
         (list #f))
        (one-of/weighted
         (list
          (cons-gen
           (one-of/weighted
            (list
             (const
              "Nested-1-1"))
            (list #f))
           (one-of/weighted
            (list
             (cons-gen
              (one-of/weighted
               (list
                (const
                 "Nested-1-1-1")
                (const
                 "Nested-1-1-2"))
               (list #f #f))
              (const null)))
            (list #f)))
          (cons-gen
           (one-of/weighted
            (list
             (const
              "Nested-1-2"))
            (list #f))
           (one-of/weighted
            (list
             (cons-gen
              (one-of/weighted
               (list
                (const
                 "Nested-1-2-1")
                (const
                 "Nested-1-2-2"))
               (list #f #f))
              (const null)))
            (list #f))))
         (list #f #f)))
       (cons-gen
        (one-of/weighted
         (list
          (const "Nested-2"))
         (list #f))
        (one-of/weighted
         (list
          (cons-gen
           (one-of/weighted
            (list
             (const
              "Nested-2-1"))
            (list #f))
           (one-of/weighted
            (list
             (cons-gen
              (one-of/weighted
               (list
                (const
                 "Nested-2-1-1"))
               (list #f))
              (const null)))
            (list #f))))
         (list #f))))
      (list #f #f))))
