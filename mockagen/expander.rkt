#lang br/quicklang

(require mockagen/generators
         (except-in syntax/parse/define integer)
         (for-syntax racket/base
                     racket/function
                     racket/syntax
                     racket/sequence
                     (except-in syntax/parse integer)
                     mockagen/generators))

(provide (rename-out [mockagen-module-begin #%module-begin]))

(begin-for-syntax
  ;; TODO figure out how to make this one behave itself
  (define syntax-local-lift-expression/disable (λ (stx) stx))
  
  (define-syntax-class joinable-value/class
    [pattern ((~datum identifier-value) value:id)
     #:with val #'(~a value)
     #:attr using-id #'value]
    [pattern ((~datum literal-value) value:string)
     #:with val #'value
     #:attr using-id #f])

  (define-syntax-class value/class
    ;; Timestamp/date
    [pattern ((~datum timestamp-date-value) from-date:string to-date:string)
     #:with val #'(timestamp/date from-date to-date)
     #:with (using-id ...) #'()]

    ;; Identifier literal
    [pattern ((~datum identifier-value) identifier:id)
     #:with val #'identifier ;; TODO this is broken at the moment
     #:with (using-id ...) #'()]
  
    ;; String literal
    [pattern ((~datum literal-value) value:string)
     #:with val #'(const value)
     #:with (using-id ...) #'()]

    ;; Integer
    [pattern ((~datum integer-value) from-int:number to-int:number)
     #:with val #'(integer from-int to-int)
     #:with (using-id ...) #'()]

    ;; String
    [pattern ((~datum string-value) smallest-length:number largest-length:number)
     #:with val #'(random-string smallest-length largest-length)
     #:with (using-id ...) #'()]

    ;; Real
    [pattern ((~datum real-value) from-real:number to-real:number)
     #:with val #'(real from-real to-real)
     #:with (using-id ...) #'()]
    
    ;; Join
    [pattern ((~datum join-value) value:joinable-value/class ...+)
     #:with val #'(λ () (string-append value.val ...))
     #:with (using-id ...) #'((~? value.using-id) ...)])

  (define-syntax-class match/class
    ;; Timestamp/date
    [pattern ((~datum timestamp-date-value) from-date-str:string to-date-str:string)
     #:with from-posix-inclusive (string->posix (syntax-e #'from-date-str))
     #:with to-posix-exclusive (sub1 (string->posix (syntax-e #'to-date-str)))
     #:with pred #'(λ (v) (<= from-posix-inclusive v to-posix-exclusive))]
  
    ;; String literal
    [pattern ((~datum literal-value) value:string)
     #:with pred #'(λ (v) (equal? v value))]

    ;; Integer
    [pattern ((~datum integer-value) from-int-inclusive:number to-int-inclusive:number)
     #:with to-int-exclusive (sub1 (syntax-e #'to-int-inclusive))
     #:with pred #'(λ (v) (<= from-int-inclusive v to-int-exclusive))]

    ;; Real
    [pattern ((~datum real-value) from-real-inclusive:number to-real-inclusive:number)
     #:with to-real-exclusive (sub1 (syntax-e #'to-real-inclusive))
     #:with pred #'(λ (v) (<= from-real-inclusive v to-real-exclusive))]

    ;; Any
    [pattern ((~datum any-value))
     #:with pred #'(λ (v) #t)])

  (define-syntax-class weight
    [pattern ((~datum weight) val:number)])

  ;; TODO MAKE THE WEIGHTINGS COUNTED BY THE COMPILER
  (define-syntax-class weighted-value/class
    [pattern ((~datum weighted-value) (~optional maybe-weight:number) value:value/class)
     #:with val #'value.val
     #:with weight #'(~? maybe-weight #f)
     #:with (using-id ...) #'(value.using-id ...)])


  (define (remove-duplicate-ids ids-stx)
    (hash-values
     (for/hash ([id (in-syntax ids-stx)])
       (values (syntax-e id) id))))


  
  (define-syntax-class single-definition
    [pattern ((~datum single-definition) name:id value:value/class)
     #:with assign-id #'name
      #:with (using-id ...) (remove-duplicate-ids #'(value.using-id ...))
     #:with generator #'(λ (using-id ...) (value.val))]
    [pattern ((~datum single-definition) name:id wval:weighted-value/class ...)
     #:with assign-id #'name
      #:with (using-id ...) (remove-duplicate-ids #'(wval.using-id ... ...))
     #:with generator (syntax-local-lift-expression/disable
                       #'(λ (using-id ...)
                           ((one-of/weighted (list wval.val ...)
                                             (list wval.weight ...)))))])
  

  ;; TODO add a branch to every match that provides a helpful error message if no other branch matches the object
  (define-syntax-class (matcher ids)
    ;; Match branch -> nested assignment
    [pattern (_ "?" (_ key:match/class ...+) child:assignment ...+)
     #:with pred (syntax-local-lift-expression/disable
                  #'(disjoin key.pred ...))
     #:with gen-name (syntax-local-lift-expression/disable
                      #'(one-of/weighted
                         (list child.generator ...)
                         (list child.weight ...)))
     #:with generator #'(gen-name)
     #:with (using-id ...) #'(child.using-id ... ...)]

    ;; Match branch -> nested match
    [pattern (_ "?" (_ key:match/class ...+) child ...+)
     #:declare child (matcher (if (null? ids) ;; Null check is a workaround for eager evaluation of this line
                                  'cannot-reach-here
                                  (cdr ids)))
     #:with pred (syntax-local-lift-expression/disable
                  #'(disjoin key.pred ...))
     #:with generator #`(match #,(car ids)
                          [(? child.pred) child.generator] ...)
     #:with (using-id ...) #'(child.using-id ... ...)])


  ;; The final output should be a list of generators
  ;; Each of these generators can produce a set of legal values according to a given path through a tree
  ;; Invoking the top-level 'generator' syntax will produce a list representing a randomly chosen path through the generator tree
  ;; You can then invoke any proc in the list to generate a value at the given depth, belonging to the given path
  (define-syntax-class assignment
    ;; Many possible values
    [pattern (_ "=" (~optional maybe-weight:weight) (_ multi-val:weighted-value/class ...+) (~optional (~seq child:assignment ...+)))
     #:with this-def (syntax-local-lift-expression/disable
                      #'(one-of/weighted (list multi-val.val ...)
                                         (list multi-val.weight ...)))
     #:with trailing-defs (syntax-local-lift-expression/disable
                           #'(~? (one-of/weighted (list child.generator ...)
                                                  (list child.weight ...))
                                 (const null)))
     #:with weight #'(~? maybe-weight.val #f)
     #:with generator #'(λ () (cons this-def (trailing-defs)))
     #:with (using-id ...) #'(multi-val.using-id ... ... (~? (~@ child.using-id ... ...)))]

    ;; Single value
    [pattern (_ "=" (~optional maybe-weight:weight) (_ mono-val:value/class) (~optional (~seq child:assignment ...+)))
     #:with trailing-defs (syntax-local-lift-expression/disable
                            #'(~? (one-of/weighted (list child.generator ...)
                                                   (list child.weight ...))
                                  (const null)))
     #:with weight #'(~? maybe-weight.val #f)
     #:with generator #'(λ () (cons mono-val.val (trailing-defs)))
     #:with (using-id ...) #'(mono-val.using-id ... (~? (~@ child.using-id ... ...)))])
  

  (define-syntax-class nested-definition
    [pattern ((~datum nested-definition)
               ((~datum using-ids) first-using-id:id other-using-id:id ...)
               ((~datum assign-ids) assign-id:id ...+)
               matcher ...+)
     #:declare matcher (matcher (syntax->list #'(other-using-id ...)))
     #:with (child-using-id ...) (remove-duplicate-ids #'(matcher.using-id ... ...))
     #:with (using-id ...) #'(first-using-id other-using-id ... child-using-id ...)
     #:with generator (syntax-local-lift-expression/disable
                       #'(λ (using-id ...)
                           (match first-using-id
                             [(? matcher.pred) matcher.generator] ...)))]

    [pattern ((~datum nested-definition)
              ((~datum assign-ids) assign-id:id ...+)
              assignment:assignment ...+)
     #:with (using-id ...) (remove-duplicate-ids #'(assignment.using-id ... ...))
     #:with generator (syntax-local-lift-expression/disable
                       #'(one-of/weighted (list assignment.generator ...)
                                          (list assignment.weight ...)))])


  (define-syntax-class definition
    [pattern s-stmt:single-definition
     #:with generator-data #'(single (s-stmt.using-id ...) s-stmt.assign-id ,s-stmt.generator)]
    [pattern n-stmt:nested-definition
     #:with generator-data #'(nested (n-stmt.using-id ...) (n-stmt.assign-id ...) ,n-stmt.generator)])


  (define-syntax-class includes
    [pattern ((~datum include-statement) given-include:string ...+)
     #:with (include ...) #'(given-include ...)]))


(define-syntax-parser mockagen-module-begin
  [(_ ((~datum body) (~optional includes:includes) ((~datum statements) def:definition ...)))
   #:with (included-gen ...) (for/list ([include (in-syntax #'(~? (includes.include ...) ()))]
                                        [n (in-naturals)])
                               (format-id include "included-gen-~a" n))
   #'(#%module-begin
      (require racket/base
               racket/match
               racket/function
               workbench/function
               mockagen/generators
               (~? (~@ . ((rename-in includes.include [all-generators included-gen]) ...))))

      (provide all-generators)
      
      (define all-generators
        `(,@included-gen ...
          def.generator-data ...)))])
