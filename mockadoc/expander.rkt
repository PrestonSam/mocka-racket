#lang br/quicklang

(require syntax/parse/define
         log-once
         (for-syntax racket/sequence
                     racket/function
                     syntax/strip-context
                     mockadoc/structs
                     threading
                     workbench/list
                     workbench/vector
                     log-once))

(provide (rename-out [mockadoc-module-begin #%module-begin]))

(begin-for-syntax  
  (define-splicing-syntax-class suffix-modifier
    (pattern ((~datum primary-timestamp-suffix))
      #:with code 'primary-timestamp)
    (pattern ((~datum personal-suffix))
      #:with code 'personal)
    (pattern ((~datum unique-suffix))
      #:with code 'unique))

  (define-splicing-syntax-class suffix-modifier+tag
    (pattern ((~datum suffix-modifier) modifier:suffix-modifier)
      #:with code #'modifier.code))
  
  (define-syntax-class column-suffix
    (pattern ((~datum suffix) modifier:suffix-modifier+tag ...+)
      #:attr codes (syntax->datum #'(modifier.code ...))
      #:attr is-personal (member? 'personal (attribute codes))
      #:with is-primary-timestamp #`#,(member? 'primary-timestamp (attribute codes))))

  (define-syntax-class column
    (pattern ((~datum column) template-name:string internal-name:id druid-type:string generator-name:id (~optional suffix:column-suffix))
      #:with generator-id #'generator-name
      #:attr personal-generator-id (if (attribute suffix.is-personal) #'generator-name #f)
      #:with data #`(column-struct template-name
                                   (symbol->string 'internal-name)
                                   druid-type
                                   (~? suffix.is-primary-timestamp #f))))
  
  (define-syntax-class event
    (pattern ((~datum event) event-name:string ((~datum typedef) -type-def:string) ((~datum columns) -column:column ...+))
      #:with type-def (strip-context #'-type-def)
      #:with name #'event-name
      #:with (personal-generator-id ...) #'((~? -column.personal-generator-id) ...)
      #:with (generator-id ...) #'(-column.generator-id ...)
      #:with (column ...) #'(-column.data ...))))

(define-syntax-parser mockadoc-module-begin
  [(_ event:event)
   #:with identity-ref (format-id #f "identity-ref")
   #'(#%module-begin
      (require syntax/parse/define
               workbench/function
               workbench/vector
               mockadoc/structs
               (for-syntax racket/base
                           racket/match
                           workbench/define
                           workbench/function
                           workbench/list
                           workbench/vector
                           event.type-def))

      (provide event-data
               (all-from-out mockadoc/structs))

      (begin-for-syntax
        (struct identity-data (identity-ref-id vectors) #:transparent)
        
        (define doc-gen-id-stxs
          `((~@ ,#'event.generator-id) ...))

        (define doc-gen-id-datums
          (map syntax-e doc-gen-id-stxs))

        (define (fetch-id id)
          (or (for/first ([id-stx (in-list doc-gen-id-stxs)]
                          [id-datum (in-list doc-gen-id-datums)]
                          #:when (eq? id-datum id))
                id-stx)
              id))        

        (define (unpack-generator-properties gen identity-vectors)
          (match gen
            [`(single (,using-ids (... ...)) ,assign-id ,generator)
             (values
              using-ids
              `(,assign-id)
              (with-syntax ([gen #`(#,generator . #,(map fetch-id using-ids))])
                #`(define #,(fetch-id assign-id)
                    #,(match (hash-ref identity-vectors assign-id #f)
                        [#f #'gen]
                        [identity-vec-name #`(vector-ref! #,identity-vec-name identity-ref (λ () gen))]))))]
               
            [`(nested (,using-ids (... ...)) (,assign-ids (... ...)) ,generator)
             (values
              using-ids
              assign-ids
              (with-syntax ([gen generator]
                            [(assign-id (... ...)) assign-ids]
                            [(gen-call (... ...)) (for/list ([id (in-list assign-ids)])
                                                    (match (hash-ref identity-vectors id #f)
                                                      [#f #`(#,id)]
                                                      [identity-vec-name
                                                       #`(vector-ref! #,identity-vec-name identity-ref #,id)]))]
                            [(fetched-assign-id (... ...)) (map fetch-id assign-ids)]
                            [(fetched-using-id (... ...)) (map fetch-id using-ids)])
                #`(define-values (fetched-assign-id (... ...))
                    (match (gen fetched-using-id (... ...))
                      [(list assign-id (... ...))
                       (values gen-call (... ...))]))))]))

        
        (define (def-fold-proc gen defs all-used-ids identity-vectors)
          (define*
            [(using-ids assign-ids def)
             (unpack-generator-properties gen identity-vectors)])
          
          (if (member*? all-used-ids assign-ids)
              (values
               `(,def . ,defs)
               `(,@using-ids . ,all-used-ids))
              (values
               defs
               all-used-ids))))

      (define-syntax-parser get-row-gen
        [_
         #:attr identity-vectors (for/hash ([id (in-syntax #'(event.personal-generator-id ...))])
                                   (values (syntax-e id) (format-id id "~a-identity-vec" id)))
         #:with (identity-vec-name (... ...)) (hash-values (attribute identity-vectors))
         #:with (def (... ...)) (for/fold ([defs null]
                                           [all-used-ids doc-gen-id-datums]
                                           #:result defs)
                                          ([gen (in-list (reverse all-generators))])
                                  (def-fold-proc gen defs all-used-ids (attribute identity-vectors)))
         #:with (doc-gen-id-stx (... ...)) doc-gen-id-stxs
         #'(λ (identity-count)
             (define identity-vec-name
               (make-vector identity-count #f)) (... ...)
             (λ ()
               (define identity-ref
                 (random 0 identity-count))
               def (... ...)
               (list doc-gen-id-stx (... ...))))])

      (define event-data
        (event-struct
         event.name
         (list event.column ...)
         get-row-gen)))])
