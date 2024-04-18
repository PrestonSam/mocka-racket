#lang br/quicklang

(require "parser.rkt"
         "tokeniser.rkt")

(define (read-syntax path port)
  (define parse-tree
    (parse path (make-tokeniser port path)))

  (strip-bindings
   #`(module mockagen-mod mockagen/expander
       #,parse-tree)))

(module+ reader
  (provide read-syntax))

#;(define src "path/to/generic-event-types.mkg")

#;(define (make-syntax-tree)
  (let* ([port (open-input-file src)])
    (read-string (string-length "#lang mockagen\n") port)
    (parse src (make-tokeniser port src))))

#;(define (make-token-list)
  (define next
    (let* ([port (open-input-file src)])
      (read-string (string-length "#lang mockagen\n") port)
      (make-tokeniser port src)))

  (let loop ([out null])
    (match (next)
      [(? eof-object?)
       (reverse out)]
      [(srcloc-token (token-struct _ _ _ _ _ _ #t) _)
       (loop out)]
      [(srcloc-token (token-struct type val _ _ _ _ _) _)
       (loop `((,type ,val) . ,out))])))

#;(require brag/support
         racket/sequence)

#;(define (check-tokens)  
  (for*/list ([token (sequence-map srcloc-token-token (in-list (make-token-list)))]
              [type (in-value (token-struct-type token))]
              [val (in-value (token-struct-val token))]
              #:unless (eq? type '| |))
    (if (eq? type 'NEWLINE)
        null
        (cons type val))))


#;(make-syntax-tree)

#;(make-token-list)



#;(require (rename-in event-type-ddl/expander [#%module-begin #%lang-module-begin])
           macro-debugger/expand)

#;(expand-only #`(#%lang-module-begin #,(make-syntax-tree))
             (list #'#%lang-module-begin))

#;(match (syntax->datum (make-syntax-tree))
  [(list 'body (list 'statements _ ... (and (list 'nested-definition _ ...) nested-definition) _ ...))
   nested-definition])

