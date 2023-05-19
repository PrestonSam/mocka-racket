#lang br

(require "lexer.rkt" brag/support)

(provide make-tokeniser)

(define (make-tokeniser input-port [path #f])
  (port-count-lines! input-port)
  (lexer-file-path path)
  (define (next-token) (mockadoc-lexer input-port))
  next-token)
