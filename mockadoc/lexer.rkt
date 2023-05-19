#lang br

(require brag/support
         workbench/regexp)

(provide mockadoc-lexer)


(define-lex-abbrev keywords&symbols
  (:or "EVENT"
       "TYPEDEF"
       "PRIMARY"
       "TIMESTAMP"
       "PERSONAL"
       "UNIQUE"
       "AND"
       "AS"
       "-"
       "#"))

(define-lex-abbrev druid-types
  (:or "long"
       "string"
       "double"
       "bool"))


(define-lex-abbrev identifier
  (:+ (:or alphabetic "-")))


(define mockadoc-lexer
  (lexer-srcloc
   ["\n" (token 'NEWLINE lexeme)]
   [whitespace (token lexeme #:skip? #t)]
   [(from/stop-before "#" "\n") (token 'COMMENT lexeme #:skip? #t)]
   [keywords&symbols (token lexeme lexeme)]
   [druid-types (token 'DRUID-TYPE lexeme)]
   [(from/to "\"" "\"")
    (token 'STRING (regexp-extract #px"^\"(.+)\"$" lexeme))]
   [identifier (token 'IDENTIFIER (string->symbol lexeme))]))
