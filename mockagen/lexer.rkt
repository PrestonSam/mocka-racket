#lang br

(require brag/support
         workbench/regexp)

(provide mockagen-lexer)


(define-lex-abbrev tab
  (:** 4 4 " "))

(define-lex-abbrev keywords&symbols
  (:or "="
       "?"
       "|"
       "#"
       ","
       
       "INCLUDE"
       "ONEOF"
       "USING"
       "DEF"
       
       "timestamp/date"
       "integer"
       "real"
       "string"
       "join"
       "any"))

(define-lex-abbrev positive
  (:& numeric
      (:~ "0")))


(define-lex-abbrev identifier
  (:+ (:or alphabetic "-")))


(define-lex-abbrev date-literal
  (:: (:** 4 4 numeric) "-" (:** 2 2 numeric) "-" (:** 2 2 numeric)))


(define-lex-abbrev percentage
  (:: (:or positive
           (:: positive numeric)
           (:: (:** 0 2 numeric) "." (:or positive
                                          (:: numeric positive)
                                          (:: positive numeric)))
           "100")
      "%"))


(define mockagen-lexer
  (lexer-srcloc
   [(:: tab tab tab tab tab) (token 'TAB5 lexeme)]
   [(:: tab tab tab tab) (token 'TAB4 lexeme)]
   [(:: tab tab tab) (token 'TAB3 lexeme)]
   [(:: tab tab) (token 'TAB2 lexeme)]
   [(:: tab) (token 'TAB1 lexeme)]

   ["\n" (token 'NEWLINE lexeme)]
   [whitespace (token lexeme #:skip? #t)]
   [(from/stop-before "#" "\n") (token 'COMMENT lexeme #:skip? #t)]
   
   [keywords&symbols (token lexeme lexeme)]
   
   [percentage (token 'WEIGHT (string->number (regexp-extract #px"(.+)%$" lexeme)))]
   
   [(from/to "\"" "\"")
    (token 'STRING-LITERAL (regexp-extract #px"^\"(.+)\"$" lexeme))]
   [date-literal (token 'DATE-LITERAL lexeme)]
   [(:: (:? "-")(:+ numeric)) (token 'INTEGER-LITERAL (string->number lexeme))]
   [(:: (:? "-") (:+ numeric) "." (:+ numeric)) (token 'REAL-LITERAL (string->number lexeme))]
   [identifier (token 'IDENTIFIER (string->symbol lexeme))]))
