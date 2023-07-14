#lang br/quicklang

(require "parser.rkt"
         "tokeniser.rkt")

(define (read-syntax path port)
  (define parse-tree
    (parse path (make-tokeniser port path)))

  (strip-bindings
   #`(module mockadoc-mod mockadoc/expander
       #,parse-tree)))

(define (read-mockadoc-syntax file-path)
  (define port
    (open-input-file file-path))
  (read-line port)
  (read-syntax file-path port))

(provide read-mockadoc-syntax)

(module+ reader
  (provide read-syntax))

#;(define src "/home/sam/Talanta10/dfs-portal-be/codegen/Racket/Experimental/Events/Transaction/event.mkd")

#;(define (make-syntax-tree)
  (let* ([port (open-input-file src)])
    (read-string (string-length "#lang mockadoc\n") port)
    (parse src (make-tokeniser port src))))

#;(define syntax-tree
  (make-syntax-tree))

#;(define (make-token-tree)
  (define next
    (let* ([port (open-input-file src)])
      (read-string (string-length "#lang mockadoc\n") port)
      (make-tokeniser port src)))

  (let loop ()
    (define val (next))
    (if (eof-object? val)
        null
        (cons val (loop)))))

#;(define token-tree
  (make-token-tree))

#;'(hash
    "Loan"
    (hash
     "Timestamp"
     "UnixTimestamp"
     "long"
     "unix-timestamp"
     (primary-timestamp)
     "Loan value"
     "TransactionValue"
     "double"
     "credit"
     "Recipient name"
     "ActorName"
     "string"
     "full-name"
     (personal unique)
     "Recipient gender"
     "ActorGender"
     "string"
     "gender"
     (personal)
     "Recipient age"
     "ActorAge"
     "long"
     "age"
     (personal)
     "Bank name"
     "ActorFSP"
     "string"
     "bank-name"
     (personal)
     "Card number"
     "ActorCardNumber"
     "long"
     "bank-card-number"
     (personal unique)
     "Account number"
     "ActorAccountNumber"
     "long"
     "bank-account-number"
     (personal unique)
     "Institution code"
     "ActorInstitutionCode"
     "string"
     "institution-code"
     (personal unique)
     "Loan type"
     "LoanType"
     "string"
     "loan-type"
     "Loan action"
     "LoanAction"
     "string"
     "loan-action"
     "Industry sector"
     "ActorIndustrySector"
     "string"
     "industry-sector"
     (personal)
     "Economic sector"
     "ActorEconomicSector"
     "string"
     "economic-sector"
     (personal)
     "Collateral type"
     "CollateralType"
     "string"
     "collateral-type"
     "Collateral value"
     "CollateralValue"
     "double"
     "credit"
     "Region"
     "ActorRegion"
     "string"
     "region"
     (personal)
     "Municipality"
     "ActorMunicipality"
     "string"
     "municipality"
     (personal)
     "Woreda"
     "ActorWoreda"
     "string"
     "woreda"
     (personal)
     "Latitude"
     "Latitude"
     "double"
     "latitude"
     (personal)
     "Longitude"
     "Longitude"
     "double"
     "longitude"
     (personal)))

