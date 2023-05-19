#lang br/quicklang

(require threading
         gregor
         racket/random)

(provide string->posix
         timestamp/date
         real
         integer
         random-string
         one-of/weighted)

(define string->posix
  (compose1 iso8601->datetime ->posix))
      
(define ((timestamp/date from-date to-date))
  (~> (random (->posix (iso8601->datetime from-date))
              (->posix (iso8601->datetime to-date)))
      posix->datetime
      datetime->iso8601))

(define ((real from-real to-real))
  (~> (random)
      (* (- to-real from-real))
      (+ from-real)))

(define (integer from-int to-int)
  (define real-gen (real from-int to-int))
  (λ ()
    (~> (real-gen)
        round
        inexact->exact)))

(define A/int (char->integer #\A))
(define z/int (char->integer #\z))

(define ((random-string min-len max-len))
  (~> (for*/list ([count (in-range 0 (random min-len max-len))])
        (integer->char (random A/int z/int)))
      (apply string _)))

(define (one-of/weighted vals weights)
  (define weight-for-unstated
    (for/fold ([total-stated null]
               [unstated-count 0]
               #:result (match* ((- 100 (apply + total-stated)) unstated-count)
                          [(0 _) 0]
                          [((? negative?) _) (error "Weightings add up to a value greater than 100%")]
                          [(_ 0) (error "All weights are stated, yet they do not sum to 100%")]
                          [(v ucount) (/ v ucount)]))
              ([weight (in-list weights)])
      (if weight
          (values `(,weight . ,total-stated) unstated-count)
          (values total-stated (add1 unstated-count)))))

  (define vals+weights
    (for/list ([val (in-list vals)]
               [weight (in-list weights)])
      `(,val . ,(/ (or weight weight-for-unstated) 100))))
  
  (λ ()
    (let loop ([choice (random)]
               [vs+ws vals+weights]
               [scan-pos 0])
      (match vs+ws
        [(list (cons val weight))
         (val)]
        [(list (cons val weight) others ..1)
         (define new-scan-pos
           (+ scan-pos weight))
         (if (<= choice new-scan-pos)
             (val)
             (loop choice others new-scan-pos))]))))
