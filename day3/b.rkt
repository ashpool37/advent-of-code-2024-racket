#lang racket

(define (main)
  (let ([matches
         (regexp-match*
          #px"do\\(\\)|don't\\(\\)|mul\\((\\d{1,3}),(\\d{1,3})\\)"
          (port->string)
          #:match-select values)]
        [enable-mul? #t])
    (for/sum ([m matches])
      (match m
        [(list "do()" #f #f)
         (set! enable-mul? #t)
         0]
        [(list "don't()" #f #f)
         (set! enable-mul? #f)
         0]
        [(list _ (app string->number a) (app string->number b))
         (if enable-mul?
             (* a b)
             0)]))))

(with-input-from-file "input" main)
