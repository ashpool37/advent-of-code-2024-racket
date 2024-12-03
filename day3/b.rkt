#lang racket

(define bytes->number (compose string->number bytes->string/utf-8))

(define (main)
  (let ([enable-mul? #t]
        [acc 0])
    (for ([m (in-producer
              (thunk (regexp-match
                      #px"do\\(\\)|don't\\(\\)|mul\\((\\d{1,3}),(\\d{1,3})\\)"
                      (current-input-port)))
              #f)])
      (match m
        [(list #"do()" #f #f)
         (set! enable-mul? #t)]
        [(list #"don't()" #f #f)
         (set! enable-mul? #f)]
        [(list _ (app bytes->number a) (app bytes->number b))
         (when enable-mul? (set! acc (+ acc (* a b))))]))
    acc))

(with-input-from-file "input" main)
