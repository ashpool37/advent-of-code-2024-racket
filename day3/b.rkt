#lang at-exp racket

(define (read-op)
  (regexp-try-match
   @pregexp{do\(\)|don't\(\)|mul\((\d{1,3}),(\d{1,3})\)}
   (current-input-port)))

(define bytes->number (compose string->number bytes->string/utf-8))

(define (main)
  (let ([is-mul-enabled #t]
        [acc 0])
    (for ([m (in-producer read-op #f)])
      (match m
        [(list #"do()" #f #f)
         (set! is-mul-enabled #t)]
        [(list #"don't()" #f #f)
         (set! is-mul-enabled #f)]
        [(list _ (app bytes->number a) (app bytes->number b))
         (when is-mul-enabled (set! acc (+ acc (* a b))))]))
    acc))

(with-input-from-file "input" main)
