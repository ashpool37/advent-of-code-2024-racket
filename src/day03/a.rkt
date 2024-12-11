#lang at-exp racket

(require (prefix-in seq: seq)
         relation/type)

(define (read-mul)
  (regexp-try-match @pregexp{mul\((\d{1,3}),(\d{1,3})\)}
                    (current-input-port)))

(define bytes->number (compose string->number bytes->string/utf-8))

(define (eval-mul match-result)
  (apply * (map bytes->number (cdr match-result))))

(define (main)
  (seq:foldl + 0 (seq:map eval-mul (->stream (in-producer read-mul #f)))))

(with-input-from-file "input" main)
