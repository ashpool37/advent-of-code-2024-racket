#lang racket

(define (main)
  (apply
   +
   (map
    (λ (strs) (apply * (map string->number strs)))
    (regexp-match* #px"mul\\((\\d{1,3}),(\\d{1,3})\\)"
                   (port->string)
                   #:match-select cdr))))

(with-input-from-file "input" main)
