#lang racket

(define (string->numbers str)
  (map string->number (string-split str)))

(define (main)
  (let*
      ([pairs (map string->numbers (sequence->list (in-lines)))]
       [ids-left (sort (map first pairs) <)]
       [ids-right (sort (map second pairs) <)]
       [pair->distance (λ (id-left id-right) (abs (- id-left id-right)))]
       [distances (map pair->distance ids-left ids-right)]
       [distances-sum (apply + distances)])
    distances-sum))

(with-input-from-file "input" main)
