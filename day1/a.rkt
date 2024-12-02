#lang racket

(require
 (prefix-in seq: seq)
 (prefix-in type: relation/type)
 (prefix-in ord: relation/order)
 (prefix-in comp: relation/composition))

(define (string->numbers str)
  (map string->number (string-split str)))

(define (main)
  (let*
      ([pairs (seq:map string->numbers (type:->stream (in-lines)))]
       [ids-left (ord:sort ord:< (seq:map first pairs))]
       [ids-right (ord:sort ord:< (seq:map second pairs))]
       [distances (seq:zip-with (compose abs -) ids-left ids-right)])
    (comp:sum distances)))

(with-input-from-file "input" main)
