#lang racket

(require
 (prefix-in seq: seq)
 (prefix-in type: relation/type))

(define (string->numbers str)
  (map string->number (string-split str)))

(define (item-counts seq)
  (let ([counts (make-hash)])
    (for ([item seq])
      (hash-update! counts item add1 0))
    counts))

(define (main)
  (let*
      ([pairs (seq:map string->numbers (type:->stream (in-lines)))]
       [ids-left (seq:map first pairs)]
       [ids-right (seq:map second pairs)]
       [ids-right-counts (item-counts ids-right)])
    (for/sum ([id ids-left])
      (* id (hash-ref ids-right-counts id 0)))))

(with-input-from-file "input" main)
