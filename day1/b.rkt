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
      ([pairs (seq:map (compose type:->vector string->numbers)
                       (type:->stream (in-lines)))]
       [ids-left (seq:map (curryr vector-ref 0) pairs)]
       [ids-right (seq:map (curryr vector-ref 1) pairs)]
       [ids-right-counts (item-counts ids-right)])
    (for/sum ([id ids-left])
      (* id (hash-ref ids-right-counts id 0)))))

(with-input-from-file "input" main)
