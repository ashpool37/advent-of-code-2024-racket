#lang racket

(require (prefix-in col: data/collection)
         memo)

(define/memoize (even-length-number-halves num)
  (define number-str (number->string num))
  (define number-strlen (string-length number-str))
  (define number-strlen-half (/ number-strlen 2))
  (if (integer? number-strlen-half)
      (list (string->number (substring number-str 0 number-strlen-half))
            (string->number (substring number-str number-strlen-half)))
      #f))

(define/memoize (blink-stone stone)
  (match stone
    [0 (list 1)]
    [(app even-length-number-halves (? list? halves)) halves]
    [_ (list (* stone 2024))]))

(define (blink stones)
  (col:flatten (stream-map blink-stone stones)))

(define (blink-n-times n)
  (apply compose (make-list n blink)))

(define (main)
  (stream-length ((blink-n-times 75) (sequence->stream (in-port read)))))

(with-input-from-file "input" main)
