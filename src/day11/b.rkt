#lang racket

(require (prefix-in seq: seq)
         memo)

(define (even-length-number-halves num)
  (define number-str (number->string num))
  (define number-strlen (string-length number-str))
  (define number-strlen-half (/ number-strlen 2))
  (if (integer? number-strlen-half)
      (list (string->number (substring number-str 0 number-strlen-half))
            (string->number (substring number-str number-strlen-half)))
      #f))

(define/memoize (blink-once stone)
  (match stone
    [0 (stream 1)]
    [(app even-length-number-halves (? stream? halves)) halves]
    [_ (stream (* stone 2024))]))

(define/memoize (blink n stone)
  (if (= 0 n)
      1
      (match stone
        [0 (blink (sub1 n) 1)]
        [(app even-length-number-halves (list left-half right-half))
         (+ (blink (sub1 n) left-half) (blink (sub1 n) right-half))]
        [_ (blink (sub1 n) (* 2024 stone))])))

(define (main)
  (seq:foldl + 0 (seq:map (curry blink 75) (sequence->stream (in-port)))))

(with-input-from-file "input" main)
