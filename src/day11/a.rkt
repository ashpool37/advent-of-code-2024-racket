#lang racket

(require racket/generator)

(define (even-length-number-halves num)
  (define number-str (number->string num))
  (define number-strlen (string-length number-str))
  (define number-strlen-half (/ number-strlen 2))
  (if (even? number-strlen)
      (list (string->number (substring number-str 0 number-strlen-half))
            (string->number (substring number-str number-strlen-half)))
      #f))

(define (blink stones)
  (define stone-gen
    (generator
     ()
     (for ([stone (in-stream stones)])
       (match stone
         [0
          (yield 1)]
         [(app even-length-number-halves (list left-half right-half))
          (yield left-half)
          (yield right-half)]
         [_
          (yield (* stone 2024))]))))
  (sequence->stream (in-producer stone-gen (void))))

(define (blink-n-times n)
  (apply compose (make-list n blink)))

(define (main)
  (stream-length ((blink-n-times 25) (sequence->stream (in-port read)))))

(with-input-from-file "input" main)
