#lang racket

(require
 (prefix-in seq: seq)
 (prefix-in type: relation/type))

(define (between? value lbound ubound)
  (and (<= lbound value) (< value ubound)))

(define (applicator fn)
  (λ (args) (apply fn args)))

(define (main)
  (let*
      ([matrix (type:->vector
                (sequence-map string->immutable-string (in-lines)))]
       [matrix-height (seq:length matrix)]
       [matrix-width (seq:length (seq:ref matrix 0))]
       [pattern "XMAS"]
       [pattern-length (seq:length pattern)]
       [count-patterns-at
        (λ (y x)
          (define (pattern-in-direction? dy dx)
            (and (between? (+ y (* (sub1 pattern-length) dy)) 0 matrix-height)
                 (between? (+ x (* (sub1 pattern-length) dx)) 0 matrix-width)
                 (for/and ([i (in-range pattern-length)])
                   ;; using string-ref and vector-ref here
                   ;; as nth from seq is REALLY slow
                   ;; https://github.com/countvajhula/seq/issues/6
                   ;; ref from data/collection is faster, but still
                   ;; noticeably slower than the native functions
                   (equal? (string-ref (vector-ref matrix (+ y (* i dy)))
                                       (+ x (* i dx)))
                           (string-ref pattern i)))))
          (define
            directions '((-1 0) (-1 1) (0 1) (1 1) (1 0) (1 -1) (0 -1) (-1 -1)))
          (seq:length (seq:filter (applicator pattern-in-direction?)
                                  directions)))])
    (for/sum ([y (in-range matrix-height)])
      (for/sum ([x (in-range matrix-width)])
        (count-patterns-at y x)))))

(with-input-from-file "input" main)
