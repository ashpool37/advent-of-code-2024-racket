#lang racket

(require
 (prefix-in seq: seq)
 (prefix-in type: relation/type))

(define (main)
  (let*
      ([matrix (type:->vector
                (sequence-map string->immutable-string (in-lines)))]
       [matrix-height (seq:length matrix)]
       [matrix-width (seq:length (seq:ref matrix 0))]
       [patterns '(#("M.M"
                     ".A."
                     "S.S")
                   #("M.S"
                     ".A."
                     "M.S")
                   #("S.S"
                     ".A."
                     "M.M")
                   #("S.M"
                     ".A."
                     "S.M"))]
       [pattern-at?
        (λ (y x)
          (for/or ([pat patterns])
            (let ([pat-height (seq:length pat)]
                  [pat-width (seq:length (seq:ref pat 0))])
              (and (< (+ x (sub1 pat-width)) matrix-width)
                   (< (+ y (sub1 pat-height)) matrix-height)
                   (for/and ([pat-y (in-range pat-height)])
                     (for/and ([pat-x (in-range pat-width)])
                       (let ([pat-ch (string-ref (vector-ref pat pat-y) pat-x)])
                         (or (equal? pat-ch #\.)
                             (let ([mtx-ch
                                    (string-ref (vector-ref matrix (+ y pat-y))
                                                (+ x pat-x))])
                               (equal? pat-ch mtx-ch))))))))))])
    (for/sum ([y (in-range matrix-height)])
      (seq:length (seq:filter (curry pattern-at? y) (in-range matrix-width))))))

(with-input-from-file "input" main)
