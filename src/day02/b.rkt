#lang racket

(require
 (prefix-in type: relation/type)
 (prefix-in seq: seq))

(define (string->numbers str)
  (map string->number (string-split str)))

(define (levels-safe? levels [trend-before 'trend-unknown])
  (match levels
    [(list) #t]
    [(list _) #t]
    [(cons head tail)
     (let*
         ([next (first tail)]
          [step (- next head)]
          [trend (cond [(positive? step) 'trend-up]
                       [(negative? step) 'trend-down]
                       [else 'trend-constant])])
       (and (not (eq? trend 'trend-constant))
            (or (eq? trend-before 'trend-unknown)
                (eq? trend-before trend))
            (<= (abs step) 3)
            (levels-safe? tail trend)))]))

(define (levels-dampened-safe? levels)
  (or (levels-safe? levels)
      (for/or ([index (in-range (length levels))])
        (levels-safe? (type:->list (seq:remove-at index levels))))))

(define (main)
  (seq:length (seq:filter (compose levels-dampened-safe? string->numbers)
                          (type:->stream (in-lines)))))

(with-input-from-file "input" main)
