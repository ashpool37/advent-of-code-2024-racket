#lang racket

(require
 (prefix-in type: relation/type)
 (prefix-in seq: seq))

(define (string->numbers str)
  (map string->number (string-split str)))

(define report%
  (class object%
    (init-field (levels (list)))
    (define/public (safe?)
      (define (levels-safe? levels trend-before)
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
      (levels-safe? (get-field levels this) 'trend-unknown))
    (super-new)))

(define (string->report str)
  (new report% [levels (string->numbers str)]))

(define (main)
  (seq:length
   (seq:filter
    identity
    (seq:map
     (compose (λ (r) (send r safe?)) string->report)
     (type:->stream (in-lines))))))

(with-input-from-file "input" main)
