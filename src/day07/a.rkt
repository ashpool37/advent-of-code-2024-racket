#lang at-exp racket

(require relation/type
         (prefix-in seq: seq))

(define bytes->number (compose string->number bytes->string/utf-8))
(define (bytes->numbers bts)
  (map string->number (string-split (bytes->string/utf-8 bts))))

(define equation%
  (class object%
    (init result operands)
    (define equation-result result)
    (define equation-operands operands)
    (super-new)

    (define (next-operand) (first equation-operands))
    (define (remove-head op)
      (new this%
           [result (op equation-result (next-operand))]
           [operands (rest equation-operands)]))
    (define/public (get-result) equation-result)
    (define/public (satisfiable?)
      (cond [(null? (cdr equation-operands))
             (equal? equation-result (next-operand))]
            [(not (positive? equation-result)) #f]
            [(not (integer? equation-result)) #f]
            [else (or (send (remove-head -) satisfiable?)
                      (and (not (zero? (next-operand)))
                           (send (remove-head /) satisfiable?)))]))))

(define (read-equation% [in (current-input-port)])
  (let ([match-result (regexp-try-match @pregexp{(\d+):((?: \d+)+)} in)])
    (if match-result
        (new equation%
             [result (bytes->number (second match-result))]
             [operands (reverse (bytes->numbers (third match-result)))])
        #f)))

(define (main)
  (seq:foldl
   + 0
   (seq:map
    (λ (e) (send e get-result))
    (seq:filter (λ (e) (send e satisfiable?))
                (->stream (in-producer read-equation% #f))))))

(with-input-from-file "input" main)
