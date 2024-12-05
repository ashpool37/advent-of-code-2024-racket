#lang racket

;; (require graph)
(require relation/type
         (prefix-in seq: seq))

(define bytes->number (compose string->number bytes->string/utf-8))

(define (read-printing-rule)
  (let ([match-result
         (regexp-try-match #px"\\s*(\\d{2})\\|(\\d{2})"
                           (current-input-port))])
    (if match-result (map bytes->number (cdr match-result))
        #f)))

(define (read-printing-order)
  (let ([match-result
         (regexp-try-match #px"\\s*((?:\\d{2},)*\\d{2})"
                           (current-input-port))])
    (if match-result
        (map
         string->number
         (string-split (bytes->string/utf-8 (second match-result)) ","))
        #f)))

(define (main)
  (let*
      ([rules (->list (in-producer read-printing-rule #f))]
       [orders (->stream (in-producer read-printing-order #f))]
       [order-correct?
        (λ (order)
          (let ([positions (make-hash)])
            (for ([page (in-list order)]
                  [page-position (in-range (length order))])
              (hash-set! positions page page-position))
            (for/and ([rule rules])
              (let ([first-pos (hash-ref positions (first rule) #f)]
                    [second-pos (hash-ref positions (second rule) #f)])
                (or (not first-pos)
                    (not second-pos)
                    (< first-pos second-pos))))))]
       [order-middle-page
        (λ (order)
          (list-ref order (exact-floor (/ (length order) 2))))])
    (seq:foldl
     + 0
     (seq:map order-middle-page (seq:filter order-correct? orders)))))

(with-input-from-file "input" main)
