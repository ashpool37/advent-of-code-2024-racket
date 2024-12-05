#lang racket

(require relation/type
         graph
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
       [order-tsort
        (λ (order)
          (let ([dep-graph (unweighted-graph/directed null)])
            (for ([page order]) (add-vertex! dep-graph page))
            (for ([rule rules])
              (when (and (has-vertex? dep-graph (first rule))
                         (has-vertex? dep-graph (second rule)))
                (apply add-directed-edge! dep-graph rule)))
            (tsort dep-graph)))]
       [order-middle-page
        (λ (order)
          (list-ref order (exact-floor (/ (length order) 2))))])
    (seq:foldl
     + 0
     (seq:map
      (compose order-middle-page order-tsort)
      (seq:filter (negate order-correct?) orders)))))

(with-input-from-file "input" main)
