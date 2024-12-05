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

(struct update (page-order dep-graph))

(define (update-correct? the-update)
  (let ([positions (make-hash)])
    (for ([(page page-position)
           (in-indexed (update-page-order the-update))])
      (hash-set! positions page page-position))
    (for/and ([rule (in-edges (update-dep-graph the-update))])
      (let ([first-pos (hash-ref positions (first rule) #f)]
            [second-pos (hash-ref positions (second rule) #f)])
        (or (not first-pos)
            (not second-pos)
            (< first-pos second-pos))))))

(define (update-tsort the-update)
 (update (tsort (update-dep-graph the-update))
         (graph-copy (update-dep-graph the-update))))

(define (update-middle-page the-update)
 (list-ref (update-page-order the-update)
           (exact-floor (/ (length (update-page-order the-update))
                           2))))

(define (main)
  (let*
      ([rules (->list (in-producer read-printing-rule #f))]
       [read-update
        (thunk
         (let* ([match-result (regexp-try-match #px"\\s*((?:\\d{2},)*\\d{2})"
                                                (current-input-port))])
           (if (not match-result) #f
               (let*
                   ([capture (second match-result)]
                    [tokens (string-split (bytes->string/utf-8 capture) ",")]
                    [page-order (map string->number tokens)]
                    [dep-graph (unweighted-graph/directed null)])
                 (for ([page page-order]) (add-vertex! dep-graph page))
                 (for ([rule rules])
                   (when (and (has-vertex? dep-graph (first rule))
                              (has-vertex? dep-graph (second rule)))
                     (apply add-directed-edge! dep-graph rule)))
                 (update page-order dep-graph)))))]
       [updates (->stream (in-producer read-update #f))])
    (seq:foldl
     + 0
     (seq:map (compose update-middle-page update-tsort)
              (seq:filter (negate update-correct?) updates)))))

(with-input-from-file "input" main)
