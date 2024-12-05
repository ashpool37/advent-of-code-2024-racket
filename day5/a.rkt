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

(define (read-update)
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
       [updates (->stream (in-producer read-update #f))]
       [update-correct?
        (λ (the-update)
          (let ([positions (make-hash)])
            (for ([(page position) (in-indexed the-update)])
              (hash-set! positions page position))
            (for/and ([rule rules])
              (let ([first-pos (hash-ref positions (first rule) #f)]
                    [second-pos (hash-ref positions (second rule) #f)])
                (or (not first-pos)
                    (not second-pos)
                    (< first-pos second-pos))))))]
       [update-middle-page
        (λ (the-update)
          (list-ref the-update (exact-floor (/ (length the-update) 2))))])
    (seq:foldl
     + 0
     (seq:map update-middle-page (seq:filter update-correct? updates)))))

(with-input-from-file "input" main)
