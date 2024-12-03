#lang racket

(require (prefix-in seq: seq))

(define bytes->number (compose string->number bytes->string/utf-8))

(define (main)
  (seq:foldl
   + 0
   (seq:map (compose (λ (ns) (apply * (map bytes->number ns)))
                     cdr)
            (sequence->stream
             (in-producer (thunk (regexp-match #px"mul\\((\\d{1,3}),(\\d{1,3})\\)"
                                               (current-input-port)))
                          #f)))))

(with-input-from-file "input" main)
