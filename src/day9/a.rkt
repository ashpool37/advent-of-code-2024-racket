#lang racket

(require racket/generator
         relation/type)

(define (port->fs [in (current-input-port)])
  (define next-block
    (generator
     ()
     (let ([is-free-space #f]
           [file-id 0])
       (for ([c (in-input-port-chars in)]
             #:break (char=? c #\newline))
         (for ([_ (in-range (string->number (string c)))])
           (yield (if is-free-space
                      'free-space
                      file-id)))
         (unless is-free-space (set! file-id (add1 file-id)))
         (set! is-free-space (not is-free-space))))))
  (list->vector (for/list ([block (in-producer next-block (void))]) block)))

(define (defrag! fs)
  (let loop ([store-index 0]
             [read-index (sub1 (vector-length fs))])
    (cond [(>= store-index read-index) (void)]
          [(not (eq? (vector-ref fs store-index) 'free-space))
           (loop (add1 store-index) read-index)]
          [(eq? (vector-ref fs read-index) 'free-space)
           (loop store-index (sub1 read-index))]
          [else
           (vector-set! fs store-index (vector-ref fs read-index))
           (vector-set! fs read-index 'free-space)
           (loop (add1 store-index) (sub1 read-index))])))

(define (checksum fs)
  (for/sum ([(file-id block-number) (in-indexed fs)])
    (* block-number (if (eq? file-id 'free-space) 0 file-id))))

(define (main)
  (let ([fs (port->fs)])
    (defrag! fs)
    (checksum fs)))

(with-input-from-file "input" main)
