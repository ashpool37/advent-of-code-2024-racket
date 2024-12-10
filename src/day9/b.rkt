#lang racket

(require racket/generator
         relation/type)

(struct fs-file (id size) #:transparent)

(struct dcons (value next prev) #:transparent #:mutable)
(define dlist%
  (class object%
    (define head #f)
    (define tail #f)
    (super-new)

    (define/public (get-head) head)
    (define/public (get-tail) tail)
    (define/public (empty?) (not head))
    (define/public (insert! value [prev #f])
      (cond [(empty?)
             (define new-dcons (dcons value #f #f))
             (set! head new-dcons)
             (set! tail new-dcons)
             new-dcons]
            [else
             (define next (if (dcons? prev) (dcons-next prev) head))
             (define new-dcons (dcons value next prev))
             (when (dcons? prev) (set-dcons-next! prev new-dcons))
             (when (dcons? next) (set-dcons-prev! next new-dcons))
             (when (eq? tail prev) (set! tail new-dcons))
             (when (eq? head next) (set! head new-dcons))
             new-dcons]))
    (define/public (push-back! value) (insert! value tail))
    (define/public (push-front! value) (insert! value #f))
    (define/public (remove! cell)
      (define next (dcons-next cell))
      (define prev (dcons-prev cell))
      (when (dcons? prev) (set-dcons-next! prev next))
      (when (dcons? next) (set-dcons-prev! next prev))
      (when (eq? tail cell) (set! tail prev))
      (when (eq? head cell) (set! head next)))
    (define/public (find-if condition?)
      (let loop ([cell head])
        (cond [(not cell) #f]
              [(condition? (dcons-value cell)) cell]
              [else (loop (dcons-next cell))])))))

(define (port->fs [in (current-input-port)])
  (define next-file
    (generator
     ()
     (let ([is-free-space #f]
           [file-id 0])
       (for ([c (in-input-port-chars in)]
             #:break (char=? c #\newline))
         (yield (fs-file (if is-free-space 'free-space file-id)
                         (string->number (string c))))
         (unless is-free-space (set! file-id (add1 file-id)))
         (set! is-free-space (not is-free-space))))))
  (let ([result (new dlist%)])
    (for ([the-file (in-producer next-file (void))])
      (send result push-back! the-file))
    result))

(define (in-fs-blocks fs)
  (define next-block
    (generator
     ()
     (let loop ([cell (send fs get-head)])
       (for ([_ (in-range (fs-file-size (dcons-value cell)))])
         (yield (fs-file-id (dcons-value cell))))
       (when (dcons-next cell) (loop (dcons-next cell))))))
  (in-producer next-block (void)))

(define (defrag! fs)
  (let defrag-loop ([src-cell (send fs get-tail)])
    (define src-file (dcons-value src-cell))
    (define (enough-space? space)
      (and (eq? 'free-space (fs-file-id space))
           (>= (fs-file-size space) (fs-file-size src-file))))
    (when (not (eq? 'free-space (fs-file-id src-file)))
      (define dest
        (let dest-seek-loop ([cell (send fs get-head)])
          (cond [(not cell) #f]
                [(eq? cell src-cell) #f]
                [(enough-space? (dcons-value cell)) cell]
                [else (dest-seek-loop (dcons-next cell))])))
      (when dest
        (define dest-file (dcons-value dest))
        (define free-space-remainder
          (- (fs-file-size dest-file) (fs-file-size src-file)))
        (define new-src-file-cell (send fs insert! src-file dest))
        (when (> free-space-remainder 0)
          (send fs insert! (fs-file 'free-space free-space-remainder)
                new-src-file-cell))
        (send fs insert! (fs-file 'free-space (fs-file-size src-file))
              src-cell)
        (send fs remove! src-cell)
        (send fs remove! dest)))
    (when (dcons-prev src-cell) (defrag-loop (dcons-prev src-cell)))))

(define (checksum fs)
  (for/sum ([(file-id block-number) (in-indexed (in-fs-blocks fs))])
    (* block-number (if (eq? file-id 'free-space) 0 file-id))))

(define (main)
  (let ([fs (port->fs)])
    (defrag! fs)
    (checksum fs)))

(with-input-from-file "input" main)
