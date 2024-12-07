#lang racket

(require racket/undefined)

(struct vec2d (x y) #:transparent)

(define (vec2d+ a b)
  (vec2d (+ (vec2d-x a) (vec2d-x b))
         (+ (vec2d-y a) (vec2d-y b))))

(define (vec2d-rotate-90cw vec)
  (vec2d (- 0 (vec2d-y vec)) (vec2d-x vec)))

(define dir-up (vec2d 0 -1))
(define dir-right (vec2d 1 0))
(define dir-down (vec2d 0 1))
(define dir-left (vec2d -1 0))

(define (make-visited-matrix dim-x dim-y)
  (build-vector
   dim-y
   (thunk* (build-vector
            dim-x
            (thunk* (make-hash (list (cons dir-up #f)
                                     (cons dir-right #f)
                                     (cons dir-down #f)
                                     (cons dir-left #f))))))))

(define (visited? mtx pos [dir #f])
  (let* ([x (vec2d-x pos)]
         [y (vec2d-y pos)]
         [dirs-hash (vector-ref (vector-ref mtx y) x)])
    (if dir
        (hash-ref dirs-hash dir)
        (for/or ([(_ visited) (in-hash dirs-hash)]) visited))))

(define (visit! mtx pos dir)
  (let* ([x (vec2d-x pos)]
         [y (vec2d-y pos)]
         [dirs-hash (vector-ref (vector-ref mtx y) x)])
    (hash-set! dirs-hash dir #t)))

(define lab%
  (class object%
    (init interior pos dir [visited #f])
    (define lab-interior interior)
    (define lab-dim-y (vector-length lab-interior))
    (define lab-dim-x (vector-length (vector-ref lab-interior 0)))
    (define guard-visited
      (or visited (make-visited-matrix lab-dim-x lab-dim-y)))
    (define guard-pos pos)
    (define guard-dir dir)
    (visit! guard-visited guard-pos guard-dir)
    (super-new)
    
    (define/public (clone)
      (new this%
           [interior
            (build-vector
             lab-dim-y
             (λ (y) (vector-copy (vector-ref lab-interior y))))]
           [pos guard-pos]
           [dir guard-dir]
           [visited
            (build-vector
             lab-dim-y
             (λ (y) (build-vector
                     lab-dim-x
                     (λ (x) (hash-copy (vector-ref (vector-ref guard-visited y)
                                                   x))))))]))
    (define/public (ref pos)
      (match pos
        [(vec2d x y) (vector-ref (vector-ref lab-interior y) x)]))
    (define/public (in-bounds? pos)
      (match pos
        [(vec2d x y) (and (<= 0 x) (<= 0 y)
                          (< x lab-dim-x) (< y lab-dim-y))]))
    (define/public (free? pos) (eq? 'free-space (ref pos)))
    (define/public (facing-pos) (vec2d+ guard-pos guard-dir))
    (define/public (facing-obstacle?) (not (free? (facing-pos))))
    (define/public (facing-edge?) (not (in-bounds? (facing-pos))))
    (define/public (facing-visited?)
      (visited? guard-visited (facing-pos)))
    (define/public (facing-loop?)
      (visited? guard-visited (facing-pos) guard-dir))
    (define/public (put-obstacle-ahead!)
      (match (facing-pos)
        [(vec2d x y) (vector-set! (vector-ref lab-interior y) x 'obstacle)]))
    (define/public (advance!)
      (cond [(facing-edge?) #f]
            [else (if (facing-obstacle?)
                      (set! guard-dir (vec2d-rotate-90cw guard-dir))
                      (set! guard-pos (vec2d+ guard-pos guard-dir)))
                  (visit! guard-visited guard-pos guard-dir)
                  #t]))
    (define/public (has-loop!?)
      (let loop ()
        (cond [(facing-edge?) #f]
              [(facing-loop?) #t]
              [else (advance!) (loop)])))))

(define (port->lab% [in (current-input-port)])
  (let* ([start-pos undefined]
         [interior
          (for/vector ([(line y) (in-indexed (in-lines in))])
            (for/vector ([(char x) (in-indexed (in-string line))])
              (match char
                [#\. 'free-space]
                [#\^ (set! start-pos (vec2d x y)) 'free-space]
                [#\# 'obstacle])))])
    (new lab% [interior interior] [pos start-pos] [dir dir-up])))

(define (main)
  (let ([lab (port->lab%)])
    (let loop ([placement-count 0])
      (cond [(send lab facing-edge?) placement-count]
            [(send lab facing-loop?) placement-count]
            [(send lab facing-visited?)
             (send lab advance!)
             (loop placement-count)]
            [(send lab facing-obstacle?)
             (send lab advance!)
             (loop placement-count)]
            [else  ; facing a free unvisited space in bounds
             (let ([new-lab (send lab clone)])
               (send new-lab put-obstacle-ahead!)
               (send lab advance!)
               (if (send new-lab has-loop!?)
                   (loop (add1 placement-count))
                   (loop placement-count)))]))))

(with-input-from-file "input" main)
