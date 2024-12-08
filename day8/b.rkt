#lang racket

(require relation/type
         (prefix-in seq: seq))

(struct vec2d (x y) #:transparent)
(define (vec2d+ a b)
  (vec2d (+ (vec2d-x a) (vec2d-x b))
         (+ (vec2d-y a) (vec2d-y b))))
(define (vec2d- a b)
  (vec2d (- (vec2d-x a) (vec2d-x b))
         (- (vec2d-y a) (vec2d-y b))))
(define (vec2d* v n)
  (vec2d (* (vec2d-x v) n)
         (* (vec2d-y v) n)))

(define (read-antenna-map [in (current-input-port)])
  (let ([antenna-locations (make-hash)]
        [dim-x 0]
        [dim-y 0])
    (for ([(line y) (in-indexed (in-lines in))])
      (set! dim-y (max (add1 y) dim-y))
      (for ([(freq x) (in-indexed (in-string line))])
        (set! dim-x (max (add1 x) dim-x))
        (when (not (char=? freq #\.))
          (hash-update! antenna-locations
                        freq
                        (curry cons (vec2d x y))
                        null))))
    (values antenna-locations dim-x dim-y)))

(define (antinodes a1 a2 in-bounds?)
  (let* ([delta (vec2d- a2 a1)]
         [negative-antinodes
          (for/stream ([i (in-naturals 1)])
            (define antinode (vec2d+ a1 (vec2d* delta (- i))))
            #:break (not (in-bounds? antinode))
            antinode)]
         [non-negative-antinodes
          (for/stream ([i (in-naturals)])
            (define antinode (vec2d+ a1 (vec2d* delta i)))
            #:break (not (in-bounds? antinode))
            antinode)])
    (->list (seq:append negative-antinodes non-negative-antinodes))))

(define (main)
  (let*-values
      ([(antenna-locations dim-x dim-y) (read-antenna-map)]
       [(in-bounds?)
        (λ (v) (match v [(vec2d x y) (and (<= 0 x) (<= 0 y)
                                          (< x dim-x) (< y dim-y))]))]
       [(antenna-pairs)
        (seq:append-map (λ (antennas) (->stream (in-combinations antennas 2)))
                        (->stream (in-hash-values antenna-locations)))]
       [(antinodes)
        (seq:deduplicate
         (seq:append-map
          (λ (antenna-pair) (apply (curryr antinodes in-bounds?) antenna-pair))
          antenna-pairs))])
    (seq:length antinodes)))

(with-input-from-file "input" main)
