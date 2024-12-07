#lang racket

(require relation/type
         (prefix-in seq: seq))

(struct vec2d (x y) #:transparent)

(define (vec2d+ a b)
  (vec2d (+ (vec2d-x a) (vec2d-x b))
         (+ (vec2d-y a) (vec2d-y b))))

(define (vec2d-rotate-90cw vec)
  (vec2d (- 0 (vec2d-y vec)) (vec2d-x vec)))

(define (main)
  (let*
      ([lab-map (->vector (in-lines))]
       [lab-dim-y (seq:length lab-map)]
       [lab-dim-x (seq:length (vector-ref lab-map 0))]
       [lab-ref
        (λ (vec)
          (string-ref (vector-ref lab-map (vec2d-y vec)) (vec2d-x vec)))]
       [lab-visit!
        (λ (vec)
          (let ([prev-value (lab-ref vec)])
            (string-set! (vector-ref lab-map (vec2d-y vec)) (vec2d-x vec) #\X)
            (not (char=? prev-value (lab-ref vec)))))]
       [lab-in-bounds-at?
        (λ (vec)
          (and (<= 0 (vec2d-x vec)) (<= 0 (vec2d-y vec))
               (< (vec2d-x vec) lab-dim-x) (< (vec2d-y vec) lab-dim-y)))]
       [lab-free-at? (λ (vec) (not (char=? #\# (lab-ref vec))))]
       [lab-starting-position
        (for/or ([y (in-range lab-dim-y)])
          (for/or ([x (in-range lab-dim-x)])
            (let ([vec (vec2d x y)])
              (and (eq? #\^ (lab-ref vec)) vec))))])
    (lab-visit! lab-starting-position)
    (let loop ([visited-tile-count 1]
               [guard-position lab-starting-position]
               [guard-look-direction (vec2d 0 -1)])
      (let ([next-position (vec2d+ guard-position guard-look-direction)])
        (cond [(not (lab-in-bounds-at? next-position)) visited-tile-count]
              [(not (lab-free-at? next-position))
               (loop visited-tile-count
                     guard-position
                     (vec2d-rotate-90cw guard-look-direction))]
              [else
               (loop (if (lab-visit! next-position) (add1 visited-tile-count)
                         visited-tile-count)
                     next-position
                     guard-look-direction)])))))

(with-input-from-file "input" main)
