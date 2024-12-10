#lang racket

(require memo
         relation/type
         racket/generator
         (prefix-in seq: seq))

(struct vec2d (x y) #:transparent)
(define (vec2d+ a b)
  (vec2d (+ (vec2d-x a) (vec2d-x b))
         (+ (vec2d-y a) (vec2d-y b))))
(define dir-up (vec2d 0 -1))
(define dir-right (vec2d 1 0))
(define dir-down (vec2d 0 1))
(define dir-left (vec2d -1 0))
(define dirs-all (list dir-up dir-right dir-down dir-left))

(define (port->topography [in (current-input-port)])
  (for/vector ([line (in-lines in)])
    (->vector
     (seq:map (compose string->number string)
              (string->immutable-string line)))))

(define (get-height topography pos)
  (match pos
    [(vec2d x y) (vector-ref (vector-ref topography y) x)]))

(define (trailhead? topography pos)
  (= 0 (get-height topography pos)))

(define (in-bounds? topography pos)
    (let ([topo-height (vector-length topography)]
          [topo-width (vector-length (vector-ref topography 0))]
          [x (vec2d-x pos)]
          [y (vec2d-y pos)])
      (and (<= 0 x) (<= 0 y)
           (< x topo-width) (< y topo-height))))

(define (in-positions topography)
  (define topo-height (vector-length topography))
  (define topo-width (vector-length (vector-ref topography 0)))
  (define coords-gen
    (generator
     ()
     (for ([y (in-range topo-height)])
       (for ([x (in-range topo-width)])
         (yield (vec2d x y))))))
  (->stream (in-producer coords-gen (void))))

(define (make-trail-rating-evaluator topography)
  (define/memoize (evaluate-fn trail-start-pos)
    (cond
      [(= 9 (get-height topography trail-start-pos)) 1]
      [else
       (for/sum ([dir dirs-all])
         (define trail-continuation (vec2d+ trail-start-pos dir))
         (cond [(not (in-bounds? topography trail-continuation)) 0]
               [(not (= 1 (- (get-height topography trail-continuation)
                             (get-height topography trail-start-pos))))
                0]
               [else (evaluate-fn trail-continuation)]))]))
  evaluate-fn)

(define (main)
  (let* ([topography (port->topography)]
         [get-rating (make-trail-rating-evaluator topography)])
    (seq:foldl
     + 0
     (seq:map get-rating
              (seq:filter (curry trailhead? topography)
                          (in-positions topography))))))

(with-input-from-file "input" main)
