#lang racket

;; part 1
(define (compute-fuel1 m)
  (- (quotient m 3) 2))
(with-input-from-file "input.txt"
  (λ ()
    (for/sum ([m (in-lines)])
      (compute-fuel1 (string->number m)))))

;; part 2
(define (compute-fuel2 m)
  (define f (compute-fuel1 m))
  (cond
    [(<= f 0) 0]
    [else (+ f (compute-fuel2 f))]))
(with-input-from-file "input.txt"
  (λ ()
    (for/sum ([m (in-lines)])
      (compute-fuel2 (string->number m)))))
