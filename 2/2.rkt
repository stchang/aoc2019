#lang racket

;; part 1
(define PROG
  (with-input-from-file "input.txt"
    (λ ()
      (list->vector
       (map (compose string->number string-trim) (string-split (port->string) ","))))))

(define (run PROG PC [in1 (vector-ref PROG 1)]
                     [in2 (vector-ref PROG 2)])

  ;; restore gravity assist program
  (vector-set! PROG 1 in1)
  (vector-set! PROG 2 in2)

  ;; shorthand for PROG lookup
  (define (ref pos) (vector-ref PROG pos))
  
  (let L ([op (ref PC)])
    (define (next)
      (set! PC (+ PC 4))
      (L (ref PC)))
    (case op
      [(1) ; add
       (define x (ref (ref (+ PC 1)))) ; dont read and write simultaneously
       (define y (ref (ref (+ PC 2))))
       (define out (ref (+ PC 3))) ; output addr
       (vector-set! PROG out (+ x y))
       (next)]
      [(2) ; mul
       (define x (ref (ref (+ PC 1)))) ; dont read and write simultaneously
       (define y (ref (ref (+ PC 2))))
       (define out (ref (+ PC 3))) ; output addr
       (vector-set! PROG out (* x y))
       (next)]
      [(99) (ref 0)] ; halt
      [else -1]))) ; err


(require rackunit)
(check-equal? (run (vector 1 0 0 0 99) 0) 2)

(check-equal? (run PROG 0 12 2) 5434663)

;; part 2

(define grav 19690720)
(for*/first ([noun 100] [verb 100]
             #:when 
             (let ([PROG ; must reread the prog to reset it
                    (with-input-from-file "input.txt"
                      (λ ()
                        (list->vector
                         (map (compose string->number string-trim) (string-split (port->string) ",")))))])
               (= (run PROG 0 noun verb) grav)))
  (+ (* 100 noun) verb))
