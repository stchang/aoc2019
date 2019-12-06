#lang racket
(require graph)

; res: 227612

(define SRC "COM")

(with-input-from-file "input.txt"
  (lambda ()
    (define omap
      (unweighted-graph/directed
       (for/list ([orb (in-lines)])
         (string-split orb ")"))))
    
    (define-values (h1 π1)
      (time 
       (bellman-ford omap SRC)))
    (displayln (apply + (hash-values h1)))
    
    (define-values (h2 π2)
      (time 
       (dijkstra omap SRC)))
    (displayln (apply + (hash-values h2)))
    
    (define-values (h3 π3)
      (time 
       (dag-shortest-paths omap SRC)))
    (displayln (apply + (hash-values h3)))))

(with-input-from-file "input.txt"
  (lambda ()
    (define omap
      (unweighted-graph/directed
       (for/list ([orb (in-lines)])
         (reverse
          (string-split orb ")")))))
    (time
     (for/sum ([v (in-vertices omap)])
       ; ignoring possible #f from fewest-vertices-path
       (sub1 (length (fewest-vertices-path omap v SRC)))))))
