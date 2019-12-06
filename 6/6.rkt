#lang racket
(require graph)

;; part 1

(displayln "PART 1: ---------------------")

(define SRC "COM")

(with-input-from-file "input.txt"
  (lambda ()
    (define omap
      (unweighted-graph/directed
       (for/list ([orb (in-lines)])
         (string-split orb ")"))))

    (displayln "using Bellman-Ford:")
    (define-values (h1 π1)
      (time 
       (bellman-ford omap SRC)))
    (displayln (apply + (hash-values h1)))
    
    (displayln "using Dijkstra:")
    (define-values (h2 π2)
      (time 
       (dijkstra omap SRC)))
    (displayln (apply + (hash-values h2)))
    
    (displayln "using DAG:")
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
    (displayln "using BFS:")
    (time
     (for/sum ([v (in-vertices omap)])
       ; ignoring possible #f from fewest-vertices-path
       (sub1 (length (fewest-vertices-path omap v SRC)))))))

;; part 2

(displayln "PART 2: ---------------------")
(define YOU "YOU")
(define SAN "SAN")
(with-input-from-file "input.txt"
  (lambda ()
    (define omap
      (unweighted-graph/undirected
       (for/list ([orb (in-lines)])
         (string-split orb ")"))))
    ; subtract: 2 endpoints, and the orbit you're already on
    (- (length (fewest-vertices-path omap YOU SAN)) 3)))
