#lang racket

(require graph)

(define START (list #\A #\A))
(define END (list #\Z #\Z))

(define (node? ch) (char=? #\. ch))

(define (maze-port-shortest-path [in (current-input-port)])
  (define lines (port->lines in))
  (define G (unweighted-graph/undirected null))
  ;; find all nodes
  (define the-maze
    (for*/hash ([(l i) (in-indexed (in-list lines))]
                [(c j) (in-indexed (in-string l))])
      (values (cons i j) c)))
  (define nodes
    (for/set ([(x+y c) (in-hash the-maze)] #:when (node? c))
      x+y))
  ;; add all nodes to graph
  (for ([v (in-set nodes)])
    (define x (car v))
    (define y (cdr v))
    (define a (cons (add1 x) y))
    (define b (cons (sub1 x) y))
    (define c (cons x (add1 y)))
    (define d (cons x (sub1 y)))
    (when (set-member? nodes a) (add-edge! G v a))
    (when (set-member? nodes b) (add-edge! G v b))
    (when (set-member? nodes c) (add-edge! G v c))
    (when (set-member? nodes d) (add-edge! G v d)))
  ;; find portals; add them as new edges
  ;; potrals hash = map node to alternate name
  (define portals (make-hash))
  (for ([(x+y c) (in-hash the-maze)] #:when (char-alphabetic? c))
    (define x (car x+y))
    (define y (cdr x+y))
    (define maybe-2nd-char1 (cons x (add1 y))) ; horiz portal names
    (define maybe-2nd-char2 (cons (add1 x) y)) ; vert portal names
    (cond
      [(and (hash-has-key? the-maze maybe-2nd-char1) ; horiz portal names
            (char-alphabetic? (hash-ref the-maze maybe-2nd-char1)))
       (define portal (list c (hash-ref the-maze maybe-2nd-char1)))
       ;; the portal's node maybe to the left or right
       (define maybe-node1 (cons x (+ 2 y)))
       (define maybe-node2 (cons x (sub1 y)))
       (when (and (hash-has-key? the-maze maybe-node1)
                  (node? (hash-ref the-maze maybe-node1)))
         (if (hash-has-key? portals portal) ; if 2nd part of portal
             (add-edge! G (hash-ref portals portal) maybe-node1) ; add edge
             (hash-set! portals portal maybe-node1))) ; else just record
       (when (and (hash-has-key? the-maze maybe-node2)
                  (node? (hash-ref the-maze maybe-node2)))
         (if (hash-has-key? portals portal)
             (add-edge! G (hash-ref portals portal) maybe-node2)
             (hash-set! portals portal maybe-node2)))]
      [(and (hash-has-key? the-maze maybe-2nd-char2) ; vert portal names
            (char-alphabetic? (hash-ref the-maze maybe-2nd-char2)))
       (define portal (list c (hash-ref the-maze maybe-2nd-char2)))
       ;; the portal's node may be above or below
       (define maybe-node1 (cons (+ 2 x) y))
       (define maybe-node2 (cons (sub1 x) y))
       (when (and (hash-has-key? the-maze maybe-node1)
                  (node? (hash-ref the-maze maybe-node1)))
         (if (hash-has-key? portals portal)
             (add-edge! G (hash-ref portals portal) maybe-node1)
             (hash-set! portals portal maybe-node1)))
       (when (and (hash-has-key? the-maze maybe-node2)
                  (node? (hash-ref the-maze maybe-node2)))
         (if (hash-has-key? portals portal)
             (add-edge! G (hash-ref portals portal) maybe-node2)
             (hash-set! portals portal maybe-node2)))]))
  (sub1 ; dont include the start
   (length
    (fewest-vertices-path G (hash-ref portals START) (hash-ref portals END)))))

(require rackunit)

(check-equal?
 (with-input-from-file "example-large.txt"
   (λ () (maze-port-shortest-path)))
 58)

(with-input-from-file "input.txt"
  (λ () (maze-port-shortest-path)))
