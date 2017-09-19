#lang sicp

; Squares each element of the tree recursively.
;
; Parameters:
; t - tree
;
; Returns:
; new tree with each element of the specified tree squared
(define (square-tree-rec t)
  (cond ((null? t) nil)
        ((not (pair? t)) (* t t))
        (else
         (let ((head (car t))
               (tail (cdr t)))
           (cons (square-tree-rec head)
                 (square-tree-rec tail))))))

(equal? (square-tree-rec nil) nil)
(equal? (square-tree-rec (list 1)) (list 1))
(equal? (square-tree-rec (list 1 2)) (list 1 4))
(equal? (square-tree-rec (list (list 1 2) 3 4))
                      (list (list 1 4) 9 16))
(equal? (square-tree-rec (list (list 2) (list 3)))
                      (list (list 4) (list 9)))

; Squares each element of the tree using map.
;
; Parameters:
; tree
;
; Returns:
; new tree with each element of the specified tree squared
(define (square-tree-map tree)
  (map (lambda (subtree)
         (if (not (pair? subtree))
             (* subtree subtree)
             (square-tree-map subtree)))
       tree))

(equal? (square-tree-map nil) nil)
(equal? (square-tree-map (list 1)) (list 1))
(equal? (square-tree-map (list 1 2)) (list 1 4))
(equal? (square-tree-map (list (list 1 2) 3 4))
                         (list (list 1 4) 9 16))
(equal? (square-tree-map (list (list 2) (list 3)))
                         (list (list 4) (list 9)))