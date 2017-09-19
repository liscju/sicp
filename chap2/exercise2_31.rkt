#lang sicp

; Iterates through leafs of the tree and transform then with given
; function.
;
; Parameters:
; fun - leaves transformation function
; tree - tree to iterate
;
; Returns:
; new tree that leaves are transformed version of specified tree
(define (tree-map fun tree)
  (map (lambda (subtree)
         (if (not (pair? subtree))
             (fun subtree)
             (tree-map fun subtree)))
       tree))

; Iterates through specified tree and squares values in leaves.
;
; Parameters:
; tree - tree to iterate
;
; Returns:
; new tree that leaves are squared version of the specified tree
(define (square-tree-map tree)
  (define (square x) (* x x))
  (tree-map square tree))

(equal? (square-tree-map nil) nil)
(equal? (square-tree-map (list 1)) (list 1))
(equal? (square-tree-map (list 1 2)) (list 1 4))
(equal? (square-tree-map (list (list 1 2) 3 4))
                         (list (list 1 4) 9 16))
(equal? (square-tree-map (list (list 2) (list 3)))
                         (list (list 4) (list 9)))