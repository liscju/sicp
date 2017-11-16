#lang sicp

; Gets value of root node of a tree.
;
; Parameters:
; tree - tree to get root node value
;
; Returns:
; value of rood node
(define (entry tree)
  (car tree))

; Gets left branch of a tree.
;
; Parameters:
; tree - tree to get left branch
;
; Returns:
; left branch
(define (left-branch tree)
  (cadr tree))

; Gets right branch of a tree.
;
; Parameters:
; tree - tree to get right branch
;
; Returns:
; right branch
(define (right-branch tree)
  (caddr tree))

; Constructs tree from entry, left and right branch.
;
; Parameters:
; entry - value of root node
; left - left branch
; right - right branch
;
; Returns:
; constructed tree
(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                   (right-result (partial-tree (cdr non-left-elts)
                                               right-size)))
                  (let ((right-tree (car right-result))
                        (remaining-elts (cdr right-result)))
                    (cons (make-tree this-entry left-tree right-tree)
                          remaining-elts))))))))

; A)

; partial-tree
; 1) divide elts into two list left part, root element and right part
; 2) create partial-tree from left and right part
; 3) join left part, root and right part into tree


(list->tree (list 1 3 5 7 9))

(cons
 5
 (cons
  (cons 1 (cons '() (cons (cons 3 (cons '() (cons '() '()))) '())))
  (cons (cons 7 (cons '() (cons (cons 9 (cons '() (cons '() '()))) '()))) '())))

;                   5
;                  / \
;                 /   \
;                1     7
;                 \     \
;                  3     9

; B)

; T(N) = T(N/2) + T(N/2) + O(1)
; T(N) = 2*T(N/2) + O(1)
; T(N) = 2*(2*T(N/4) + O(1)) + O(1)
; T(N) = 4*T(N/4) + 3*O(1)
; T(N) = O(N)
