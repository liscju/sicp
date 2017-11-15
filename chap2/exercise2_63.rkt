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

; Returns true when set contains element.
;
; Parameters:
; x - element to search for
; set - set to search
;
; Returns:
; true when set contains element, false otherwise
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

; Adds element to set.
;
; Parameters:
; x - element to add
; set - set to add element to
;
; Returns:
; set with added element
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

; Converts tree to list.
;
; Parameters:
; tree - tree to convert
;
; Returns:
; list
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

; Z(N) = Z(N/2) + Z(N/2) + O(n)

; Converts tree to list
;
; Parameters:
; tree - tree to convert
;
; Returns:
; list
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

; Z(N) = 2*Z(N/2) + O(1)

(define example-1 (list 7
                        (list 3 (list 1 '() '()) (list 5 '() '()))
                        (list 9 '() (list 11 '() '()))))

(define example-2 (list 3
                        (list 1 '() '())
                        (list 7
                              (list 5 '() '())
                              (list 9 '() (list 11 '() '())))))

(define example-3 (list 5
                        (list 3
                              (list 1 '() '())
                              '())
                        (list 9
                              (list 7 '() '())
                              (list 11 '() '()))))

; A)

(equal? (tree->list-1 example-1) (tree->list-2 example-1))
(tree->list-1 example-1)
; (mcons 1 (mcons 3 (mcons 5 (mcons 7 (mcons 9 (mcons 11 '()))))))

(equal? (tree->list-1 example-2) (tree->list-2 example-2))
(tree->list-1 example-2)
; (mcons 1 (mcons 3 (mcons 5 (mcons 7 (mcons 9 (mcons 11 '()))))))

(equal? (tree->list-1 example-3) (tree->list-2 example-3))
(tree->list-1 example-3)
; (mcons 1 (mcons 3 (mcons 5 (mcons 7 (mcons 9 (mcons 11 '()))))))

; They give the same results - tree entries sorted in ascending order


; B)

; First
; T(N) = 2*T(N/2) + O(N/2)
; T(N) = O(n * log(n))

; Second
; T(N) = 2*T(N/2) + O(1)
; T(N) = O(N)

; Second has less 


