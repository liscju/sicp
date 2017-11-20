#lang sicp

; Creates record with specified key and value.
;
; Parameters:
; key - key of record
; value - value of record
;
; Returns:
; record
(define (make-record key value)
  (list key value))

; Gets key of a record.
;
; Parameters:
; record
;
; Returns:
; key of record
(define (record-key record)
  (car record))

; Gets value of a record.
;
; Parameters:
; record
;
; Returns:
; value of a record
(define (record-value record)
  (cdr record))

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

; Finds record with specified key on set of records.
;
; Parameters:
; key - key to find
; records - set of records represented as binary tree
;
; Returns:
; record with specified key
(define (lookup key tree-of-records)
  (cond ((null? tree-of-records) #f)
        ((= key (record-key (entry tree-of-records)))
         (entry tree-of-records))
        ((< key (record-key (entry tree-of-records)))
         (lookup key (left-branch tree-of-records)))
        ((> key (record-key (entry tree-of-records)))
         (lookup key (right-branch tree-of-records)))))


(equal? (lookup 5 nil) #f)
(equal? (lookup 5 (make-tree (make-record 3 "street") nil nil)) #f)
(equal? (lookup 5 (make-tree (make-record 5 "city") nil nil))
        (make-record 5 "city"))
(equal? (lookup 5 (make-tree (make-record 7 "glob")
                             (make-tree (make-record 5 "food") nil nil)
                             nil))
        (make-record 5 "food"))
(equal? (lookup 5 (make-tree (make-record 4 "wardrobe")
                             nil
                             (make-tree (make-record 5 "foot") nil nil)))
        (make-record 5 "foot"))



