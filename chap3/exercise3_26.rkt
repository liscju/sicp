#lang sicp

; record = (cons key value)

(define (create-list record list)
  (cons record list))

(define (assoc-list key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc-list key (cdr records)))))

; Tree declarations

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
(define (lookup-set key tree-of-records)
  (cond ((null? tree-of-records) #f)
        ((= key (record-key (entry tree-of-records)))
         (entry tree-of-records))
        ((< key (record-key (entry tree-of-records)))
         (lookup-set key (left-branch tree-of-records)))
        ((> key (record-key (entry tree-of-records)))
         (lookup-set key (right-branch tree-of-records)))))

(define (adjoin-set x set)
  (display set)
  (newline)
  (cond ((null? set) (make-tree x '() '()))
        ((= (record-key x) (record-key (entry set))) set)
        ((< (record-key x) (record-key (entry set)))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> (record-key x) (record-key (entry set)))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

; End of tree declarations

(define (make-table assoc create)
  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            false)))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (create (cons key value) (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Not recognized operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table lookup-set adjoin-set))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))






