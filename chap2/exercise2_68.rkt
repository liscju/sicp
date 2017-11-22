#lang sicp

; Huffman tree leaf structure

; Constructs leaf of a huffman tree.
;
; Parameters:
; symbol - symbol in a leaf
; weight - weight of a leaf
;
; Returns:
; leaf
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

; Returns #t when object is a Huffman tree leaf.
;
; Parameters:
; object - object to check
;
; Returns:
; #t when object is a Huffman tree leaf, #f otherwise
(define (leaf? object)
  (eq? (car object) 'leaf))

; Gets symbol of a Huffman tree leaf.
;
; Parameters:
; x - Huffman tree leaf
;
; Returns:
; symbol of a leaf
(define (symbol-leaf x)
  (cadr x))

; Gets weight of a Huffman tree leaf.
;
; Parameters:
; x - Huffman tree leaf
;
; Returns:
; weight of a leaf
(define (weight-leaf x)
  (caddr x))

; Huffman tree node structure

; Constructs node of a Huffman tree.
;
; Parameters:
; left - left branch of a node
; right - right branch of a node
;
; Returns:
; Huffman tree node
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

; Gets left branch of a Huffman tree node.
;
; Parameters:
; tree - Huffman tree node
;
; Returns:
; left branch of a Huffman tree node
(define (left-branch tree)
  (car tree))

; Gets right branch of a Huffman tree node.
;
; Parameters:
; tree - Huffman tree node
;
; Returns:
; right branch of a Huffman tree node
(define (right-branch tree)
  (cadr tree))

; Miscelannous utilities

; Gets symbols used in a Huffman tree element (node or leaf).
;
; Parameters:
; tree - Huffman tree element (node or leaf)
;
; Returns:
; symbols of a Huffman tree element
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

; Gets weight of a Huffman tree element(node or leaf).
;
; Parameters:
; tree - Huffman tree element (node or leaf)
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; Building Huffman tree

; Adds Huffman tree element to set of Huffman tree elements.
;
; Set is kept as list of ordered Huffman tree elements according
; to its weight in ascending order.
;
; Parameters:
; x - Huffman tree element
; set - list of ordered Huffman tree elements
;
; Returns:
; set with added Huffman tree element
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; Creates initial Huffman tree elements set to construct
; Huffman tree.
;
; Parameters:
; pairs - list of pair of (symbol, weight)
;
; Returns:
; 
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

; Coding/Decoding

; Decodes bits using Huffman tree.
;
; Parameters:
; bits - bits to decode
; tree - Huffman tree
;
; Returns:
; decoded bits
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

; Chooses next branch to jump in according to specified input bit.
;
; Parameters:
; bit - current bit
; branch - current branch
;
; Returns:
; next branch to jump in
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "Zla wartosc bitu -- CHOOSE-BRANCH" bit))))


; Exercise

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;(decode sample-message sample-tree)
; A D A B B C A

; Encodes message using specified huffman tree.
;
; Parameters:
; message - message to encode
; tree - huffman tree
;
; Returns:
; encoded message
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; Encodes symbol using specified huffman tree.
;
; Parameters:
; symbol - symbol to encode
; tree - huffman tree
;
; Returns:
; encoded symbol
(define (encode-symbol symbol tree)
  (cond ((not (contains? (symbols tree) symbol))
         (error "There is no such an encoding"))
        ((leaf? tree) nil)
        ((contains? (symbols (left-branch tree)) symbol)
         (cons '0 (encode-symbol symbol (left-branch tree))))
        ((contains? (symbols (right-branch tree)) symbol)
         (cons '1 (encode-symbol symbol (right-branch tree))))))

; Checks if symbols contains specified symbol.
;
; Parameters:
; symbols - list of symbol
; symbol - symbol to check
;
; Returns:
; #t when symbols contains symbol, #f otherwise
(define (contains? symbols symbol)
  (cond ((null? symbols) #f)
        ((eq? (car symbols) symbol) #t)
        (else (contains? (cdr symbols) symbol))))

(equal? (encode (decode sample-message sample-tree) sample-tree)
        sample-message)









