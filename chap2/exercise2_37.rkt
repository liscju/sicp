#lang sicp

; Accumulates elements of sequence using specified operation.
;
; Accumulate operation is defined as follows:
; (accumulate op initial (list e1 e2)) == (op e1 (op e2 initial))
;
; Parameters:
; op - operation that accumulates sequence elements
; initial - initial accumulate value
; sequence - list of elements
;
; Returns:
; accumulated sequence
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Version of accumulates that works on sequence of sequences.
;
; Each sequence in sequences is expected to have same number
; of elements.
;
; Parameters:
; op - operation that accumulates nested sequence elements
; init - initial nested accumulates values
; segs - sequence of sequences
(define (accumulate-n op init segs)
  (if (null? (car segs))
      nil
      (cons (accumulate op init (map car segs))
            (accumulate-n op init (map cdr segs)))))

; Calculates dot product of two vectors.
;
; Dot product of
;    v=(v1,v2,v3,v4,...)
;    u=(u1,u2,u3,u4,...)
;    v dot u = v1*u1+v2*u2+v3*u3+v4*u4+...
;
; Parameters:
; v,u - vectors of the same size
;
; Returns:
; dot product of specified vectors
(define (dot-product v u)
  (accumulate + 0 (map * v u)))

; Multiplies matrix by vector.
;
; Parameters:
; m - matrix of size RxL
; v - vector of size L
;
; Returns:
; product of matrix multiplied by vector
(define (matrix-*-vector m v)
  (map (lambda (mrow) (dot-product mrow v)) m))

(equal? (list 17 39)
        (matrix-*-vector
         (list (list 1 2) (list 3 4))
         (list 5 6)))

; Transposes matrix.
;
; Parameters:
; mat - matrix
;
; Returns:
; transposed matrix
(define (transpose mat)
  (accumulate-n cons nil mat))

(equal? (list (list 1 3) (list 2 4))
        (transpose (list (list 1 2) (list 3 4))))

; Multiplies specified matrixes.
;
; Parameters:
; m, n - matrixes
;
; Returns:
; result of multiplied matrixes
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mrow) (matrix-*-vector cols mrow)) m)))

(equal? (matrix-*-matrix (list (list 1 2) (list 3 4))
                         (list (list 5 6) (list 7 8)))
        (list (list 19 22) (list 43 50)))





