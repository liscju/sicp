#lang sicp

; Constructs mobile construction from left and right arm.
;
; Constructs contains left and right arm. Each of arms
; contains weight (represented with int) or branch (represented
; with branch structure).
;
; Parameters:
; left  - left arm
; right - right arm
(define (make-mobile left right)
  (list left right))

; Gets left branch of the mobile construction.
;
; Parameters:
; m - mobile construction
;
; Returns:
; left branch of the mobile construction
(define (left-branch m)
  (car m))

; Gets right branch of the mobile construction.
;
; Parameters:
; m - mobile construction
;
; Returns:
; right branch of the mobile construction
(define (right-branch m)
  (car (cdr m)))

; Constructs branch from specified length and structure.
;
; Branch has assigned length and structure. Structure can
; be weight (represented with int) or another mobile construction.
;
; Parameters;
; length    - length of the arm
; structure - structure attached to arm
(define (make-branch length structure)
  (list length structure))

; Gets length of the branch.
;
; Parameters:
; b - branch
;
; Returns:
; length of the branch
(define (branch-length b)
  (car b))

; Gets structure of the branch.
;
; Parameters:
; b - branch
;
; Returns:
; structure of the branch
(define (branch-structure b)
  (car (cdr b)))

; Calculates total weight of the mobile construction.
;
; Parameters:
; m - mobile construction
;
; Returns:
; total weight of the mobile construction
(define (total-weight m)
  (define (branch-weight b)
    (let ((structure (branch-structure b)))
      (if (not (pair? structure))
          structure
          (total-weight structure))))
  (+ (branch-weight (left-branch m))
     (branch-weight (right-branch m))))

(= 25 (total-weight (make-mobile (make-branch 5 10) (make-branch 5 15))))
(= 60 (total-weight (make-mobile
                     (make-branch 5 (make-mobile (make-branch 5 10) (make-branch 5 20)))
                     (make-branch 5 30))))


; Calculates torque for specified branch.
;
; torque(branch) = length(branch) * branch_weight(branch)
;
; Parameters:
; b - branch
;
; Returns:
; torque value
(define (torque b)
  (define (branch-weight b)
    (let ((structure (branch-structure b)))
      (if (not (pair? structure))
          structure
          (total-weight structure))))
  (* (branch-length b) (branch-weight b)))

; Checks if mobile construction is balanced.
;
; Mobile construction is balanced when for all construction
; inside it torque for left branch equals torque for right branch.
;
; Parameters:
; m - mobile construction
;
; Returns:
; true when mobile construction is balanced, false otherwise
(define (is-balanced? m)
  (define (is-construction-in-branch-balanced? b)
    (if (not (pair? (branch-structure b)))
        #t
        (is-balanced? (branch-structure b))))
  (and (= (torque (left-branch m)) (torque (right-branch m))))
       (is-construction-in-branch-balanced? (left-branch m))
       (is-construction-in-branch-balanced? (right-branch m)))

(if (is-balanced? (make-mobile (make-branch 5 10) (make-branch 5 10))) #t #f)
(if (is-balanced? (make-mobile (make-branch 3 5)
                               (make-branch 15 1)))
    #t
    #f)
(if (is-balanced? (make-mobile (make-branch 3 5)
                               (make-branch 15 (make-mobile (make-branch 3 5) (make-branch 15 1)))))
    #t
    #f)

