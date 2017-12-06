#lang sicp

; Primitives used later(put/get)
(define table (list ))
(define (put op type proc)
  (set! table (append table (list (list op type proc)))) 
)
(define (get op type) 
  (define (search op type t) 
   (cond ((null? t) #f) 
    ((and (eqv? (caar t) op) (eqv? (cadar t) type)) 
         (caddar t)) 
    (else (search op type (cdr t))))) 
 (search op type table) 
)
; End of primitives

; Register in Seattle

(define (install-seattle-register)
  (define register
    (list (list 'miller 500) (list 'smith 1000)))
  (define (get-record surname)
    (define (get-record-from-reg reg name)
      (cond ((null? reg) #f)
            ((eq? (caar reg) name) (car reg))
            (else (get-record-from-reg (cdr reg) name))))
    (get-record-from-reg register surname))
  (define (get-salary surname)
    (let ((record (get-record surname)))
      (cond ((eq? record #f) #f)
            (else (cadr record)))))
  (put 'get-record 'seattle get-record)
  (put 'get-salary 'seattle get-salary))

; Register in New York


(define (install-new-york-register)
  (define register
    (list (list 'williams 100) (list 'anderson 400)))
  (define (get-record surname)
    (define (get-record-from-reg reg name)
      (cond ((null? reg) #f)
            ((eq? (caar reg) name) (car reg))
            (else (get-record-from-reg (cdr reg) name))))
    (get-record-from-reg register surname))
  (define (get-salary surname)
    (let ((record (get-record surname)))
      (cond ((eq? record #f) #f)
            (else (cadr record)))))
  (put 'get-record 'new-york get-record)
  (put 'get-salary 'new-york get-salary))

; Installing registers
(install-seattle-register)
(install-new-york-register)

; A)
(define (get-record register emp)
  (let ((get-record-from-reg (get 'get-record register)))
    (cond ((eq? get-record-from-reg #f) #f)
          (else (get-record-from-reg emp)))))

; > (get-record 'new-york 'anderson)
; (mcons 'anderson (mcons 400 '()))
; > (get-record 'new-york 'williams)
; (mcons 'williams (mcons 100 '()))
; > (get-record 'new-york 'miller)
; #f

; B)

(define (get-salary register emp)
  (let ((get-salary-from-reg (get 'get-salary register)))
    (cond ((eq? get-salary-from-reg #f) #f)
          (else (get-salary-from-reg emp)))))

; > (get-salary 'seattle 'miller)
; 500
; > (get-salary 'seattle 'smith)
; 1000
; > (get-salary 'seattle 'williams)
; #f
; > (get-salary 'new-york 'williams)
; 100
; > (get-salary 'new-york 'anderson)
; 400

; C)

(define (find-employee-record emp registers)
  (if (null? registers)
      #f
      (let ((register (car registers)))
        (let ((emp-in-register (get-record register emp)))
          (if (eq? emp-in-register #f)
              (find-employee-record emp (cdr registers))
              emp-in-register)))))

; > (find-employee-record 'smith (list 'new-york 'seattle))
; (mcons 'smith (mcons 1000 '()))
; > (find-employee-record 'miller (list 'new-york 'seattle))
; (mcons 'miller (mcons 500 '()))
; > (find-employee-record 'williams (list 'new-york 'seattle))
; (mcons 'williams (mcons 100 '()))
; > (find-employee-record 'joshua (list 'new-york 'seattle))
; #f

; D)

; Just install new register as before