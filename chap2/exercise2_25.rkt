#lang sicp

(define sample1 (list 1 3 (list 5 7) 9))

(= 7 (car (cdr (car (cdr (cdr sample1))))))

(define sample2 (list (list 7)))

(= 7 (car (car sample2)))

(define sample3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(= 7 (car
      (cdr
       (car
        (cdr
         (car
          (cdr
           (car
            (cdr
             (car
              (cdr
               (car
                (cdr sample3)))))))))))))