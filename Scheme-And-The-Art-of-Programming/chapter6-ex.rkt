#lang scheme
(define tolerance 0.000005)

(define close-enough?
  (lambda (u v)
    (< (abs (- u v)) tolerance)))


(define square-root
  (lambda (a)
    (letrec
         ((next-estimate
           (lambda (u)
             (let ((v (/ (+ u (/ a u)) 2)))
               (if (close-enough? u v)
                   v
                   (next-estimate v))))))
       (next-estimate 1))))

