#lang scheme
(define add1-to-item
  (lambda (ls)
    (cond
      ((null? ls) '())
      (else (cons (add1 (car ls)) (add1-to-item (cdr ls)))))))

(map add1 '(2 3 4))

(define my-map
  (lambda (proc ls)
    (cond
      ((null? ls) '())
      (else (cons (proc (car ls)) (my-map proc (cdr ls)))))))

(my-map add1 '(1 2 3 4))

(my-map (lambda (num) (* num 2)) '(2 4 8))

(let ((proc (lambda (ls) (cons 'a ls))))
  (my-map proc '((b c) (d e) (f g))))


(define member?
  (lambda (a ls)
    (if (null? ls) #f
        (or (equal? a (car ls)) (member? a (cdr ls))))))


(let ((x 'a))
  (let ((proc (lambda (ls) (member? x ls))))
    (my-map proc '((a b c) (b c d) (c d a)))))


(define add
  (letrec ((list-add
            (lambda (ls)
              (if (null? ls)
                  0
                  (+ (car ls) (list-add (cdr ls)))))))
    (lambda args
      (list-add args))))

(define my-list (lambda args args))
(my-list 'a 'b 'c 'd 'e 'f 'g)
(apply add '(1 2 3 4 5))

(define add2
  (lambda args
    (if (null? args) 0
        (+ (car args) (apply add2 (cdr args))))))


(define compose
  (lambda (f g x)
    (f (g x))))

(define compose2
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(define h (compose2 sqrt add1))
(h 8)
(h 15)

(compose sqrt add1 8)




















