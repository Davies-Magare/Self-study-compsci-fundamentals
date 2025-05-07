#lang scheme
(define add-letter
  (lambda (letter ls)
    (if (null? ls) ls
        (let ((first (car ls)))
          (cons (if (pair? first) (cons letter first) first)
                (add-letter letter (cdr ls)))))))

(let ((proc (lambda (item)
              (if (pair? item) (cons 'v item) item))))
  (map proc '((a b) c (c d) v (v v))))

;(for-each display '("Vamos " "adelante " "cabrones")

(define add
  (letrec ((list-add
            (lambda (ls)
              (if (null? ls) 0
                  (+ (car ls) (list-add (cdr ls)))))))
    (lambda args
      (list-add args))))

(define writeln
  (lambda args
    (for-each display args)
    (newline)))


(define add2
  (lambda args
    (if (null? args) 0
        (+ (car args) (apply add2 (cdr args))))))

(define plus
  (lambda (x y)
    (if (zero? y) x
        (add1 (plus x (sub1 y))))))
(define times
  (lambda (x y)
    (if (zero? y) 0
        (plus x (times x (sub1 y))))))

(define exponent
  (lambda (x y)
    (if (= y 0) 1
        (times x (exponent x (sub1 y))))))

(define super
  (lambda (x y)
    (if (= y 0) 1
        (exponent x (super x (sub1 y))))))


(define superduper
  (lambda (x y)
    (if (= y 0) 1
        (super x (superduper x (sub1 y))))))

(define super-order
  (lambda (n)
    (cond
      ((= n 1) plus)
      ((= n 2) times)
      (else
       (lambda (x y)
         (if (zero? y) 1
             ((super-order (sub1 n)) x ((super-order n) x (sub1 y)))))))))


;; exercise 7.2

(define compose3
  (lambda (f g h)
    (lambda (x)
      (f (g (h x))))))

;; exercise 7.3

(define compose-many
  (lambda args
    (lambda (x)
      (letrec ((proc (lambda (x ls)
                       (if (null? ls) x
                           (proc ((car ls) x) (cdr ls))))))
        (proc x args)))))


(define subtract
  (lambda (x y)
    (cond
      ((zero? y) x)
      (else (plus -1 (subtract x (sub1 y)))))))



(define iota
  (lambda (n)
    (letrec ((iota-helper (lambda (k acc)
                            (if (zero? k) (cons 0 acc)
                                (iota-helper (sub1 k) (cons k acc))))))
      (iota-helper (sub1 n) '()))))



(define factorial
  (lambda (n)
    (if (zero? n) 1
        (* n (factorial (sub1 n))))))

(map factorial (iota 6))

(map (lambda (x) (+ x (add1 x))) (iota 5))


(define mystery
  (lambda (len base)
    (letrec
        ((mystery-help
          (lambda (n s)
            (if (zero? n)
                (list s)
                (let ((h (lambda (x)
                           (mystery-help (sub1 n) (cons x s)))))
                  (apply append (map h (iota base))))))))
      (mystery-help len '()))))

;;Exercise 7.6
(define map-first-two
  (lambda (proc ls)
    (cond
      ((< (length ls) 2) '())
      (else
       (cons (proc (car ls) (cadr ls)) (map-first-two proc (cdr ls)))))))


(define error
  (lambda args
    (display "Error:")
    (for-each (lambda (value) (display " ") (display value)) args)
    (newline)))
;Exercise 7.7

(define reduce
  (lambda (proc ls)
    (cond
      ((< (length ls) 2) (error "You need more than two arguments"))
      (else
       (let ((first (car ls)) (second (cadr ls)) (rest (cddr ls)))
         (cond
           ((= (length ls) 2) (proc first second))
           (else
            (reduce proc (cons (proc first second) rest)))))))))



;Exercise 7.8
(define andmap
  (lambda (pred ls)
    (cond
      ((null? ls) #t)
      (else
       (and (pred (car ls)) (andmap pred (cdr ls)))))))



(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(define map2
  (lambda (proc ls1 ls2)
    (cond
      ((null? ls1) '())
      (else
       (cons (proc (car ls1) (car ls2)) (map2 proc (cdr ls1) (cdr ls2)))))))


;; (define map
;;   (lambda args
;;     (let ((proc (car args)))
;;       ((letrec ((map-helper
;;                  (lambda (a*)
;;                    (if (any-null? a*)
;;                        '()
;;                        (cons
;;                         (apply proc (map car a*


(define member?-c
  (lambda (item)
    (letrec
        ((helper
          (lambda (ls)
            (if (null? ls)
                #f
                (or (equal? (car ls) item) (helper (cdr ls)))))))
      helper)))

(define member-a (member?-c 'a))

(define apply-to-all
  (lambda (proc)
    (letrec
        ((helper
          (lambda (ls)
            (if (null? ls) '()
                (cons (proc (car ls)) (helper (cdr ls)))))))
      helper)))
(define map-add1 (apply-to-all add1))

(define sum
  (letrec
      ((helper
        (lambda (ls)
          (if (null? ls) 0
              (+ (car ls) (helper (cdr ls)))))))
    helper))

(define product
  (letrec
      ((helper
        (lambda (ls)
          (if (null? ls)
              1
              (* (car ls) (helper (cdr ls)))))))
    helper))

