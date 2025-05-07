#lang scheme
;; (define filter-in-all-c
;;   (lambda (pred)
;;     (letrec
;;         ((helper
;;           (lambda (ls)
;;             (if (null? ls)
;;                 '()
;;                 (let ((a (car ls)))
;;                   (if (or (pair? a) (null? a))
;;                       (cons (helper a) (helper (cdr ls)))
;;                       (if (pred a)
;;                           (cons a (helper (cdr ls)))
;;                           (helper (cdr ls)))))))))
;;       helper)))

;; (define filter-odd-all (filter-in-all-c odd?))

;; (define sum-all
;;   (letrec ((helper
;;             (lambda (ls)
;;               (if (null? ls)
;;                   0
;;                   (let ((a (car ls)))
;;                     (if (or (pair? a) (null? a))
;;                         (+ (helper a) (helper (cdr ls)))
;;                         (+ a (helper (cdr ls)))))))))
;;     helper))



(define deep-recur
  (lambda (seed item-proc list-proc)
    (letrec ((helper
              (lambda (ls)
                (if (null? ls)
                    seed
                    (let ((a (car ls)))
                      (if (or (pair? a) (null? a))
                          (list-proc (helper a) (helper (cdr ls)))
                          (item-proc a (helper (cdr ls)))))))))
      helper)))

;; Redefine filter-in-all-c and sum-all using procedural abstraction

(define filter-in-all-c
  (lambda (pred)
    (deep-recur '() (lambda (x y)
                      (if (pred x)
                          (cons x y)
                          y))
                cons)))


(define sum-all (deep-recur 0 + +))
(define filter-odd-all (filter-in-all-c odd?))

;Exercise 7.26
;; (define remove-all-c
;;   (lambda (item)
;;     (letrec
;;         ((helper
;;           (lambda (ls)
;;             (if (null? ls)
;;                 '()
;;                 (let ((a (car ls)))
;;                   (if (or (pair? a) (null? a))
;;                       (cons (helper a) (helper (cdr ls)))
;;                       (if (not (equal? a item))
;;                           (cons a (helper (cdr ls)))
;;                           (helper (cdr ls)))))))))
;;       helper)))
;; 
;; 
;; (define product-all
;;   (letrec ((helper
;;             (lambda (ls)
;;               (if (null? ls)
;;                   1
;;                   (let ((a (car ls)))
;;                     (if (or (pair? a) (null? a))
;;                         (* (helper a) (helper (cdr ls)))
;;                         (* a (helper (cdr ls)))))))))
;;     helper))

;Exercise 7.27

(define remove-all-c
  (lambda (item)
    (deep-recur '() (lambda (x y)
                      (if (not (equal? x item))
                          (cons x y)
                          y))
                cons)))
(define product-all
  (deep-recur 1 * *))

;;WORKS LIKE MAGIC!!
(define subst-all-m
  (lambda (new)
    (lambda (old)
      (deep-recur '() (lambda (x y)
                        (if (equal? x old)
                            (cons new y)
                            (cons x y)))
                  cons))))


;Exercise 7.28

(define filter-out-all-c
  (lambda (pred)
    (deep-recur '() (lambda (x y) (if (pred x) y
                                      (cons x y)))
                cons)))
(define filter-out-all
  (lambda (pred ls)
    ((filter-out-all-c pred) ls)))

;Exercise 7.30
(define reverse-append
  (lambda (x y)
    (append y (list x))))
;;idk brilliant? dumb? idk let's go with the latter: for a junior is always dumb
(define reverse-all
  (deep-recur '()
              reverse-append
              reverse-append))


;;NOT YET
;;The problem is that the operands must be evaluated to the args and idk (helper (car ls)) does not help
(define flat-recur
  (lambda (seed list-proc)
    (deep-recur seed (lambda (x y) #f) (lambda (x y) (list-proc x y)))))


 (define member?
   (lambda (item)
     (flat-recur #f (lambda (x y) (or (equal? x item) y)))))



