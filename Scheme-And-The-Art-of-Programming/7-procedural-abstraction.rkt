#lang scheme
;; (define member?-c
;;   (lambda (item)
;;     (letrec
;;         ((helper
;;           (lambda (ls)
;;             (if (null? ls)
;;                 #f
;;                 (or (equal? (car ls) item) (helper (cdr ls)))))))
;;       helper)))
;; 
;; (define member-a (member?-c 'a))
;; 
;; (define apply-to-all
;;   (lambda (proc)
;;     (letrec
;;         ((helper
;;           (lambda (ls)
;;             (if (null? ls) '()
;;                 (cons (proc (car ls)) (helper (cdr ls)))))))
;;       helper)))
;; (define map-add1 (apply-to-all add1))
;; 
;; (define sum
;;   (letrec
;;       ((helper
;;         (lambda (ls)
;;           (if (null? ls) 0
;;               (+ (car ls) (helper (cdr ls)))))))
;;     helper))
;; 
;; (define product
;;   (letrec
;;       ((helper
;;         (lambda (ls)
;;           (if (null? ls)
;;               1
;;               (* (car ls) (helper (cdr ls)))))))
;;     helper))



(define flat-recur
  (lambda (seed list-proc)
    (letrec
        ((helper
          (lambda (ls)
            (if (null? ls)
                seed
                (list-proc (car ls) (helper (cdr ls)))))))
      helper)))

(define member-c?
  (lambda (item)
    (flat-recur #f (lambda (x y) (or (equal? x item) y)))))

(define apply-to-all
  (lambda (proc)
    (flat-recur '() (lambda (x y) (cons (proc x) y)))))


(define sum (flat-recur 0 +))


(define product (flat-recur 1 *))


(define filter-c
  (lambda (proc)
    (flat-recur '() (lambda (x y)
                      (if (proc x)
                          (cons x y)
                          y)))))


(define mult-by-scalar
  (lambda (c)
    (flat-recur '() (lambda (x y) (cons (* c x) y)))))

((mult-by-scalar 3) '(1 -2 3 -4))
((mult-by-scalar 5) '())


(define filter-out-c
  (lambda (pred)
    (flat-recur '() (lambda (x y) (if (pred x)
                                      y
                                      (cons x y))))))
(define filter-out
  (lambda (pred ls)
    ((filter-out-c pred) ls)))

(filter-out even? '(1 2 3 4 5))
(filter-out odd? '(1 2 3 4 5))


(define insert-left
  (lambda (new old)
    (flat-recur '() (lambda (x y)
                      (if (equal? x old)
                          (cons new y)
                          (cons x y))))))

(define insert-left-m
  (lambda (new old)
    (insert-left new old)))

((insert-left-m 'a' b) '(b b b b b))

;;Exercise 7.25
;; 

;; (define partial-sum
;;   (lambda (proc k n)
;;     (letrec ((helper (lambda (acc k)
;;                        (if (> k n)
;;                            acc
;;                            (helper (+ acc (proc k)) (add1 k))))))
;;       (helper 0 k))))
;; REWRITE WITHOUT THE ACCUMULATOR; AND PROPER USE OF RECURSION


;; 
;; (partial-sum (lambda (m) (* m m)) 3 7)

;; (define partial-product
;;   (lambda (proc k n)
;;     (letrec ((helper (lambda (acc k)
;;                        (if (> k n)
;;                            acc
;;                            (helper (* acc (proc k)) (add1 k))))))
;;       (helper 1 k))))

;; (partial-product (lambda (m) (* m m)) 3 7)

;; (define partial-product
;;   (lambda (acc proc)
;;     (letrec ((helper (lambda (k n)
;;                        (if (> k n)
;;                            acc
;;                            (let ((acc (* acc k (helper (add1 k) n))))
;;                              acc)))))
;;       helper)))



;;PROGRESS!!!!!
;;THIS WALKED SO THAT THE ANSWER COULD RUN!!!
                           
;; (define partial
;;   (lambda (acc proc)
;;     (letrec ((helper (lambda (k n)
;;                        (if (> k n)
;;                            acc
;;                            (let ((acc (proc acc k (helper (add1 k) n))))
;;                              acc)))))
;;       helper)))
;; 
;;                                                 
;;                                          
;; (define partial-sum (partial 0 +))
;; (define partial-product (partial 1 *))

(define partial-sum-walk
  (lambda (proc k n)
    (letrec ((helper (lambda (k acc)
                       (if (> k n)
                           acc
                           (+ acc (proc k) (helper (add1 k) acc))))))
      (helper k 0))))

(partial-sum-walk (lambda (m) (* m m)) 3 7)

(define partial
  (lambda (acc sum-multi)
    (letrec ((helper (lambda (proc k n)
                       (if (> k n)
                           acc
                           (let ((acc (sum-multi acc (proc k) (helper proc (add1 k) n))))
                             acc)))))
      helper)))


;; (define partial-sum (partial 0 +))
;; (define partial-product (partial 1 *))
;; FINAL EFFICIENT SOLUTION THAT RECURS PROPERLY
(define partial-sum-eff
  (lambda (proc k n)
    (letrec ((helper (lambda (k)
                       (if (> k n)
                           0
                           (+ (proc k) (helper (add1 k)))))))
      (helper k))))

(define partial-product-eff
  (lambda (proc k n)
    (letrec ((helper (lambda (k)
                       (if (> k n)
                           1
                           (* (proc k) (helper (add1 k)))))))
      (helper k))))

(define partial-eff
  (lambda (acc sum-multi)
    (letrec
    ((helper (lambda (proc k n)
               (if (> k n)
                   acc
                   (sum-multi (proc k) (helper proc (add1 k) n))))))
    helper)))
(define partial-sum (partial-eff 0 +))
(define partial-product (partial-eff 1 *))
(partial-sum (lambda (m) (* m m)) 3 7)
(partial-product (lambda (m) (* m m)) 3 7)
