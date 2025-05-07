#lang scheme
(define both
  (lambda (pred)
    (lambda (arg1 arg2)
      (and (pred arg1) (pred arg2)))))

;; ((both (lambda (ls) (not (null? ls)))) '(a b) '())

;; (define neither
;;   (lambda (pred)
;;     (lambda (arg1 arg2)
;;       (not (or (pred arg1) (pred arg2))))))

;; ((neither (lambda (n) (zero? n))) 4 5)

;; (define at-least-one
;;   (lambda (pred)
;;     (lambda (arg1 arg2)
;;       (or (pred arg1) (pred arg2)))))

;; ((at-least-one (lambda (n) (zero? n))) 4 5)
;; (define one-zero? (at-least-one (lambda (n) (zero? n))))
;; (one-zero? 4 5)
;; (one-zero? 2 0)

;; (define both
;;   (lambda (pred)
;;     (lambda (arg1 arg2)
;;       ((neither (lambda (arg) (not (pred arg)))) arg1 arg2))))

(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

;; (define both
;;   (lambda (pred)
;;     (lambda (arg1 arg2)
;;       ((neither (compose not pred)) arg1 arg2))))


;; (define both
;;   (lambda (pred)
;;     (neither (compose not pred))))

((both zero?) 4 5)
((both even?) 2 4)


;; (define at-least-one
;;   (lambda (pred)
;;     (lambda (arg1 arg2)
;;       (or (not(both pred)) (not(neither pred))))))

;; (define neither
;;   (lambda (pred)
;;     (lambda (arg1 arg2)
;;       ((both (lambda (arg) (not (pred arg)))) arg1 arg2))))

(define neither
  (lambda (pred)
    (both (compose not pred))))

(define at-least-one
  (lambda (pred)
    (lambda (arg1 arg2)
      (not ((both (lambda (arg) (not (pred arg)))) arg1 arg2)))))

;; (define at-least-one
;;   (lambda (pred)
;;     (not (both (compose not pred)))))


(define at-least-one-curr
  (lambda (pred)
    (lambda (arg1)
      (lambda (arg2)
        (not ((both (lambda (arg) (not (pred arg)))) arg1 arg2))))))


