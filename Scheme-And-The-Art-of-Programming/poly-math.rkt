#lang scheme
;Exercise 5.9
(define zero-poly?
  (lambda (poly)
    (and (zero? (degree poly)) (zero? (leading-coef poly)))))
(define make-term
  (lambda (deg coef)
    (poly-cons deg coef the-zero-poly)))
(define leading-term
   (lambda (poly)
     (make-term (degree poly) (leading-coef poly))))
(define p+
  (lambda (poly1 poly2)
    (cond
      ((zero-poly? poly1) poly2)
      ((zero-poly? poly2) poly1)
      (else
       (let ((n1 (degree poly1))
             (n2 (degree poly2))
             (a1 (leading-coef poly1))
             (a2 (leading-coef poly2))
             (rest1 (rest-of-poly poly1))
             (rest2 (rest-of-poly poly2)))
         (cond
           ((> n1 n2) (poly-cons n1 a1 (p+ rest1 poly2)))
           ((< n1 n2) (poly-cons n2 a2 (p+ poly1 rest2)))
           (else
            (poly-cons n1 (+ a1 a2) (p+ rest1 rest2)))))))))


(define p*
  (letrec ((t* (lambda (trm poly)
                 (if (zero-poly? poly)
                     the-zero-poly
                     (poly-cons (+ (degree trm) (degree poly))
                                (* (leading-coef trm) (leading-coef poly))
                                (t* trm (rest-of-poly poly)))))))
    (lambda (poly1 poly2)
      (letrec
          ((p*-helper (lambda (p1)
                        (zero-poly? p1)
                        the-zero-poly
                        (p+ (t* (leading-term p1) poly2)
                            (p*-helper (rest-of-poly p1))))))
        (p*-helper poly1)))))

(define negative-poly
  (lambda (poly)
    (let ((poly-negative-one (make-term 0 -1)))
      (p* poly-negative-one poly))))

(define p-
  (lambda (poly1 poly2)
    (p+ (negative-poly poly1) poly2)))

(define poly-value
  (lambda (poly num)
    (letrec
        ((pvalue (lambda (p)
                   (let ((n (degree p)))
                     (if (zero? n)
                         (leading-coef p)
                         (let ((rest (rest-of-poly p)))
                           (if (< (degree rest) (- n 1))
                               (pvalue (poly-cons (- n 1)
                                                  (* num (leading-coef p))
                                                  rest))
                               (pvalue (poly-cons (- n 1)
                                                  (+ (* num (leading-coef p))
                                                     (leading-coef rest))
                                                  (rest-of-poly rest))))))))))
      (pvalue poly))))

;;FIRST REPRESENTATION IMPLEMENTATION

(define the-zero-poly '(0))

(define degree
  (lambda (poly)
    (- (length poly) 1)))
;(define length
;  (lambda (ls)
;    (letrec ((len-helper (lambda (ls acc)
;                           (if (null? ls)
;                               '()
;                               (len-helper (cdr ls) (+1 acc))))))
;      (len-helper ls 0))))
(define leading-coef
  (lambda (poly)
    (car poly)))

(define rest-of-poly
  (lambda (poly)
    (cond
      ((= 0 (degree poly)) the-zero-poly)
      ((= 0 (leading-coef (cdr poly)))
       (rest-of-poly (cdr poly)))
      (else (cdr poly)))))

(define poly-cons
  (lambda (deg coef poly)
    (let ((deg-p (degree poly)))
      (cond
        ((and (= 0 deg) (equal? poly the-zero-poly)) (list coef))
        ((= 0 coef) poly)
        (else
         (cons coef
               (append (list-of-zeros (- (- deg deg-p) 1))
                       poly)))))))
(define list-of-zeros
  (lambda (n)
    (letrec
        ((lst-zeros (lambda (n acc)
                      (if (= 0 n)
                      acc
                      (lst-zeros (- n 1) (cons 0 acc))))))
      (lst-zeros n '()))))
;; 
 
;; ;(define append
;; ;  (lambda (ls1 ls2)
;; ;    (if (null? ls1) ls2
;; ;         (cons (car ls1) (append (cdr ls1) ls2)))))

;; (define p1 '(5 -7 0 2 -4))
;; (define p2 '(1 6 -3 0))
;; 
;; (p+ p1 p2)

;; SECOND REPRESENTATION IMPLEMENTATION
;(define the-zero-poly '((0 0)))

;; (define the-zero-poly (cons (cons 0 0) '()))
;; (define degree
;;   (lambda (poly)
;;     (caar poly)))
;; (define leading-coef
;;   (lambda (poly)
;;     (cadar poly)))

;; (define leading-coef
;;   (lambda (poly)
;;     (cdar poly)))
;; (define rest-of-poly
;;   (lambda (poly)
;;     (if (null? (cdr poly))
;;         the-zero-poly
;;         (cdr poly))))
;; (define poly-cons
;;   (lambda (deg coef poly)
;;     (let ((deg-p (degree poly)))
;;       (cond
;;         ((and (zero? deg) (equal? poly the-zero-poly))
;;          (list (list 0 coef)))
;;         ;((>= deg-p deg)
;;          ;(error "poly-cons: Degree too high in" poly))
;;         ((zero? coef) poly)
;;         (else
;;          (cons (cons deg coef) poly))))))

;; 
;; (define p1-v2 '((4 5) (3 -7) (1 2) (0 -4)))
;; (define p2-v2 '((3 1) (2 6) (1 -3)))
;; (define p3-v2 '((1 -3) (2 6) (3 1)))
;; (define p4-v2 '((0 -4) (1 2) (3 -7) (4 5)))
;; (p+ p1-v2 p2-v2)
;; ;(p+-improved p1-v2 p2-v2)
;; (poly-value p3-v2 0)
;; (poly-value p1-v2 -1)
;; (poly-value p1-v2 2)
;; (poly-value p2-v2 0)
;; (poly-value p2-v2 -2)

;;Exercise 5.10
;; (define p+-improved
;;   (lambda (poly1 poly2)
;;     (cond
;;       ((zero-poly? poly1) poly2)
;;       ((zero-poly? poly2) poly1)
;;       (else (let ((n1 (degree poly1))
;;                   (n2 (degree poly2)))
;;               (cond
;;                 ((> n1 n2)
;;                  (let
;;                      ((a1 (leading-coef poly1))
;;                       (rest1 (rest-of-poly poly1)))
;;                    (poly-cons n1 a1 (p+ rest1 poly2))))
;;                 ((< n1 n2)
;;                  (let
;;                      ((a2 (leading-coef poly2))
;;                       (rest2 (rest-of-poly poly2)))
;;                    (poly-cons n1 a2 (p+ poly1 rest2))))
;;                 (else
;;                  (let
;;                      ((a1 (leading-coef poly1))
;;                       (a2 (leading-coef poly2))
;;                       (rest1 (rest-of-poly poly1))
;;                       (rest2 (rest-of-poly poly2)))
;;                    (poly-cons n1 (+ a1 a2) (p+ rest1 rest2))))))))))

;; (p+-improved p1-v2 p2-v2)
;; (p+-improved p3-v2 p4-v2)


;; Exercise 5.12
;; Representing polynomials in increasing degree does not work unless we implement
;; the operations afresh
;; It is not a natural way to represent polynomials--they are normally represented in
;; increasing degree

;;Exercise 5.13
;;We will have to change the leading-coef procedure to use cdar instead of cadar
;;The zero poly will also have to be changed to a pair of two zeros
;;We will also have to change the inputs from lists to pairs
;; (p+ '((4 . 5) (3 . -7) (1 . 2) (0 . -4)) '((3 . 1) (2 . 6) (1 . -3)))


;;Exercise 5.14
;;We use a local binding for the degree and leading coef of term and inside
;;the let body we use a helper procedure to allow recursion and then apply the
;; helper procedure to the original poly 
;; (define p*-new
;;   (let
;;       ((t* (lambda (trm poly)
;;              (let ((deg (degree trm))
;;                    (lc (leading-coef trm)))
;;                (letrec
;;                    ((t*-helper
;;                      (lambda (poly)
;;                        (if (zero-poly? poly)
;;                            the-zero-poly
;;                            (poly-cons
;;                             (+ deg (degree poly))
;;                             (* lc (leading-coef poly))
;;                             (t*-helper (rest-of-poly poly)))))))
;;                  (t*-helper poly))))))
;;     (lambda (poly1 poly2)
;;       (if (zero-poly? poly1)
;;           the-zero-poly
;;           (let ((lead-term (leading-term poly1))
;;                 (rest1 (rest-of-poly poly1)))
;;             (p+ (t* lead-term poly2) (p*-new rest1 poly2)))))))
;; 
;; (p*-new '((2 . 2)) '((1 . 2)))

;;BINARY NUMBERS
;;PROGRAM 5.16
(define digits->poly
  (lambda (digit-list)
    (if (null? digit-list)
        (error "digits->poly: Not defined for empty list")
        (letrec
            ((make-poly
              (lambda (deg ls)
                (if (null? ls)
                    the-zero-poly
                    (poly-cons deg (car ls)
                               (make-poly (sub1 deg) (cdr ls)))))))
        (make-poly (sub1 (length digit-list)) digit-list)))))


(define binary->decimal
  (lambda (digit-list)
    (poly-value (digits->poly digit-list) 2)))



(define octal->decimal
  (lambda (digit-list)
    (poly-value (digits->poly digit-list) 8)))

(binary->decimal '(1 1 0 0 1 0 1))
(binary->decimal '(1 1 0 0 1 1 0 1))
(octal->decimal '(1 4 4))



(define poly->digits
  (lambda (poly)
    (letrec
        ((convert
          (lambda (p deg)
            (cond
            ((zero? deg) (list (leading-coef p)))
            ((= (degree p) deg)
             (cons (leading-coef p)
                   (convert (rest-of-poly p) (sub1 deg))))
            (else (cons 0 (convert p (sub1 deg))))))))
      (convert poly (degree poly)))))

(define decimal->binary
  (lambda (num)
    (letrec
        ((dec->bin
          (lambda (n deg)
            (if (zero? n)
                the-zero-poly
                (p+ (make-term deg (remainder n 2))
                    (dec->bin (quotient n 2) (add1 deg)))))))
      (poly->digits (dec->bin num 0)))))

(define decimal->base
  (lambda (num base)
    (letrec
        ((dec->base
          (lambda (n deg)
            (if (zero? n)
                the-zero-poly
                (p+ (make-term deg (remainder n base))
                    (dec->base (quotient n base) (add1 deg)))))))
      (poly->digits (dec->base num 0)))))


(decimal->binary 45)
(binary->decimal (decimal->binary 45))

(define decimal->base-4
  (lambda (num)
    (decimal->base num 4)))

(define decimal->hex
  (let ((num->letter (lambda (num)
                          (cond
                            ((= num 10) 'A)
                            ((= num 11) 'B)
                            ((= num 12) 'C)
                            ((= num 13) 'D)
                            ((= num 14) 'E)
                            ((= num 15) 'F)
                            (else num)))))
    (letrec ((print-hex (lambda (ls)
                          (if (null? ls)
                              '()
                              (cons (num->letter (car ls)) (print-hex (cdr ls)))))))
      (lambda (num)
        (print-hex (decimal->base num 16))))))


(decimal->hex 3162)
(decimal->base-4 2000)