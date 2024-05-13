;; Definition of harmonic sum using a letrec binding

(define harmonic-sum-it
    (lambda (n)
      (letrec ((harmonic-sum-it-helper
                 (lambda (n acc)
                   (cond
                     ((zero? n) acc)
                     (else
                       (harmonic-sum-it-helper (sub1 n) (+ acc (/ 1 n))))))))
        (harmonic-sum-it-helper n 0))))
;; Recursive and iterative definitions of integer list making procedures
;; from n to 1
;; Ascending list of integers:
;; Recursive definition:
(define mk-asc-list-of-ints
    (lambda (n)
      (if (zero? n)
          '()
          (append (mk-asc-list-of-ints (sub1 n)) (list n)))))

;;Iterative definition
(define mk-asc-list-of-ints-it
    (lambda (n)
      (letrec ((mk-asc-helper (lambda (n acc)
                                (if (zero? n)
                                    acc
                                    (mk-asc-helper (sub1 n) (cons n acc))))))
        (mk-asc-helper n '()))))

;;Descending list of integers
;;Recursive definition
(define mk-desc-list-of-ints
    (lambda (n)
      (if (zero? n)
          '()
          (append (list n) (mk-desc-list-of-ints (sub1 n))))))

;; Iterative definition
(define mk-desc-list-of-ints-it
    (lambda (n)
      (letrec ((mk-desc-helper (lambda (n acc)
                                 (if (zero? n)
                                     acc
                                     (mk-desc-helper (sub1 n) (cons acc (list n)))))))
        (mk-desc-helper n '()))))

