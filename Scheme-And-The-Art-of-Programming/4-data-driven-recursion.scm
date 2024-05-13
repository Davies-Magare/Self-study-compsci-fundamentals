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


;; occurs - count the top level occurences of item in a list
;; @item: The item whose number of occurences to count
;; @ls: The list to count occurences of item
;; helper-procedures: none
;; Return: The integer occurences of item in ls
(define occurs
    (lambda (item ls)
      (cond
        ((null? ls) 0)
        (else
          (if (equal? (car ls) item)
              (add1 (occurs item (cdr ls)))
              (occurs item (cdr ls)))))))

;; occurs-it - Count occurences of an item in a list iteratively
;; @item: The item to count
;; @list: The list in which to count item
;;     helper-procedures: occurs-it-helper
;;     @occurs-it-helper - Helper procedure to occurs-it
;;     @ls: A list to count the number of occurences of item
;;     @acc: An accumulator to store the results in each iteration
;;     Return: A procedure that counts the number of occurences of item in ls
;; Return: Number of occurences of item in ls

(define occurs-it
    (lambda (item ls)
      (letrec ((occurs-it-helper (lambda (ls acc)
                                   (cond
                                     ((null? ls) acc)
                                     (else
                                       (if (equal? (car ls) item)
                                           (occurs-it-helper (cdr ls) (add1 acc))
                                           (occurs-it-helper (cdr ls) acc)))))))
        (occurs-it-helper ls 0))))


