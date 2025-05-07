#lang scheme
(require racket/pretty)
(substring (string-append "we " "love " "programming")
           5
           19)

(string-ci=? "davies" "DAVIES")
(string=? "davies" "Davies")

;;Exercise 6.1 
(define substring?
  (lambda (sstr strng)
    (let ((len-sub (string-length sstr))
          (len-str (string-length strng)))
      (letrec ((find-subst (lambda (start)
                             (cond
                              ((< (string-length
                                  (substring strng start len-str))
                                  len-sub) #f)
                              ((string=? (substring strng start (+ start len-sub))
                                         sstr) #t)
                              (else (find-subst (add1 start)))))))
        (find-subst 0)))))

(define substring-ref
  (lambda (strng n)
    (substring strng n (add1 n))))

;;Exercise 6.2
(define string-reverse
  (lambda (strng)
    (let ((len-str (string-length strng)))
      (letrec
          ((reverse (lambda (start str)
                      (if (string=? str "")
                          ""
                          (string-append
                           (reverse (add1 start) (substring strng (add1 start) len-str))
                           (substring-ref strng start))))))
        (reverse 0 strng)))))


;;Exercise 6.3
(define palindrome?
  (lambda (strng)
    (let ((reversed-str (string-reverse strng)))
      (string=? strng reversed-str))))



;;Note 6.1 and 6.2 require more refinement to adjust variable names
;; and to make better use of local procedures
(define writeln-n
  (lambda args
    (for-each display args)
    (newline)))

(define mystery
  (lambda (pos-int)
    (letrec ((helper
              (lambda (n count)
                (cond
                  ((= n 1)
                   (newline)
                   (writeln-n "It took " count " steps to get to 1."))
                  ((even? n)
                   (writeln-n count
                            ". We divide " n " by 2.")
                   (helper (/ n 2) (add1 count)))
                  (else
                   (writeln-n count
                            ". We multiply " n " by 3 and add 1.")
                   (helper (+ (* n 3) 1) (add1 count)))))))
      (helper pos-int 0))))


;;Squareroot by Newton Method
(define tolerance 0.000005)
(define close-enough?
  (lambda (u v)
    (< (abs (- u v)) tolerance)))

;; (define square-root
;;   (lambda (a)
;;     (letrec
;;         ((next-estimate
;;           (lambda (u)
;;             (let ((v (/ (+ u (/ a u)) 2)))
;;               (if (close-enough? u v)
;;                   v
;;                   (next-estimate v))))))
;;       (next-estimate 1))))

(begin
  (display "Is")
  (display " ")
  (display 1.4142)
  (display " the square root of 2?"))


(define square-root-display
  (lambda (a)
    (letrec ((next-estimate (lambda (u)
                              (let ((v (/ (+ u (/ a u)) 2)))
                                (if (close-enough? u v)
                                    v
                                    (begin
                                      (display v)
                                      (newline)
                                      (next-estimate v)))))))
      (next-estimate 1))))

(define round-5-places
  (lambda (n dec-num)
    (let ((scale-factor (expt 10 n)))
      (/ (round (* dec-num scale-factor)) scale-factor))))
(round-5-places 5 (square-root-display 13))


(define read-demo
  (lambda ()
    (display "Enter data (enter done when finished): ")
    (let ((response (read)))
      (cond
        ((eq? response 'done) (display "Gracias. Adios."))
        (else (display "You entered: ")
              (write response)
              (newline)
              (read-demo))))))

;;Moved square root procedure definition here to
;; improve on the display. Idk how to display in decimal
;; in racket ide 
(define square-root-pre
  (lambda (a)
    (letrec
        ((next-estimate
          (lambda (u)
            (let ((v (/ (+ u (/ a u)) 2)))
              (if (close-enough? u v)
                  v
                  (next-estimate v))))))
      (next-estimate 1))))

(define square-root
  (lambda (n)
    (round-5-places 3 (square-root-pre n))))

(define interactive-square-root
  (lambda ()
    (writeln-n "Enter the number whose square root you want, "
             " or enter done to quit:")
    (let ((n (read)))
      (if (eq? n 'done)
          (writeln "Adios, Tio!")
          (begin
            (pretty-print-exact-as-decimal #t)
            (writeln-n "The square root of " n " is " (square-root n))
            (newline)
            (interactive-square-root))))))

(define tower-of-hanoi
  (lambda (n)
    (letrec
        ((move
          (lambda (n source destination helper)
            (if (= n 1)
                (list (list source destination))
                (append
                 (move (sub1 n) source helper destination)
                 (cons
                  (list source destination)
                  (move (sub1 n) helper destination source)))))))
      (move n 'L 'R 'C))))


(define tower-of-hanoi-v2
  (lambda (n)
    (letrec
        ((move
          (lambda (n source destination helper)
            (if (= n 1)
                (list (list source destination))
                (cons
                 (append (move (sub1 n) source helper destination)
                         (list source destination))
                 (move (sub1 n) helper destination source))))))
      (move n 'L 'R 'C))))

