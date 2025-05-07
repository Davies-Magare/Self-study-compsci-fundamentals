#lang scheme
;; Ya'll walked so that display and calculate could run!
;;   (define change
;;   (let ((hundred 0) (twenty 0) (ten 0) (five 0) (one 0))
;;     (lambda (amount)
;;       (cond
;;         ((zero? amount) (list hundred twenty ten five one))
;;         ((>= amount 100) (add1 hundred) (change (- amount 100)))
;;         ((>= amount 20) (add1 twenty) (change (- amount 20)))
;;         ((>= amount 10) (add1 ten) (change (- amount 10)))
;;         ((>= amount 5) (add1 five) (change (- amount 5)))
;;         (else (add1 one) (change (sub1 amount)))))))
;; 
;; 
;; (define hundred 0)
;; (define twenty 0)
;; (define ten 0)
;; (define five 0)
;; (define one 0)
;; 
;; (define change-bet
;;   (lambda (init-amount)
;;     (letrec ((change
;;               (lambda (amount)
;;                 (cond
;;                   ((zero? amount) (list hundred twenty ten five one))
;;                   ((>= amount 100) (add1 hundred) (display hundred) (change (- amount 100)))
;;                   ((>= amount 20) (add1 twenty) (change (- amount 20)))
;;                   ((>= amount 10) (add1 ten) (change (- amount 10)))
;;                   ((>= amount 5) (add1 five) (change (- amount 5)))
;;                   (else (add1 one) (change (sub1 amount)))))))
;;       (change init-amount))))
;; 
;; 
;; (define change-long
;;   (lambda (amount)
;;     (cond
;;       ((zero? amount) '())
;;       ((>= amount 100) (cons 100 (change-long (- amount 100))))
;;       ((>= amount 20) (cons 20 (change-long (- amount 20))))
;;       ((>= amount 10) (cons 10 (change-long (- amount 10))))
;;       ((>= amount 5) (cons 5 (change-long (- amount 5))))
;;       (else (cons 1 (change-long (sub1 amount)))))))

(define writeln
  (lambda args
    (for-each display args)
    (newline)))

;; I think this is a moment to celebrate even though I don't have a fcking idea how this came about lol.
(define display-change
  (lambda (bill amount)
    (let ((notes (floor (/ amount bill))))
      (begin
      (writeln notes " " (cond
                           ((= bill 100) "hundred")
                           ((= bill 20) "twenty")
                           ((= bill 10) "ten")
                           (else "five"))
               " dollar bills")
      (calculate-change (- amount (* bill notes)))))))

(define calculate-change
  (lambda (amount)
      (cond
        ((< amount 5) (writeln amount " dimes"))
        ((>= amount 100) (display-change 100 amount))
        ((>= amount 20) (display-change 20 amount))
        ((>= amount 10) (display-change 10 amount))
        (else (display-change 5 amount)))))

                           
               
 (define ask-for-change
   (lambda ()
     (display "Enter the amount to find change: ")
     (let ((amount (read)))
       (cond
         ((eq? amount 'done) (writeln "Okay. Adios"))
         (else
          (writeln "Your change is: ")
          (calculate-change amount)
          (ask-for-change))))))
;; (writeln "Your change is: ")
;; (define change-div
;;   (lambda (amount)
;;     (cond
;;       ((zero? amount) '())
;;       ((>= amount 100) (writeln (floor (/ amount 100)) " hundred dollar bill") (change-div (- amount (* (floor (/ amount 100)) 100))))
;;       ((>= amount 20) (writeln (floor (/ amount 20)) " twenty dollar bills") (change-div (- amount (* (floor (/ amount 20)) 20))))
;;       ((>= amount 10) (writeln (floor (/ amount 10)) " ten dollar bills") (change-div (- amount (* (floor (/ amount 10)) 10))))
;;       ((>= amount 5) (writeln (floor (/ amount 5)) " five dollar bills") (change-div (- amount (* (floor (/ amount 5)) 5))))
;;       (else (writeln amount " dimes") (change-div 0)))))










