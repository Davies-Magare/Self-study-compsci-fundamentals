#lang scheme
;; (define tower-of-hanoi
;;   (lambda (n)
;;     (letrec ((move
;;               (lambda (n source destination helper)
;;                 (if (= n 1)
;;                     (list (list source destination))
;;                     (append
;;                      (move (sub1 n) helper destination source)
;;                      (cons
;;                       (move (sub1 n) source helper destination)
;;                       (list source destination)))))))
;;       (move n 'L 'R 'C))))
;; 
;; (define tower-of-hanoi-1
;;   (lambda (n)
;;     (letrec ((move
;;               (lambda (n source destination helper)
;;                 (if (= n 1)
;;                     (list (list source destination))
;;                     (cons
;;                      (append
;;                       (move (sub1 n) source helper destination)
;;                       (list source destination))
;;                      (move (sub1 n) helper destination source))))))
;;       (move n 'L 'R 'C))))
;; 
;; 
;; (define tower-of-hanoi-2
;;   (lambda (n)
;;     (letrec ((move
;;               (lambda (n source destination helper)
;;                 (if (= n 1)
;;                     (list (list source destination))
;;                     (append
;;                      (move (sub1 n) source helper destination)
;;                      (cons
;;                       (list source destination)
;;                       (move (sub1 n) helper destination source)))))))
;;       (move n 'L 'R 'C))))
;; 
;; 
;; (define a '((1)))
;; (define b '((2)))
;; (define c '((3)))
;; 
;; (cons (append a b) c)
;; (append a (cons b c))


(define writeln
  (lambda args
    (for-each display args)
    (newline)))



(define display-tower-of-hanoi
  (let ((show-move (lambda (s d)
                      (display s)
                      (display " --> ")
                      (display d))))
     (lambda (n)
       (letrec
           ((move
             (lambda (n source destination helper)
               (if (= n 1)
                   (begin
                     (show-move source destination)
                     (newline))
                   (begin
                     (move (sub1 n) source helper destination)
                     (show-move source destination)
                     (display ", ")
                     (move (sub1 n) helper destination source))))))
         (move n 'L 'R 'C)))))


(define tower-of-hanoi-4
  (lambda (n)
    (letrec ((move
	           (lambda (n source destination helper)
			     (if (= n 1)
				   (list (list source destination))
				   (append
				   (move (sub1 n) source helper destination)
				   (cons (list source destination)
				         (move (sub1 n) helper destination source)))))))
		(move n 'L 'R 'C))))













































