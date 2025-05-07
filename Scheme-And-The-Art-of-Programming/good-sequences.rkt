#lang scheme
(define legal?
  (lambda (solution try)
    (cond
      ((null? solution) #t)
      ((and (< (length solution) 3) (member try solution)) #f)
      ((< (length solution) 3) #t)
      ((or (equal? (car solution) try) (equal? (cadr solution) try)) #f)
      (else
       (let ((p1 (car solution)) (p2 (cadr solution)) (p3 (caddr solution)))
         (if (equal? (list try p1) (list p2 p3)) #f #t))))))

(define solution?
  (lambda (legal-seq n)
    (= (length legal-seq) n)))

(define fresh-try 3)

(define build-solution
  (lambda (legal-seq n)
    (cond
      ((solution? legal-seq n) legal-seq)
      (else (forward fresh-try legal-seq n)))))

(define forward
  (lambda (try legal-seq n)
    (cond
      ((zero? try) (backtrack legal-seq n))
      ((legal? legal-seq try) (build-solution (cons try legal-seq) n))
      (else (forward (sub1 try) legal-seq n)))))

(define backtrack
  (lambda (legal-seq n)
    (cond
      ((null? legal-seq) '())
      (else (forward (sub1 (car legal-seq)) (cdr legal-seq) n)))))

(let ((sol1 (build-solution '() 5)))
  (let ((sol2 (backtrack sol1 5)))
    (let ((sol3 (backtrack sol2 5)))
      (list sol1 sol2 sol3))))


(define build-all-solutions
  (lambda (n)
    (letrec
        ((loop (lambda (sol)
                 (cond
                   ((null? sol) '())
                   (else (cons sol (loop (backtrack sol n))))))))
      (loop (build-solution '() n)))))
(build-all-solutions 7)







































