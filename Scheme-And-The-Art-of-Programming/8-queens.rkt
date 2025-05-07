#lang scheme
(define writeln
  (lambda args
    (for-each display args)
    (newline)))
(define legal?
  (lambda (try legal-pl)
    (letrec
        ((good?
          (lambda (new-pl up down)
            (cond
              ((null? new-pl) #t)
              (else (let ((next-pos (car new-pl)))
                      (and
                       (not (= next-pos try))
                       (not (= next-pos up))
                       (not (= next-pos down))
                       (good? (cdr new-pl)
                             (add1 up)
                             (sub1 down)))))))))
      (good? legal-pl (add1 try) (sub1 try)))))

(define solution?
  (lambda (legal-pl)
    (= (length legal-pl) 8)))

(define fresh-try 8)

(define build-solution
  (lambda (legal-pl)
    (cond
      ((solution? legal-pl) legal-pl)
      (else (forward fresh-try legal-pl)))))

(define forward
  (lambda (try legal-pl)
    (cond
      ((zero? try) (backtrack legal-pl))
      ((legal? try legal-pl) (build-solution (cons try legal-pl)))
      (else (forward (sub1 try) legal-pl)))))

(define backtrack
  (lambda (legal-pl)
    (cond
      ((null? legal-pl) '())
      (else (forward (sub1 (car legal-pl)) (cdr legal-pl))))))

(let ((sol1 (build-solution '())))
  (let ((sol2 (backtrack sol1)))
    (let ((sol3 (backtrack sol2)))
      (list sol1 sol2 sol3))))


(define build-all-solutions
  (lambda ()
    (letrec
        ((loop (lambda (sol)
                 (cond
                   ((null? sol) '())
                   (else (cons sol (loop (backtrack sol))))))))
      (loop (build-solution '())))))

(define searcher
  (lambda (legal? solution? fresh-try)
    (letrec
        ((build-solution
          (lambda (legal-pl)
            (writeln "Build-Solution : " legal-pl)
            (cond
              ((solution? legal-pl) legal-pl)
              (else (forward fresh-try legal-pl)))))
         (forward
          (lambda (try legal-pl)
            (cond
              ((zero? try) (backtrack legal-pl))
              ((legal? try legal-pl) (build-solution (cons try legal-pl)))
              (else (forward (sub1 try) legal-pl)))))
         (backtrack
          (lambda (legal-pl)
            (writeln "Backtrack      : " legal-pl)
            (cond
              ((null? legal-pl) '())
              (else (forward (sub1 (car legal-pl)) (cdr legal-pl))))))
         (build-all-solutions
          (lambda ()
            (letrec
                ((loop (lambda (sol)
                         (cond
                           ((null? sol) '())
                           (else (cons sol (loop (backtrack sol))))))))
              (loop (build-solution '()))))))
      (build-all-solutions))))
