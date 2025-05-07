#lang scheme
(define tower-of-hanoi
  (lambda (n)
    (letrec
        ((move
          (lambda (n source destination helper)
            (cond
              ((= n 1)
               (list (list source destination)))
              (else
               (append
                (move (sub1 n) helper destination source)
                (cons
                 (move (sub1 n) source helper destination)
                 (list source destination))))))))
      (move n 'L 'R 'C))))

(define tower-of-hanoi-corr
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
