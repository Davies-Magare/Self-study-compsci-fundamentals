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

