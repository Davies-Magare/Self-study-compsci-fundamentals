#lang scheme
(define make-set
  (lambda args
    (letrec
        ((list-make-set
          (lambda (args-list)
            (if (null? args-list)
                the-empty-set
                (adjoin
                 (car args-list)
                 (list-make-set (cdr args-list)))))))
      (list-make-set args))))

(define none
  (lambda (pred)
    (letrec
        ((test
          (lambda (s)
            (or (empty-set? s)
                (let ((elem (pick s)))
                  (and (not (pred elem))
                       (test ((residue elem) s))))))))
      test)))

(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(define there-exists
  (lambda (pred)
    (lambda (s)
      (not ((none pred) s)))))

;;Note that the result of the quantifier
;;is the value of applying not to the result of
;;((none pred) s) THE RESULT IS NOT A PROCEDURE BUT A VALUE
;;Hence in rewriting the function with compose we change the
;;last line to be the application of the result of (compose not (none pred)) on s
;;which then reduces to (compose not (none pred)) as the result of applying (compose not (none pred)) to
;;s is the same as that of applying (lambda (s) ((compose not (none pred)) s))


(define there-exists
  (lambda (pred)
    (compose not (none pred))))


(define for-all
  (lambda (pred)
    (none (compose not pred))))

(define set-equal
  (lambda (obj1)
    (lambda (obj2)
      (or (and ((neither set?) obj1 obj2)
               (equal? obj1 obj2))
          (and ((both set?) obj1 obj2)
               ((subset obj1) obj2)
               ((subset obj2) obj1))))))

;; (define element
;;   (lambda (obj)
;;     (there-exists (set-equal obj))))

(define element (compose there-exists set-equal))

(define contains
  (lambda (set)
    (lambda (obj)
      ((element obj) set))))


(define superset
  (lambda (s1)
    (lambda (s2)
      ((for-all (contains s1)) s2))))


(define subset
  (lambda (s1)
    (lambda (s2)
      ((superset s2) s1))))


(define cardinal
  (lambda (s)
    (if (empty-set? s)
        0
        (let ((elem (pick s)))
          (add1 (cardinal ((residue elem) s)))))))

;; (define intersection
;;   (lambda (s1 s2)
;;     (letrec
;;         ((helper
;;           (lambda (s1)
;;             (if (empty-set? s1)
;;                 the-empty-set
;;                 (let ((elem (pick s1)))
;;                   (if ((contains s2) elem)
;;                       (adjoin elem (helper ((residue elem) s1)))
;;                       (helper ((residue elem) s1))))))))
;;       (helper s1))))
;; 
;; 
;; (define union
;;   (lambda (s1 s2)
;;     (letrec
;;         ((helper
;;           (lambda (s1)
;;             (if (empty-set? s1)
;;                 s2
;;                 (let ((elem (pick s1)))
;;                   (if (not ((contains s2) elem))
;;                       (adjoin elem (helper ((residue elem) s1)))
;;                       (helper ((residue elem) s1)))))))))
;;     (helper s1)))
;; 
;; 
;; 
;; (define difference
;;   (lambda (s1 s2)
;;     (letrec ((helper (lambda (s1)
;;                        (if (empty-set? s1)
;;                            the-empty-set
;;                            (let ((elem (pick s1)))
;;                              (if (not ((contains s2) elem))
;;                                  (adjoin elem (helper ((residue elem) s1)))
;;                                  (helper ((residue elem) s1))))))))
;;       (helper s1))))


(define set-builder
  (lambda (pred base-set)
    (letrec
        ((helper
          (lambda (s)
            (if (empty-set? s)
                base-set
                (let ((elem (pick s)))
                  (if (pred elem)
                      (adjoin elem (helper ((residue elem) s)))
                      (helper ((residue elem) s))))))))
    helper)))

;; Rewrite the procedures using procedural abstraction

(define intersection
  (lambda (s1 s2)
    ((set-builder (contains s2) the-empty-set) s1)))

(define union
  (lambda (s1 s2)
    ((set-builder (compose not (contains s2)) s2) s1)))

(define difference
  (lambda (s1 s2)
    ((set-builder (compose not (contains s2)) the-empty-set) s1)))



(define family-union
  (lambda (s)
    (if (empty-set? s)
        the-empty-set
        (let ((elem (pick s)))
          (union elem (family-union ((residue elem) s)))))))


;; (define family-intersection
;;   (lambda (s)
;;     (letrec ((family-int (lambda (s)
;;                        (if (empty-set? s)
;;                            the-empty-set
;;                            (let ((elem (pick s)))
;;                              (intersection elem ((family-int ((residue elem) s)))))))))
;;       (family-int s))))


(define set-map
  (lambda (proc s)
    (if (empty-set? s)
        the-empty-set
        (let ((elem (pick s)))
          (adjoin (proc elem)
                  (set-map proc ((residue elem) s)))))))

(define list->set
  (lambda (ls)
    (apply make-set ls)))

(define set->list
  (lambda (s)
    (if (empty-set? s)
        '()
        (let ((elem (pick s)))
          (cons elem (set->list ((re sidue elem) s)))))))


;;Exercise 8.4
;;Defining there-exists in curried form
(define there-exists-ex
  (lambda (pred)
    (letrec
        ((test
          (lambda (s)
            (if (empty-set? s)
                #f
                (let ((elem (pick s)))
                  (or (pred elem) (test ((residue elem) s))))))))
      test)))

;;BEFORE SIMPLIFICATION WITH COMPOSITION
;; (define for-all-ex
;;   (lambda (pred)
;;     (lambda (s)
;;       (not ((there-exists (not pred)) s)))))


;;AFTER SIMPLIFICATION WITH COMPOSITION
;; (define for-all-ex
;;   (lambda (pred)
;;     (not (there-exists (not pred)))))

(define for-all-ex
  (not (compose there-exists not)))

;; (define none
;;   (lambda (pred)
;;     (lambda (s)
;;       (not ((there-exists pred) s)))))
;; (define none
;;   (lambda (pred)
;;     (not (there-exists pred))))

(define none
  (compose not there-exists))




































;; 
















