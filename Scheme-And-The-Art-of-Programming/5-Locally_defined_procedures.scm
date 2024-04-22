;;Exercises 5.1 to 5.8
;;I predict this this let expression will have a value of 5

(display
 (let ((a 5))
   (let ((fun (lambda (x) (max x a))))
     (let ((a 10)
	  (x 20))
       (fun 1)))))
;; Got that one right! Some progress finally

(display
  (let ((a 1) (b 2))
    (let ((b 3) (c (+ a b)))
      (let ((b 5))
	(cons a (cons b (cons c '())))))))
;;my prediction is that the value of the expression
;; will be (1 5 4)
;; The value actually was (1 5 3) and my guess at this
;; point is that if we have not finished setting up the environment
;; we can't use those values yet/we can't use the same values
;; we are binding in an environment to bind to other values in
;; the same environment.
;; Let me try this:

(display
  (let ((a 5))
    (let ((a 4) (c (+ 1 a)))
      (+ a c))))
;; I'm expecting the value here to be 10 since c will use the value
;; of a from the non local environment and not the one from a = 4 which
;; we are still defining.
;; Yes, my guess was right. I'll have to relook up the explanations in the
;; book to understand why this is happening.
;; If we break it down to a lambda expression
;; we'll have ((lambda (a) ((lambda (a c) (+ a c)) 4 (+ 1 a))) 5)
;; It now makes sense. Initially the variable a is bound to 5 then 
;; in evaluating the body a is bound to 4 and c is bound to 6, which makes
;; the inner let evaluate to 10.
;;
;;============================================================================|
;; And by the way by the definition of scope (+a c) is in the scope           |
;; of a = 4. ( + 1 a) is in the scope of a = 5 since the let expression is in |
;; the body of the outer expression. That is the correct explanation.         |
;;============================================================================|
;;
;; The next challenge is to rewrite the first two expression as pure lambda
;; expressions.
;; This is what I've come up with for the first expression:
;;

((lambda (a)((lambda (fun) ((lambda (a x) (fun 1)) 10 20))(lambda (x) (max x a)))) 5)
;;
;; That worked yay!
;; Let me try the second one
;; In fact the easiest way is to work from inside outward.
;;
((lambda (a b) ((lambda (b c) ((lambda (b) (cons a (cons b (cons c '())))) 5)) 3 (+ a b))) 1 2)
;;
;; Yes that works. The trick is to rewrite the innermost lambda, which is the body of the immediate
;; outer lambda. All you have to add is the lambda key word, the parameters and arguments.
;; The next challenge is to rewrite insert-left-all procedure using a locally defined procedure
;; that takes ls as its only argument. Currently ls looks like this:
(define insert-left-all
    (lambda (new old ls)
      (cond
        ((null? ls) '())
        ((equal? (car ls) old)
         (cons new (cons old (insert-left-all new old (cdr ls)))))
        ((pair? (car ls))
         (cons (insert-left-all new old (car ls)) (insert-left-all new old (cdr ls))))
        (else
          (cons (car ls) (insert-left-all new old (cdr ls)))))))
;;wow this could be interesting. We will use a letrec to define a recursive binding then
;;use the non local bindings of new and old such that we don't have to repeat them in 
;;successive recursive calls. Then the body of the letrec will be the insert-left-all-helper
;;with the ls only. Brilliant if you ask me
;;Easier said than done haha. But we got this. Lets go.

(define insert-left-all
  (lambda (new old ls)
    (let ((first (car ls)))
    ((letrec ((insert-helper
		(lambda (ls)
		  (cond
		    ((null? ls) '())
		    ((equal? first old)
		     (cons new (cons old (insert-helper(cdr ls)))))
		    ((pair? first)
		     (cons (insert-helper first) (insert-helper (cdr ls))))
		    (else
		      (cons first (insert-helper (cdr ls))))))))
       (insert-helper ls))))))
;;This is my first attempt. I used the a let expression to make a local binding for (car ls)
;;I'm not sure if I should have done the same for (cdr ls) but my thought is it will make the
;;whole function a whole mess.
;;Didn't work this time. I don't have a clue where I went wrong. Maybe I'll rewrite it again
;;Actually my approach is wrong. I cannot make a local binding of (car ls) because it is changing
;;in successive recursive calls. I guess just letrec will do it.
(define insert-left-all
  (lambda (new old ls)
    (letrec ((insert-helper
		(lambda (ls*)
		  (cond
		    ((null? ls*) '())
		    ((equal? (car ls*) old)
		     (cons new (cons old (insert-helper (cdr ls*)))))
		    ((pair? (car ls*))
		     (cons (insert-helper (car ls*)) (insert-helper (cdr ls*))))
		    (else
		      (cons (car ls*) (insert-helper (cdr ls*))))))))
       (insert-helper ls))))
;;The problem was an extra pair of parentheses that I had placed on letrec and it's return value was
;;being applied as an operator which should not happen.
;;
;;The next challenge is to write fib iterative 
;;I had even forgotten how to define it. We need to accumulators
;; to carry over the results to the next function  call.
(define fib-it-pre
  (lambda (n acc1 acc2)
    (cond
      ((< n 2) acc2)
      (else (fib-it-pre (sub1 n) acc2 (+ acc1 acc2))))))
;; I used pre to mean preliminary. The fib-it appetizer haha.
(define fib-it
  (lambda (n)
    (fib-it-pre n 1 1)))
;; new define with letrec
(define fib-it
  (lambda (n)
    (letrec ((fib-it-helper 
	       (lambda (n acc1 acc2)
		 (if (< n 2)
		     acc2
		     (fib-it-helper (sub1 n) acc2 (+ acc1 acc2))))))
      (fib-it-helper n 1 1))))
;; I think that should work. We create a local procedure to handle the recursion then
;; make the value of fib it a procedure that applies the fib-it-helper procedure on n.
;; I don't know if I'm saying the right thing.
;; Confirmed it works. I have to find out what is going on though.
