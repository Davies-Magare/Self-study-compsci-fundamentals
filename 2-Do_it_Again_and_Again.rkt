;; lat? Asks whether a list consists only of atoms. If yes it evaluates
;; to true; otherwise it evaluates to false.

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
	  (else #f))))

;; For example evaluate lat? l where l is (bacon and eggs)
;; We find out by asking the questions asked by cond. The first
;; question is if l is null? This is the first commandment and we should
;; ask this question when evaluating any function. l is not a null list in
;; this case so we move to the next line and ask the next question. The next
;; line asks the question if the car of l is an atom. If it is the value of the
;; application is the value of the question (lat? (cdr l)) or in other words we want
;; to find out if the rest of l also consists of only atoms by referring to the 
;; function with new arguments which are the rest of l or the cdr of l. In this case bacon
;; is an atom so we refer to lat? with new arguments (and eggs) The next question is if
;; l is a null list which it's not. We therefore move to the next line and ask the next
;; question. The next question asks if car l is an atom; and if it is we check whether the
;; rest of the list is made up of only atoms. and is an atom, so we refer back to the function with cdr l
;; which is (eggs) in this case. We ask the next question. Is l an empty list? No, l is not an empty list. 
;; We therefore move to the next line and ask the next question. It asks whether car l is an atom if it is
;; we recur to lat? with new arguments. This time l is () --Reading again I've noticed if l is () then it is a
;; lat because it does not contain a list. In other words any list that does not contain a list as an s expression
;; is a lat!. We ask the next question: is l null? Yes. l is () which is a null list. Therefore the value of the application
;; is true when l is (). But lat? (()) was the value of the application in which l was (eggs). The value of lat? (eggs) was the value of 
;; recurring with cdr (eggs) which was (). So the value of this application is true. But we the value of lat? eggs was the value of the application
;; in which our l was (and eggs) and our cdr l was (eggs) whose value we have now found to be true. Hence the lat? l where l is (and eggs) is also
;; true. But we are not done yet. The value of recurring with (and eggs) was the value of lat? (bacon and eggs). Hence if lat? (and eggs) is true,
;; then the value of lat? (bacon and eggs) is also true.

;;===============================================================================================================================
;; The member? function
;;===============================================================================================================================
(define member?
  (lambda(a lat)
    (cond
      ((null? lat) #f)
      (else
	(or (eq? (car lat) a) (member? a (cdr lat)))))))

;; what is (member? a lat) where a is meat and lat is (mashed potatoes and meat gravy)
;; We ask the first question null? lat and the value is not true so we ask the next question
;; else? Yes. The value of else is always true.
;; or..? Perhaps. We will find its value by asking each of its questions one at a time.
;; eq? mashed--car lat-- meat--a-- No
;; The value of or is the value of (member? a lat) where the argument for lat is the cdr of lat(potatoes and meat gravy)
;; The next question is null? lat which is not true since (potatoes and meat gravy) is not a null list
;; else? Yes. or.. We find out by asking each of its questions: eq? potatoes meat? No. The value of or is the value of
;; member? a lat where we use cdr lat as the new argument. In other words if a is not equal to the first atom of lat
;; check the rest of lat to find out whether it's there. We do that by recurring with cdr lat. 
;; The next question we ask is if lat is null? No
;; else? Yes. or..perhaps. Let's find out. eq? and meat? No. The value of or is therefore (member? a (cdr lat))
;; i.e we recur to the function with new arguments.
;; The next question is null? lat No. since (meat gravy) is not a null list
;; else? Yes. 
;; or.. lets find out. eq? meat meat? Yes. The value of or where lat is (meat gravy) is true. Therefore the value of the
;; function member? where lat is (meat gravy) is true or #t
;; The value of member? where lat is (and meat gravy) is #t. Therefore the value of member? where lat is (and meat gravy) is
;; true.
;; The value of or where lat is (potatoes and meat gravy) is true. Therefore the value of the function member? where lat is 
;; (potatoes and meat gravy) is true.
;; The value of or where lat is (mashed potatoes and meat gravy) is true. Therefore the value of the function member? where
;; lat is (potatoes and meat gravy) is also true. This was our original function.
1
