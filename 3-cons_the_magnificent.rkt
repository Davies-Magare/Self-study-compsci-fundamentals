;;rember removes a specific atom from a list of atoms
;;example
;;(rember a lat) where a is mint and lat is (lamb chops and mint jelly)
;;The new value for lat is (lamb chops and jelly) i.e

(define a 'mint)
(define lat '(lamb chops and mint jelly))
(rember a lat)
;; => '(lamb chops and jelly)

(define a 'toast)
(define lat '(bacon lettuce and tomato))
(rember a lat)
;; => '(bacon lettuce and tomato)
;;The atom toast is not in lat

(define a 'cup)
(define lat '(coffee cup tea cup and hick cup))
(rember a lat)
;; => '(coffee tea cup and hick cup)

;;I think the better definition of rember is that it 
;;removes the first occurence of an atom in a list of 
;;atoms.(inefficient because I forgot to include the argument part?)

;; Book definition: It takes an atom and a lat as its
;; arguments and makes a new lat with the first occurence of 
;; the atom in the old lat removed.

;; We first test (null? lat) i.e find out if the list of 
;; atoms is empty. If it is we return an empty list ()
;;otherwise we know that the list contains at least one atom
;;i.e if the list is not null.

;; If the list is not null we next ask if a is equal to the first
;; atom of the lat(atom? s expression?? I'll have to look that up)
;; We ask questions using (cond (__ __) (__ __)).


;; We ask if a is equal to (car lat) by (eq? (car lat) a).
;; So far:
;; If a is equal to (car lat) the value of the application
;; (rember a lat) is (cdr lat)
;; if that is not the case we keep car lat then look for a in the 
;; rest of lat i.e (cdr lat) we do that by referring to 
;; (rember a lat) with new arguments: (rember a (cdr lat)).


;;So far:
(define rember
  (lambda a lat
    (cond
      ((null? lat)(quote()))
      (else (cond
	      ((eq? (car lat) a) (cdr lat))
	      (else (rember a
		    (cdr lat))))))))
;; This function is not effective because while it
;; removes the target atoms, the atoms preceding it are also
;; lost. For example if our lat was (bacon lettuce and tomato),
;; we will recur to the function 2 times and the third time 
;; (eq? (car lat) a) will evaluate to true since lat will be 
;; (and tomato). The resulting value of the application --(tomato)--
;; is not the correct answer since we expected to get (bacon lettuce tomato)
;; To solve this problem we will rely on a third rule:

;; =================================================
;;  USE CONS TO BUILD LISTS.
;; =================================================

(define rember
  (lambda a lat
    (cond
      ((null? lat)(quote()))
      (else (cond
	      ((eq? (car lat) a) (cdr lat))
	      (else (cons (car lat) (rember a (cdr lat)))))))))

;; What is the value of (rember a lat) where a is and
;; and lat is (bacon lettuce and tomato)
;; We ask the first question which is if the lat is null.
;; No, move to the next line. else? of course else is always true
;; Ask if car lat which is bacon is equal to a which is and.. and the 
;; answer is no. Move to the next line and ask the next question. else..
;; of course.. cons [[bacon]] to the result of (rember a (cdr lat))
;; Since we do not know the value of (rember a (cdr lat)) we will have to find
;; that out.
;; (rember a (cdr lat)) means refer back to the function(recur) with new values
;; i.e a is and and lat is (lettuce and tomatoes). We ask the first question as usual.
;; is lat null? No; Move to the next line and ask the next question. The next question is else,
;; which is true, of course. We ask if car lat is eq to a. Lettuce is not equal to and so we move 
;; to the next line and ask the next question. else? Yes else is always true. We cons [[lettuce]] to
;; the value of (rember a (cdr lat)). Since we do not know this value, we have to find out what it is.
;; (rember a (cdr lat)) means we recur to the function with different arguments. a is still and.. and 
;; lat will be (and tomato).
;; (null? lat)? No. Move to the next line and ask the next question.
;; else? Yes, of course, else is always true.
;; (eq? (car lat) a) YES. (car lat) -- and-- is equal to a which is and.
;; Therefore the value of the application is (cdr lat) which is (tomato)
;; We are not finished though. Our goal was to evaluate (rember a lat) where
;; a was and and lat was (bacon lettuce and tomato); and (tomato) is not the correct
;; answer.
;; What does tomato represent? (tomato) is the value of (rember a (cdr lat)) where lat was
;; (lettuce and tomato). Since we now have this value we can cons lettuce onto it as we originally intended.
;; The result of consing lettuce onto (tomato) is (lettuce tomato).
;; We are still not done yet. 
;; What does (lettuce tomato) represent? (lettuce tomato) is the value of (rember(a (cdr lat)) where lat was
;; bacon lettuce and tomato. Since we now have the value we can cons bacon onto it and obtain the value of 
;; (cons (car lat) (rember a (cdr lat))). Our final result is (bacon lettuce tomato).
;; This is the answer to our original problem.

;; =======================================================
;; firsts: Checks whether a list contains non empty lists
;; then takes the first element of each inner list and builds a 
;; new list with it.
;; ============================================================
;; My version of this function would be:
(define firsts
  (lambda l
    (cond
      ((null? l)(quote())
       (else
	 (cons(car(car l)) (firsts (cdr l)))))))))

;;My answer was right!!
;;I'm still wondering what we'll do if the first element is not a list
;;For example what is (firsts l) where l is (dog (meows cat))??
;;=============================================================
;; The third commandment: When building a list cons the first
;; element to the natural recursion.
;;=============================================================
;;Example
;;(firsts l) where l is ((sugar)(butter)(water salt))
;; We ask whether l is null? No. Ask the next question else:
;; Yes, of course. We cons [[sugar]] to the value of (firsts (cdr l)),
;; cdr l being ((butter)(water salt)).
;; We answer(firsts(cdr l)) by recurring to the function with different arguments --((butter)(water salt))
;; Is l null? No else, Yes, of course. We cons [[butter]] to the value of 
;; (firsts (cdr l)) l being ((meows cat)) Is l null? No Next question. else.. Yes.
;; We cons [[water]] to the value of (firsts (cdr l)) l being (). Ask the first question
;; is l null? Yes.  The value of the application is ()
;; This is not the final answer, because we have more to do. What is ()? It is the value of
;; (firsts(cdr l)) where l was ((water salt)). We wanted to cons water to this value. So the 
;; answer to the whole expression is (water). 
;; But what is (water)? It is the answer to the question  (firsts (cdr l)) where l was
;;((butter)(water salt)) and we wanted to cons butter to this answer. So the final answer will be
;; (butter water). 
;; We are not done yet since (butter water) is the answer to (firsts (cdr l)) where l was
;; ((sugar)(butter)(water salt)). We wanted to cons sugar to this value. Therefore our final solution
;; will be (sugar butter water).
;; We are done now.
