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

;; December 26th 5:20 a.m
;; The function insertR takes three arguments lat old and new. It builds
;; a new list with new inserted to the right of old.
;; If we had lat as (cow goat dog) and our old as goat and new as cat

(define lat'(cow goat dog))
(define old 'goat)
(define new 'cat)
(insertR lat old new)
;;=> '(cow goat cat dog)

;; The function insert r examines each s expression in lat
;; one at a time and compares them to old. If they are not the same
;; we save car lat to cons to the new list later and then keep checking
;; the rest of lat by recurring to the function with the cdr or lat
;; if we find an s expression that matches old we cons the cdr of lat with new
;; then cons the resulting expression to old such that we have old new cdr lat
;; then cons this value in order with the values that we had saved.

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat)(quote()))
      (else
	((eq? old (car lat)) (cons old (cons new (cdr lat))))
	(else
	  (cons (car lat) (insertR new old (cdr lat))))))))

;; example What is the value of the application (insertR new old lat)
;; where new is topping old is fudge and lat is (ice cream with fudge for dessert)
;; We ask the first question is lat null? No. So we move to the next question. else
;; Yes of course else is always true. Is ice equal to fudge? No. Ask the next question.
;; else? Yes of course: else is always true. We cons [[ice]] with (insertR new old (cdr lat))
;; Since we do not know this value we have to find out. Ask the first question: is lat null? No.
;; Ask the next question. else? Yes. is cream equal to fudge? No. So we ask the next question
;; else? Yes, of course. We cons [[cream]] to the value of (insertR new old (cdr lat)). Since we do not know
;; this value we have to find out by recurring to the function. We check again if lat is null? No. else? Yes.
;; Is with equal to fudge? No. else? Yes. cons [[with]] to the value of (insertR new old (cdr lat)). Recur.
;; Is lat null? No. Ask the next question. Is fudge equal to fudge? YES. So, the value of this application
;; is the result of (cons old (cons new (cdr lat))) which means we first cons new to (for dessert) to get
;; (topping for dessert) then cons old to get (fudge topping for dessert). We are not done yet. (fudge topping for dessert)
;; is the answer to (insertR new old (cdr lat)) where we wanted to cons with with. The value of this application then becomes
;; (with fudge topping for dessert). But what is (with fudge topping for dessert)? It is our answer to our recur to insertR which we
;; wanted to cons to "cream". So the value of this application becomes (cream with fudge topping for dessert). Are we done yet? No. 
;; (cream with fudge topping for dessert) is our answer to our recur to insertR which we wanted to cons with ice. After we cons
;; The result becomes (ice cream with fudge topping for dessert). This indeed is the answer to our problem (insertR new old lat) where
;; new was topping old was fudge and lat was (ice cream with fudge for dessert).
;;
;; insertL inserts new to the left of the first occurence of the atom old.
;; I guess this could be something like:
(define insertL
  (lambda new old lat
    (cond
      ((null? lat)(quote()))
      (else (cond
      (eq? old (car lat)) (cons new lat)
      (else 
	(cons (car lat)(insertL new old (cdr lat)))))))))

;; Yess I got it right. Instead of cons(new(cons(old (cdr lat)))) I used lat because it is the
;; cons of old and cdr lat; and all we have to do is cons new onto it.
;;
;; NOw let's try subst. subst replaces the first occurence of old in
;; lat with new. for example if new is topping and old is fudge
;; lat is (ice cream with fudge for dessert) the value is
;; (ice cream with topping for dessert)
;; I think that is not hard since all we have to do is drop the old and replace it with the new.
;; We will check if the car of lat is equal to old. If it is we just cons new to the cdr of lat then
;; cons the previous s expressions if any.
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat)(quote())) ;;first commandment
	(else
	  (cond
	  (eq? (car lat) old) (cons new (cdr lat))
	  (else
	    (cons (car lat) (subst(new old (cdr lat))))))))))

;; Correct!!
;;
;; Lets try subst2. (subst2 new o1 o2 lat) subst2 replaces either the
;; first occurence of o1 or of o2 with new.
;; For example where
;; new is vanilla
;; o1 is chocolate
;; o2 is banana
;; and lat is (banana ice cream with chocolate topping)
;; the value is (vanilla ice cream with chocolate topping)
;; What i can think of is we should compare car lat with both o1 and o2
;; in separate questions. If either question evaluates to true then we cons new its cdr
;; and this value is the value of the application and we cons that value with the atoms we had saved if any.
;; if we get to an empty list just con the empty list to the saved atoms in order.
(define subst2
  (lambda(new o1 o2 lat)
    (cond ;; first question
      ((null? lat)(quote()))
      (else ;; if the prev question is not true
	(cond ((eq (car lat) o1) (cons new (cdr lat)))
	  ((eq (car lat) o2) (cons new (cdr lat)))
	  (else
	    (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))
;; I got this wrong a bit for not paying attention but my methodology was right however.
;; We can replace the two eq with or which is more elegant.
((or (eq (car lat) o1) (eq (car lat) o2)) (cons new (cdr lat)))
;;
;; Write the function multirember which gives its final value with all occurences of a removed.
;; My reasoning was that if car lat is equal to a then we should recur with cdr lat to check if the
;; lat has more occurences of a i.e we should not save the value. If car lat is not equal to a
;; then we should save this atom as the new lat we are building should contain all atoms not equal to a
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else
	(cond
	  ((eq? a (car lat))(multirember a (cdr lat))) ;; i.e drop the car by recurring with cdr
	  (else
	    (cons (car lat) (multirember a (cdr lat))))))))) ;; i.e save the value to cons later with the value of recurring with fxn
;; Yess. I was right! I'm so proud of the progress.
;; For example if we have a as cup and lat as (coffee cup tea cup and hick cup) we want
;; (coffee tea and hick) Let me test to see whether the function works:
;; We ask the first question is the lat null? This is the first commandment and we should ask this always
;; No. So go to the next line and ask the next question. else? Yes else is a question that is always true.
;; We ask if coffee is equal to cup; and the answer is no. So, we move to the next line and ask the next
;; question. else? Of course. We cons [[coffee]] to the result of the natural recursion(third commandment) i.e
;; we recur  with cdr lat which is (cup tea cup and hick cup)
;; Is lat null? No.
;; Next. else? Yes. is cup equal to cup? Yes. We recur with (tea cup and hick cup) hence dropping
;; cup. 
;; Ask first question. Is lat null? NO.
;; Next. else? Yes. is tea equal to cup? No. Move to the next line and ask the next question
;; else? Yes. else is always true. cons [[tea]] to the result of the natural recursion: (multirember a (cdr lat))
;; So we recur with (cup and hick cup) 
;; Is lat null? No. Ask the next question.
;; else? Yes
;; is cup equal to cup? Yes. We recur with cdr lat hence dropping cup. Our new argument for lat is
;; (and hick cup)
;; Is lat null? No. Next question. else? Yes. Is and equal to cup? No. We move to the next line and ask the next
;; question. else? Yes. We cons [[and]] to the result of (multirember a (cdr lat)) 
;; Ask first question: is lat null? No. We move to the next line and ask the next question. 
;; else? Yes. Is hick equal to cup? No. We move to the next line and ask the next question.
;; We cons [[hick]] to the result of the recursion with cdr lat.
;; Ask the first question: is lat null? No. It is not empty.
;; Is cup equal to cup? Yes. So we recur with cdr lat which is now ().
;; First question: is lat null? YES. So the value of the application is ()
;; But we are not done yet. We have to cons () with the values we saved.
;; () is the result of the recursion where we wanted to cons hick. So afterwards we have
;; (hick). We cons hick to and to get (and hick). We then cons tea to (and hick) to have
;; (tea and hick) and then finally we cons coffee to (tea and hick) to get (coffee tea and hick) which is the
;; solution to our original problem.
;; We will cons other parts of chapter 3 with this part tomorrow!í¸…
