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

;; We keep the first ement of the list to build a new
;; list with the rest of the list that has had the first
;; occurence of a removed
;;

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
;;#Update: I've found out that since this function takes a lat as the argument
;; then we are sure that the list consists of only atoms considering the question
;; lat?
;;=============================================================
;; The third commandment: When building a list cons the first
;; element to the natural recursion.
;;=============================================================

;; December 26th 5:20 a.m
;; The function insertR takes three arguments lat old and new. It builds
;; a new list with new inserted to the right of old.
;; If we had lat as (cow goat dog) and our old as goat and new as cat

(define lat'(cow goat dog))
(define old 'goat)
(define new 'cat)
(insertR lat old new)
;;=> '(cow goat cat dog)

;; The function insertR examines each s expression in lat
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

;;EDIT 21 NOVEMBER 2024
;;Mastered the concept and used "or" intuitively.
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
;;Examples to be added




;; UPDATE THURSDAY NOVEMBER 21 2024

;; First version of multiinsertL
;; Inserts new to the rigth of every
;; occurence of old in a list of atoms
;; lat, recursively
;; version 1: Has a bug
;; old is to be replaced with (car lat)
;; in the correct version

(define multiinsertL
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        (else
          (cond
            ((eq? (car lat) old)
             (cons new (cons old
                         (multiinsertL new old (cdr lat)))))
            (else
              (cons old (multiinsertL new old (cdr lat)))))))))
;; > (multiinsertL 'fried 'fish '(chips and fish or fish and fried))
;;  (fish fish fried fish fish fried fish fish fish)
  
;; multirember corrected version:
(define multiinsertL
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        (else
          (cond
            ((eq? (car lat) old)
             (cons new (cons old
                         (multiinsertL new old (cdr lat)))))
            (else
              (cons (car lat) (multiinsertL new old (cdr lat)))))))))
;; > (multiinsertL 'fried 'fish '(chips and fish or fish and fried))
;;   (chips and fried fish or fried fish and fried)
;; this version works properly

;; Multisubst: Susbstitutes each occurence of old
;; in a list with new

(define multisubst
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        (else
          (cond
            ((eq? (car lat) old)
             (cons new (multisubst new old (cdr lat))))
            (else
              (cons (car lat) (multisubst new old (cdr lat)))))))))

;;Usage Example
;; (multisubst 'cat 'dogs '(meows and dogs always dogs and meows))
;;  (meows and cat always cat and meows)



;; MultiinsertR recursively inserts new to the right
;; of each occurence of old in the list 
;; lat 

(define multiinsertR
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        (else
          (cond
            ((eq? (car lat) old)
             (cons old (cons new
                         (multiinsertR new old (cdr lat)))))
            (else
              (cons (car lat) (multiinsertR new old (cdr lat)))))))))
;;Usage example
;; > (multiinsertR 'cat 'dog '(my dog and my other dog love dogs very much))
;;     (my dog cat and my other dog cat love dogs very much)

;; > (multiinsertR 'cat 'dog '())
      ()

;; Multirember recursively removes every occurence of a
;; from the list of atoms lat

(define multirember
    (lambda (a lat)
      (cond
        ((null? lat) '())
        (else
          (cond
            ((eq? (car lat) a)
             (multirember a (cdr lat)))
            (else
              (cons (car lat) (multirember a (cdr lat)))))))))
;; Usage example
;; > (multirember 'one '(one two one three one four one))
      (two three four)
