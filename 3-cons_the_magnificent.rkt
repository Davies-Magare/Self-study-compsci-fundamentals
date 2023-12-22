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


