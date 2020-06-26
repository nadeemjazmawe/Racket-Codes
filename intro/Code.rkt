#lang pl

;; ---------------------------------- Question 1 ---------------------------------- 

 #|
plSuffixContained is function that accept List of strings and check if there is any
Sting end with "pl", if there is it will return it , if there is no string like this
it will return "false".
it work in recursion , it take first string in the list and check it , in case it has suffix "pl"
it return the string , if not , it will check the next string , and continue in this method until
the list end , if it don't find any string end with "pl" it will return "false".
|#


( : plSuffixContained : (Listof String) -> String)
(define ( plSuffixContained list)
  (: help : String -> Boolean)
  (define (help s)
    (cond [(< (string-length s) 2) #f]
          [(and (equal? #\p (string-ref s (- (string-length s) 2))) (equal? #\l (string-ref s (- (string-length s) 1)))) #t]
          [else #f]))
  (cond [(null? list) "false"]
        [(help (first list)) (first list)]
        [else (plSuffixContained (rest list)) ] ))



;;  TESTS :-

(test (plSuffixContained '()) => "false")
(test (plSuffixContained '("pllp" "plyy" "ppp" "lpTT" "lol")) => "false")
(test (plSuffixContained '("pl")) => "pl")
(test (plSuffixContained '("pllp" "plyy" "ppp" "lpTT" "lol" "plplpl" "lppl" )) => "plplpl")
(test (plSuffixContained '("pllp" "plyy" "ppp" "lpTT" "lol" "plplPl" "lppl" )) => "lppl")


;; ---------------------------------- Question 2.1  ----------------------------------
#|
Write-poly function take a list consumes/coefficients (numbers) a1, a2, ... , an and returns the polynomia
of this list.
the function send the list and the length of the list to help function , that type the polynomia.
help function work in tail-recursion , it check if the list is empty it will stop work , else it will type the polynomia according to X power ,
that will know it from the list length.
|#

( : write-poly : (Listof Number) -> String )
(define ( write-poly ls)
  ( : help : (Listof Number) Number String -> String)
  (define (help l num s)
    (cond [(null? l) s]
          [(< 1 num) (help (rest l) (- num 1) (string-append s (number->string (first l)) "x^" (number->string num) "+"))]
          [(< 0 num) (help (rest l) (- num 1) (string-append  s (number->string (first l)) "x+"))]
          [else (help (rest l) (+ num 1) (string-append s  (number->string (first l)) ))]))
   (cond [(null? ls) ""]
         [else (help ls (- (length ls) 1 )"")]))

;; TESTS :-
(test (write-poly '()) => "")
(test (write-poly '(7)) => "7")
(test (write-poly '(17 29)) => "17x+29")
(test (write-poly '(3 2 6)) => "3x^2+2x+6")
(test (write-poly '(7 8 9 10)) => "7x^3+8x^2+9x+10")





;; ---------------------------------- Question 2.2  ----------------------------------
#|
This function take a number as a X and a list of coefficient(numbers) , and it
calculate the polynomial and it return the result .
we use help function to calculate it in recursion , we enter the list in reverse , so now we
can calculate it like this an + an-1 * x ^ 1 + ..... + a1 * x ^ n-1 , so every time  we enter
the help function we multiply the x ^ n by x so we recive x ^ n+1 now we can know the power
of every x , in the start the x is 1 (x ^ 0 = 1) .
in every enter to the help function we summation => an * x ^ n-1 to recive the sum.
|#

( : compute-poly : Number (Listof Number) -> Number )
(define (compute-poly  x list )
  ( : help : (Listof Number) Number Number Number -> Number)
  (define (help ls newx x sum)
    (cond [(null? ls) sum]
          [else (help (rest ls) (* newx x) x ( + (* newx (first ls)) sum) )]))
  (cond [(null? list) 0]
        [else (help (reverse list) 1 x 0)]))

(test (compute-poly 2 '()) => 0)
(test (compute-poly 2 '(3 2 6)) => 22)
(test (compute-poly 3 '(4 3 -2 0)) => 129)
(test (compute-poly 1 '(17 28)) => 45)
(test (compute-poly 1 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) => 18)


;; ---------------------------------- Question 3.1 ----------------------------------
#|
definnig data structure , it have two constractors , the first one is empty constractor it call EmptyKS , and the second one
take Symbol String KeyStack variable it call keystack.
|#

(define-type KeyStack
  [EmptyKS]
  [keystack Symbol String KeyStack])

;; ---------------------------------- Question 3.2 ----------------------------------
#|
this method push a KeyStack  into our KeyStack , it push it to the start of the stack
|#
( : Push : Symbol String KeyStack -> KeyStack)
(define ( Push sym str key)
  (keystack sym str key))

;; ---------------------------------- Question 3.3 ----------------------------------
#|
this method take Symbol and a KeyStack , and it search in the Keystack a Symbol that equal to the Symbol that we have
give it to this function , in case it find this Symbol it return the String that the KeyStack hold.
in case it wearch all the KeyStack and dont find any Symbol that equal to the Sympol that we enter to the fumction ,
or we enter un empty KeyStack , it will return false.
|#
( : search-stack : Symbol KeyStack -> (U String Boolean))
(define (search-stack sym key)
  (cases key
    [(EmptyKS) #f]
    [(keystack newSym Str newKey) (cond [(equal? newSym sym) Str]
                                          [else (search-stack sym newKey)])]
    ))

;; ---------------------------------- Question 3.4 ----------------------------------
#|
this function take a KeyStack and delete the first KeyStack , and return the rest of the KeyStack.
in case we enter un empty KeyStack , it will return false
|#
( : pop-stack : KeyStack -> (U KeyStack Boolean))
(define (pop-stack key)
  (cases key
    [(EmptyKS) #f]
    [(keystack sym str ks) ks]))

;; TESTS :-
(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) =>(Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))=> (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)


;; ---------------------------------- Question 4 ----------------------------------
#|
  In this question we got full code with her tests , and the require is to add describe
  what the function takes as input, what it outputs, what its purpose is, and how it operates.
|#

(: is-odd? : Natural -> Boolean)
;; << Add your comments here>>
#|
  input : Natural Number
  outputs : True / false
  purpose : this function check if the Natural Number we enter is an odd Number.
  how it operates : it check if it an 0 , if it is it return false , else subtract
  from it 1 and enter it to the even function (is-even?) , until receives a zero .
|#
;; << Add your comments here>>
(define (is-odd? x)
 (if (zero? x)
 false
 (is-even? (- x 1))))


(: is-even? : Natural -> Boolean)
;; << Add your comments here>>
#|
  input : Natural Number
  outputs : True / False
  purpose : this function check if the Natural Number we enter is an even Number.
  how it operates : if the number we enter is equal to 0 , return True , else subtract
  from it 1 and enter it to the odd function (is-odd?) , until receives a zero .
|#
;; << Add your comments here>>
(define (is-even? x)
 (if (zero? x)
 true
 (is-odd? (- x 1))))
;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))
(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
;; See explanation about the All syntax at the end of the file...
;; << Add your comments here>>
#|
  input : A function and a list of A's function argument
  outputs : True / False
  purpose : check if all the list achieve the function
  how it operates : taking the first index on th list , running the function A on it , if we get false , it will stop
and return false value , if we run over all the list (accept null list) it will return True value.
|#
;; << Add your comments here>>
(define (every? pred lst)
 (or (null? lst)
 (and (pred (first lst))
 (every? pred (rest lst)))))
;; An example for the usefulness of this polymorphic function
(: all-even? : (Listof Natural) -> Boolean)
;; << Add your comments here>>
#|
  input : listof Natural
  outputs : True / False
  purpose : checking if all the index in the list is-even.
  how it operates : it send the list with is-even? function , to every? function , and every? function
check if the list is all-even.
|#
;; << Add your comments here>>
(define (all-even? lst)
 (every? is-even? lst))
;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))
(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) ->
Boolean))
;; << Add your comments here>>
#|
  input :  A function and a list of A's function argument , and  B function and a list of B's function argument
  outpots : True / False
  purpose : check if all the first list (lst1) achieve the A function , and if all the second list (lst2) achieve the B function
  how it operates : it run in list1 and list2 took the first index in two of list and enter it to function A and function B
  if we got false it stop and return false value , and if we run over the two lists(get null list) it will return true value.
|#
;; << Add your comments here>>
(define (every2? pred1 pred2 lst1 lst2)
 (or (null? lst1) ;; both lists assumed to be of same length
 (and (pred1 (first lst1))
 (pred2 (first lst2))
 (every2? pred1 pred2 (rest lst1) (rest lst2)))))


