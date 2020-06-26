
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Q1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The ROL BNF and Parsing code:
#lang pl
;; Defining two new types
(define-type BIT = (U 0 1))
(define-type Bit-List = (Listof BIT))


;; The actual interpreter
#| BNF for the RegE language:
 <ROL> ::= {reg-len = <num> <RegE>}
 <RegE> ::= <Bits>
            | {and <RegE> <RegE>}
            | {or <RegE> <RegE>}
            | {shl <RegE>}
 <Bits> ::= <BIT> | <BIT> <Bits>   => <BIT> = (U 0 1)
 |#
;; RegE abstract syntax trees
(define-type RegEbol
 [Reg Bit-List]
 [And RegE RegE]
 [Or RegE RegE]
 [Shl RegE])


;; Next is a technical function that converts (casts)
;; (any) list into a bit-list. We use it in parse-sexpr.
 (: list->bit-list : (Listof Any) -> Bit-List)
 ;; to cast a list of bits as a bit-list
 (define (list->bit-list lst)
 (cond [(null? lst) null]
 [(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]
 [else (cons 0 (list->bit-list (rest lst)))]))


(: parse-sexpr : Sexpr -> RegE)
 ;; to convert the main s-expression into ROL
 (define (parse-sexpr sexpr)
 (match sexpr
   [(list 'reg-len `= (number: n) args)
     (if(> n 0)
        (parse-sexpr-RegL args n)
        (error 'parse-sexpr "Error the length most to be at least 1 in ~s" sexpr))]
   [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))


 (: parse-sexpr-RegL : Sexpr Number -> RegE)
 ;; to convert s-expressions into RegEs
 (define (parse-sexpr-RegL sexpr reg-len)
 (match sexpr
 [(list (and a (or 1 0)) ... ) (if(= reg-len(length a)) (Reg(list->bit-list a)) (error 'parse-sexpr "wrong number of bits in ~s" a))]
 [(list 'and sexpr1 sexpr2) (And(parse-sexpr-RegL sexpr1 reg-len)(parse-sexpr-RegL sexpr2 reg-len))]
 [(list 'or sexpr1 sexpr2) (Or(parse-sexpr-RegL sexpr1 reg-len)(parse-sexpr-RegL sexpr2 reg-len))]
 [(list 'shl list) (Shl(parse-sexpr-RegL list reg-len))]
 [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))
 

(: parse : String -> RegE)
 ;; parses a string containing a RegE expression to a RegE AST
 (define (parse str)
 (parse-sexpr (string->sexpr str)))


;; tests
 (test (parse "{ reg-len = 4 {1 0 0 0}}") => (Reg '(1 0 0 0)))
 (test (parse "{ reg-len = 4 {shl {1 0 0 0}}}") => (Shl (Reg '(1 0 0 0))))
 (test (parse "{ reg-len = 4 {and {shl {1 0 1 0}} {shl {1 0 1 0}}}}") => (And (Shl (Reg
'(1 0 1 0))) (Shl (Reg '(1 0 1 0)))))
 (test (parse "{ reg-len = 4 { or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") =>
(Or (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))
 (test (parse "{ reg-len = 2 { or {and {shl {1 0}} {1 0}} {1 0}}}") => (Or (And (Shl
(Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))))
 (test (parse "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Q2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Q2.1:-
   {* {+ {set 1} {set 2}} get}
int the set function we can't know wich set will work first , so the get function well returntha last set
the answer will not know , because we don't know witch set have been first.

<MAE> ::= {seq <set>} | {seq <num>}

<set> ::= {set <AE> get} | {set <set> <AE> get}

<AE> ::= <num>
 | { + <AE> <AE> }
 | { - <AE> <AE> }
 | { * <AE> <AE> }
 | { / <AE> <AE> }
 | get

derivation process(for My ID - 314638867):-

1) <MAE>
   {seq <set>}
   {seq { set <AE> get}}
   {seq {set {+ <AE> <AE>} get}}
   {seq {set {+ <num> <num>} get}}
   {seq {set {+ 31463 8867} get}

2) <MAE>
   {seq <set>}
   {seq { set <AE> get}}
   {seq {set {+ <AE> <AE>} get}}
   {seq {set {+ {+ <AE> <AE>}  <num>} get}}
   {seq {set {+ {+ <num> <num>}  <num>} get}}
   {seq {set {+ {+ 314 638} 867} get}

3) <MAE>
   {seq <set>}
   {seq { set <set> <AE> get}}
   {seq { set { set <AE> get } <AE> get}}
   {seq { set { set <num> get } <num> get}}
   {seq { set { set <31463> get } <8867> get}}

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Q3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
These function take a list of Numbers and square the numbers in the list , and calculate the sum of it and return it.
|#

(: sum-of-squares : (Listof Natural) -> Natural)
#|
it square the number in the list and enter it to map , and it use the foldl function to calculate it.
this function use the squareNum to square the numbers.
|#
(define(sum-of-squares list)
  (foldl + 0 (map squareNum list)))

#|
these function accept natural Number and calculate the square of this number , and return it .
|#
 ( : squareNum : Natural -> Natural)
;; it take the number(n) and multiply it with themself and return the result.
   (define(squareNum n)
     (* n n))

;; test:-
(test(sum-of-squares '()) => 0)
(test(sum-of-squares '(1)) => 1)
(test(sum-of-squares '(1 2)) => 5)
(test(sum-of-squares '(1 2 3)) => 14)
(test(sum-of-squares '(1 3)) => 10)
(test(sum-of-squares '(2 3)) => 13)
(test(sum-of-squares '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) => 20)
(test(sum-of-squares '(1 2 3 4)) => 30)
(test(sum-of-squares '(1 2 3 4 5)) => 55)
(test(sum-of-squares '(7 8)) => 113)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Q4;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
In this code we gonna write a code for binary tree

First I write the define-type of the binary tree , I call it BINTREE .
This define have two variants that most have in each Leaf , first one is the Node , that have
data(Number) or two Binnary tree(left leaf and right leaf).

BINTREE :== <num>
          | {node <BINTREE> <BINTREE>}

|#
(define-type BINTREE
  [Leaf Number]
  [Node BINTREE BINTREE])

#|
after that I type function call tree-map , its an function that take a binarry tree(BINTREE) , and
some function(that take a Number and return a Number in our case) and it make this function work
on all the binary tree , and return a new binnary tree that have the new values after activation
the function on every leaf alone .
|#
(: tree-map : (Number -> Number) BINTREE -> BINTREE)
(define (tree-map fun leaf)
  (cases leaf
    [(Node leftLeaf rightLeaf) (Node (tree-map fun leftLeaf) (tree-map fun rightLeaf))]
    [(Leaf n) (Leaf (fun n))]
  ))

;; tests:-
;;in our test we use add1 function that racket define it , so we don't need to define it.
(test (tree-map add1 (Leaf 3)) => (Leaf 4))
(test (tree-map add1 (Node (Leaf 5) (Leaf 6))) => (Node (Leaf 6) (Leaf 7)))
(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
(test (tree-map add1 (Node (Node (Leaf 5) (Leaf 6)) (Node (Leaf 2) (Leaf 3)))) => (Node (Node (Leaf 6) (Leaf 7)) (Node (Leaf 3) (Leaf 4))))


#|
now I have bulid a tree-fold Function , that take two function and a binarytree(BINTREE) inputs ,
and combenation the functions in the input binarytree(BINTREE)
|#
(: tree-fold : (All (A) (A A -> A) (Number -> A) BINTREE -> A))
(define (tree-fold fun secondFun binarytree)
  (cases binarytree
    [(Leaf n) (secondFun n)]
    [(Node leaftNode rightNode) (fun (tree-fold fun secondFun leaftNode) (tree-fold fun secondFun rightNode))]
  )
)


#|
here we implement a tree-flatten function that consumes a BINTREE and returns a list of its values
 from left to right
|#
(: tree-flatten : BINTREE -> (Listof Number))
;; flattens a binary tree to a list of its values in
;; left-to-right order
(define (tree-flatten tree)
 (tree-fold (inst append Number) (inst list Number)
tree))


#|
The last thing we well do , is building the tree-reverse function that revers the binary tree(BINTREE) ,
it so easy , if we got leaf we return it , and if we got node , we reflects the leaft node and the right node.
|#
(: tree-reverse : BINTREE -> BINTREE)
(define (tree-reverse binarytree)
  (cases binarytree
    [(Leaf n) (Leaf n)]
    [(Node leaftNode rightNode) (Node (tree-reverse rightNode) (tree-reverse leaftNode))]
  ))

;;tests:-
(test (tree-reverse (Leaf 15)) => (Leaf 15))
(test (tree-reverse (Node (Leaf 1) (Leaf 2))) => (Node (Leaf 2) (Leaf 1)))
(test (tree-reverse (Node (Node (Leaf 4) (Leaf 5)) (Leaf 6))) => (Node (Leaf 6) (Node (Leaf 5) (Leaf 4))))

;;another test to test the tree-reverse function and the tree-flatten function:-
(test (equal? (reverse (tree-flatten (Node (Node (Leaf 4) (Leaf 5)) (Leaf 6))))
 (tree-flatten (tree-reverse (Node (Node (Leaf 4) (Leaf 5)) (Leaf 6))))) => #t)



