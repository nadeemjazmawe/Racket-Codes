  #lang pl

;;                                            First Part
  #| BNF for the MUWAE language:
       <MUWAE> ::= <num>
               | { + <MUWAE> <MUWAE> }
               | { - <MUWAE> <MUWAE> }
               | { * <MUWAE> <MUWAE> }
               | { / <MUWAE> <MUWAE> }
               | { with { <id> <MUWAE> } <MUWAE> }
               | <id>
               | { sqrt <MUWAE> }
  |#

  ;; MUWAE abstract syntax trees
  (define-type MUWAE
    [Num  (Listof Number)]
    [Add  MUWAE MUWAE]
    [Sub  MUWAE MUWAE]
    [Mul  MUWAE MUWAE]
    [Div  MUWAE MUWAE]
    [Id   Symbol]
    [With Symbol MUWAE MUWAE]
    [Sqrt MUWAE])

  (: parse-sexpr : Sexpr -> MUWAE)
  ;; to convert s-expressions into MUWAEs
  (define (parse-sexpr sexpr)
    (match sexpr
      [(number: n)    (Num (list n))]
      [(symbol: name) (Id name)]
      [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name) named) body)
          (With name (parse-sexpr named) (parse-sexpr body))]
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
      [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
      ;;adding the sqrt parse before end of the ending of parse-sexpr function
      [(list 'sqrt e) (Sqrt (parse-sexpr e))]
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

  (: parse : String -> MUWAE)
  ;; parses a string containing a MUWAE expression to a MUWAE AST
  (define (parse str)
    (parse-sexpr (string->sexpr str)))

  #| Formal specs for `subst':
     (`N' is a <num>, `E1', `E2' are <MUWAE>s, `x' is some <id>, `y' is a
     *different* <id>)
        N[v/x]                = N
        {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
        {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
        {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
        {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
        y[v/x]                = y
        x[v/x]                = v
        {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
        {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
  |#

  (: subst : MUWAE Symbol MUWAE -> MUWAE)
  ;; substitutes the second argument with the third argument in the
  ;; first argument, as per the rules of substitution; the resulting
  ;; expression contains no free instances of the second argument
  (define (subst expr from to)
    (cases expr
      [(Num n) expr]
      [(Add l r) (Add (subst l from to) (subst r from to))]
      [(Sub l r) (Sub (subst l from to) (subst r from to))]
      [(Mul l r) (Mul (subst l from to) (subst r from to))]
      [(Div l r) (Div (subst l from to) (subst r from to))]
      [(Id name) (if (eq? name from) to expr)]
      [(With bound-id named-expr bound-body)
       (With bound-id
             (subst named-expr from to)
             (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]
      ;;adding the sqrt
      [(Sqrt e) (Sqrt (subst e from to))]))

  #| Formal specs for `eval':
       eval(N)         = N
       eval({+ E1 E2}) = eval(E1) + eval(E2)
       eval({- E1 E2}) = eval(E1) - eval(E2)
       eval({* E1 E2}) = eval(E1) * eval(E2)
       eval({/ E1 E2}) = eval(E1) / eval(E2)
       eval(id)        = error!
       eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
       eval({sqrt E}) = sqrt(eval E)
  |#

  (: eval : MUWAE -> (Listof Number))
  ;; evaluates MUWAE expressions by reducing them to numbers
  (define (eval expr)
    (cases expr
      [(Num n) n]
      [(Add l r) (bin-op + (eval l) (eval r))]
      [(Sub l r) (bin-op - (eval l) (eval r))]
      [(Mul l r) (bin-op * (eval l) (eval r))]
      [(Div l r) (bin-op / (eval l) (eval r))]
      ;;adding the sqrt in eval:
      [(Sqrt e) (sqrt+ (eval e))] 
      [(With bound-id named-expr bound-body)
       (eval (subst bound-body
                    bound-id
                    (Num (eval named-expr))))]
      [(Id name) (error 'eval "free identifier: ~s" name)]))

;;adding the sqrt function :
(: sqrt+ : (Listof Number) -> (Listof Number))
;; a version of `sqrt' that takes a list of numbers, and return a list
;; with twice the elements, holding the two roots of each of the inputs;
(define (sqrt+ exp)
  (cond
    [(null? exp) exp]
    [(< (first exp) 0) (error 'sqrt+ "`sqrt' requires a nonnegative input")]
    [else (let ([x (first exp)])
          (if (> x -1)
              (cons (sqrt x) (cons (- 0 (sqrt x)) (sqrt+ (rest exp))))
              (error 'sqrt+ "`sqrt' requires a nonnegative input")))]))


(: bin-op : (Number Number -> Number) (Listof Number) (Listof Number) -> (Listof Number)) 
;; applies a binary numeric function on all combinations of numbers from 
;; the two input lists, and return the list of all of the results 
(define (bin-op op ls rs) 
  (: helper : Number (Listof Number) -> (Listof Number)) 
  (define (helper l rs) 
    (: f : Number -> Number)
    (define (f n)
      (op l n)) 
    (map f rs)) 
  (if (null? ls)
      null
      (append (helper (first ls) rs) (bin-op op (rest ls) rs)))) 




  (: run : String -> (Listof Number))
  ;; evaluate a MUMUWAE program contained in a string
  (define (run str)
    (eval (parse str)))



;;this the givven test , i typed a new test down :P
#|
  ;; tests
  (test (run "5") => 5)
  (test (run "{+ 5 5}") => 10)
  (test (run "{with {x {+ 5 5}} {+ x x}}") => 20)
  (test (run "{with {x 5} {+ x x}}") => 10)
  (test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => 14)
  (test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") => 4)
  (test (run "{with {x 5} {+ x {with {x 3} 10}}}") => 15)
  (test (run "{with {x 5} {+ x {with {x 3} x}}}") => 8)
  (test (run "{with {x 5} {+ x {with {y 3} x}}}") => 10)
  (test (run "{with {x 5} {with {y x} y}}") => 5)
  (test (run "{with {x 5} {with {x x} x}}") => 5)
  (test (run "{with {x 1} y}") =error> "free identifier")
 

{call {fun {x}
        {* x x}}
      5}


sqr = {fun {x}
        {* x x}}
{+ {sqr 5}
   {sqr 6}}

[parameter: 'x
 body:  (Mul (Id 'x) (Id 'x))
 ]

((lambda (x)
         (* x x)) 5)
(f 5)


(test (run "{sqrt 9}") => 3)
(test (run "{sqrt 1}") => 1)
(test (run "{sqrt 0}") => 0)
(test (run "{sqrt -1}") =error> "`sqrt' requires a nonnegative input")
|#
;;other given tests:
(test (run "{+ {sqrt 1} 3}") => '(4 2))
(test (run "{+ {/ {+ {sqrt 1} 3} 2} {sqrt 100}}") => '(12 -8 11 -9))
(test (run "{sqrt {+ 16 {* {+ 1 {sqrt 1}} {/ 9 2}}}}") => '(5 -5 4 -4))

;; My tests:-
(test (run "123") => '(123))
(test (run "{+ 15 5}") => '(20))
(test (run "{- 5 5}") => '(0))
(test (run "{* 5 5}") => '(25))
(test (run "{/ 121 11}") => '(11))
(test (run "{with {num {+ 1 1}} num }") => '(2))
(test (run "{with {num 1} {+ num num}}") => '(2))
(test (run "{with {num {+ 1 1}} {with {num2 {- num 2}} {+ num2 num2}}}") => '(0))
(test (run "{with {num 1} {with {num2 {- num 3}} {+ num2 num2}}}") => '(-4))
(test (run "{with {num 1} {+ num {with {num 3} 10}}}") => '(11))
(test (run "{with {num 1} {+ num {with {num 3} num}}}") => '(4))
(test (run "{with {num 1} {+ num {with {num2 3} num}}}") => '(2))
(test (run "{with {num 1} {+ num {with {num2 3} {/ 6 num2}}}}") => '(3))
(test (run "{with {num 1} {+ num {with {num2 2} {* num2 num}}}}") => '(3))
(test (run "{with {num 1} {with {num2 num} num2}}") => '(1))
(test (run "{with {num 1} {with {num num} num}}") => '(1))
(test (run "{sqrt 9}") => '(3 -3))
(test (run "{sqrt 1}") => '(1 -1))
(test (run "{sqrt 0}") => '(0 0))
(test (run "{with {num 1} {with {num2 {- num 1}} {sqrt num2}}}") => '(0 0))
(test (run "{+ {sqrt 1} 3}") => '(4 2))
(test (run "{+ {/ {+ {sqrt 1} 3} 2} {sqrt 100}}") => '(12 -8 11 -9))
(test (run "{sqrt {+ 16 {* {+ 1 {sqrt 1}} {/ 9 2}}}}") => '(5 -5 4 -4))
(test (run "{with {num {sqrt {+ 16 {* {+ 1 {sqrt 1}} {/ 9 2}}}}} num}") => '(5 -5 4 -4))
(test (run "{with {num {sqrt {+ 16 {* {+ 1 {sqrt 1}} {/ 9 2}}}}} {+ 1 num}}") => '(6 -4 5 -3))

(test (run "{+ {sqrt 1} 3}") => '(4 2))
(test (run "{+ {/ {+ {sqrt 1} 3} 2} {sqrt 100}}") => '(12 -8 11 -9))
(test (run "{sqrt {+ 16 {* {+ 1 {sqrt 1}} {/ 9 2}}}}") => '(5 -5 4 -4))

(test (run "{sqrt -1}") =error> "`sqrt' requires a nonnegative input")
(test (run "{with {num 1} num2}") =error> "free identifier")
(test (run "{/ {2 3 2} 25}") =error> "bad syntax")
(test (run "{with {num} {+ num num}}") =error> "bad")
(test (run "{with {3 {+ 5 5}} {+ num num}}") =error> "bad")
(test (run "{wth {num {+ 5 5}} {+ num num}}") =error> "bad syntax")


;;                                    Second Part


  #| BNF for the WAE language:
       <WAE> ::= <num>
               | { + <WAE> <WAE> }
               | { - <WAE> <WAE> }
               | { * <WAE> <WAE> }
               | { / <WAE> <WAE> }
               | { with { <id> <WAE> } <WAE> }
               | <id>
  |#

  ;; WAE abstract syntax trees
  (define-type WAE
    [Num2  Number]
    [Add2  WAE WAE]
    [Sub2  WAE WAE]
    [Mul2  WAE WAE]
    [Div2  WAE WAE]
    [Id2   Symbol]
    [With2 Symbol WAE WAE])

  (: parse-sexpr2 : Sexpr -> WAE)
  ;; to convert s-expressions into WAEs
  (define (parse-sexpr2 sexpr)
    (match sexpr
      [(number: n)    (Num2 n)]
      [(symbol: name) (Id2 name)]
      [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name) named) body)
          (With2 name (parse-sexpr2 named) (parse-sexpr2 body))]
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
      [(list '+ lhs rhs) (Add2 (parse-sexpr2 lhs) (parse-sexpr2 rhs))]
      [(list '- lhs rhs) (Sub2 (parse-sexpr2 lhs) (parse-sexpr2 rhs))]
      [(list '* lhs rhs) (Mul2 (parse-sexpr2 lhs) (parse-sexpr2 rhs))]
      [(list '/ lhs rhs) (Div2 (parse-sexpr2 lhs) (parse-sexpr2 rhs))]
      [else (error 'parse-sexpr2 "bad syntax in ~s" sexpr)]))

  (: parse2 : String -> WAE)
  ;; parses a string containing a WAE expression to a WAE AST
  (define (parse2 str)
    (parse-sexpr2 (string->sexpr str)))

  #| Formal specs for `subst':
     (`N' is a <Num2>, `E1', `E2' are <WAE>s, `x' is some <id>, `y' is a
     *different* <id>)
        N[v/x]                = N
        {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
        {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
        {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
        {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
        y[v/x]                = y
        x[v/x]                = v
        {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
        {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
  |#

  (: subst2 : WAE Symbol WAE -> WAE)
  ;; substitutes the second argument with the third argument in the
  ;; first argument, as per the rules of substitution; the resulting
  ;; expression contains no free instances of the second argument
  (define (subst2 expr from to)
    (cases expr
      [(Num2 n) expr]
      [(Add2 l r) (Add2 (subst2 l from to) (subst2 r from to))]
      [(Sub2 l r) (Sub2 (subst2 l from to) (subst2 r from to))]
      [(Mul2 l r) (Mul2 (subst2 l from to) (subst2 r from to))]
      [(Div2 l r) (Div2 (subst2 l from to) (subst2 r from to))]
      [(Id2 name) (if (eq? name from) to expr)]
      [(With2 bound-id named-expr bound-body)
       (With2 bound-id
             (subst2 named-expr from to)
             (if (eq? bound-id from)
               bound-body
               (subst2 bound-body from to)))]))



  (: freeInstanceList : WAE -> (Listof Any))
  ;; evaluates WAE expressions by reducing them to numbers
  (define (freeInstanceList expr)
    (cases expr
      [(Num2 n) '()]
      [(Add2 l r) (append (freeInstanceList l) (freeInstanceList r))]
      [(Sub2 l r) (append (freeInstanceList l) (freeInstanceList r))]
      [(Mul2 l r) (append (freeInstanceList l) (freeInstanceList r))]
      [(Div2 l r) (append (freeInstanceList l) (freeInstanceList r))]
      [(With2 bound-id named-expr bound-body) (append (freeInstanceList named-expr) (freeInstanceList (subst2 bound-body bound-id (Num2 0))))]
      [(Id2 name) (list name)]))

;; My Tests:-
(test (freeInstanceList (parse2 "WAE")) => '(WAE))
(test (freeInstanceList (parse2 "{with {num num}}")) =error> "bad")
(test (freeInstanceList (parse2 "{- num}")) =error> "bad")
(test (freeInstanceList (parse2 "{with {num 2} {with {num2 3} {+ {- num3 num2} num4}}}")) => '(num3 num4))
(test (freeInstanceList (With2 'x (Num2 2) (Add2 (Id2 'N) (Num2 3)))) => '(N))
(test (freeInstanceList (With2 'x (Num2 2) (Mul2 (Id2 'N) (Id2 'A)))) => '(N A))
(test (freeInstanceList (parse2 "{+ N {+ A N}}")) => '(N A N))
(test (freeInstanceList (parse2 "{+ N {* A N}}")) => '(N A N))
(test (freeInstanceList (parse2 "{with {N 1} {with {A 2} {with {D {/ N 3}} {* D D}}}}")) => '())


