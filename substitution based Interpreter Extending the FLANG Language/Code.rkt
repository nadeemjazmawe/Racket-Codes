;; The Flang interpreter

#lang pl

#|
  The grammar:
    <FLANG> ::= <num>
              | <bool>
              | { + <FLANG> <FLANG> }
              | { - <FLANG> <FLANG> }
              | { * <FLANG> <FLANG> }
              | { / <FLANG> <FLANG> }
              | { with { <id> <FLANG> } <FLANG> }
              | <id>
              | { fun { <id> } <FLANG> }
              | { call <FLANG> <FLANG> }
              | { bool <boolean> }
              | { < <FLANG> <FLANG> }
              | { > <FLANG> <FLANG> }
              | { = <FLANG> <FLANG> }
              | { if <FLANG> {then-do <FLANG>} {else-do <FLANG>} }
              | { not <FLANG> > }


  Evaluation rules:

   subst:
 N[v/x] = N
 {+ E1 E2}[v/x] = {+ E1[v/x] E2[v/x]}
 {- E1 E2}[v/x] = {- E1[v/x] E2[v/x]}
 {* E1 E2}[v/x] = {* E1[v/x] E2[v/x]}
 {/ E1 E2}[v/x] = {/ E1[v/x] E2[v/x]}
 y[v/x] = y
 x[v/x] = v
 {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]} ; if y =/= x
 {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
 {call E1 E2}[v/x] = {call E1[v/x] E2[v/x]}
 {fun {y} E}[v/x] = {fun {y} E[v/x]} ; if y =/= x
 {fun {x} E}[v/x] = {fun {x} E}
 B[v/x] = B ;; B is Boolean
 {= E1 E2}[v/x] = {= E1[v/x] E2[v/x]}
 {> E1 E2}[v/x] = {> E1[v/x] E2[v/x]}
 {< E1 E2}[v/x] = {< E1[v/x] E2[v/x]}
 { not E}[v/x] = {not E[v/x]}
 {if Econd {then-do Edo} {else-do Eelse}}[v/x]
 = {if Econd[v/x] {then-do Edo[v/x]} {else-do
Eelse[v/x]}}

    eval: Evaluation rules:
 eval(N) = N ;; N is an expression for a numeric value
 eval({+ E1 E2}) = eval(E1) + eval(E2) \ if both E1 and E2
 eval({- E1 E2}) = eval(E1) - eval(E2) \ evaluate to numbers
 eval({* E1 E2}) = eval(E1) * eval(E2) / otherwise error!
 eval({/ E1 E2}) = eval(E1) / eval(E2) /
 eval(id) = error!
 eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
 eval(FUN) = FUN ; assuming FUN is a function expression
 eval({call E1 E2}) = eval(Ef[eval(E2)/x])
 if eval(E1)={fun {x} Ef}
 = error! otherwise

 eval(B) = B ;; B is an expression for a Boolean value
 eval({= E1 E2}) = eval(E1) = eval(E2) \ if both E1 and E2
 eval({> E1 E2}) = eval(E1) > eval(E2) \ evaluate to
 numbers
 eval({< E1 E2}) = eval(E1) < eval(E2) / otherwise error!
 eval({not E}) = not(eval(E)) /E may be anything
 eval({if Econd {then-do Edo} {else-do Eelse}})
 = eval(Edo) if eval(Econd) =/= false,
 eval(Eelse), otherwise.
  |#

(define-type FLANG
  [Num  Number]
  [Add  FLANG FLANG]
  [Sub  FLANG FLANG]
  [Mul  FLANG FLANG]
  [Div  FLANG FLANG]
  [Id   Symbol]
  [With Symbol FLANG FLANG]
  [Fun  Symbol FLANG]
  [Call FLANG FLANG]
  [Bool Boolean]
  [Bigger FLANG FLANG]
  [Smaller FLANG FLANG]
  [Equal FLANG FLANG]
  [Not FLANG]
  [If FLANG FLANG FLANG]) 

(: parse-sexpr : Sexpr -> FLANG)
;; to convert s-expressions into FLANGs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    ['True (Bool true)]
    ['False (Bool false)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name)) body)
        (Fun name (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
    [(list '= lhs rhs) (Equal (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '> lhs rhs) (Bigger (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '< lhs rhs) (Smaller (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'not exp) (Not (parse-sexpr exp))]
    [(cons 'if rest)
     (match sexpr
       [(list 'if cond-expr (list 'then-do then-expr) (list 'else-do else-expr)) (If (parse-sexpr cond-expr) (parse-sexpr then-expr) (parse-sexpr else-expr))]
       [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> FLANG)
;; parses a string containing a FLANG expression to a FLANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

(: subst : FLANG Symbol FLANG -> FLANG)
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
    [(Call l r) (Call (subst l from to) (subst r from to))]
    [(Fun bound-id bound-body)
     (if (eq? bound-id from)
         expr
         (Fun bound-id (subst bound-body from to)))]
    [(Bool b) expr]
    [(Equal l r) (Equal (subst l from to) (subst r from to))]
    [(Not b) (Not (subst b from to))]
    [(Bigger l r) (Bigger (subst l from to) (subst r from to))]
    [(Smaller l r) (Smaller (subst l from to) (subst r from to))]
    [(If l m r) (If (subst l from to) (subst m from to) (subst r from to)) ]))

;; The following function is used in multiple places below,
;; hence, it is now a top-level definition
(: Num->number : FLANG -> Number)
;; gets a FLANG -- presumably a Num variant -- and returns the
;; unwrapped number
(define (Num->number e)
  (cases e
    [(Num n) n]
    [else (error 'Num->number "expected a number, got: ~s" e)]))

(: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
;; gets a Racket numeric binary operator, and uses it within a FLANG
;; `Num' wrapper
(define (arith-op op expr1 expr2)
  (Num (op (Num->number expr1) (Num->number expr2))))

(: logic-op : (Number Number -> Boolean) FLANG FLANG -> FLANG)
;; gets a Racket Boolean binary operator (on numbers), and applies it
;; to two `Num' wrapped FLANGs
(define (logic-op op expr1 expr2)
  (Bool(op (Num->number expr1) (Num->number expr2))))

(: flang->bool : FLANG -> Boolean)
;; gets a Flang E (of any kind) and returns a its appropiate
;; Boolean value -- which is true if and only if E does not
;; represent false
;; Remark: the `flang->bool` function will also be top-level
;; since it's used in more than one place.
(define (flang->bool e)
  (cases e
    [(Bool e) (if e true false)]
    [else true]))


(: eval : FLANG -> FLANG)
;; evaluates FLANG expressions by reducing them to *expressions*
(define (eval expr)
  (cases expr
    [(Num n) expr]
    [(Add l r) (arith-op + (eval l) (eval r))]
    [(Sub l r) (arith-op - (eval l) (eval r))]
    [(Mul l r) (arith-op * (eval l) (eval r))]
    [(Div l r) (arith-op / (eval l) (eval r))]
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  (eval named-expr)))]
    [(Id name) (error 'eval "free identifier: ~s" name)]
    [(Fun bound-id bound-body) expr]
    [(Call fun-expr arg-expr)
     (let([fval (eval fun-expr)])
       (cases fval
         [(Fun bound-id bound-body)
          (eval (subst bound-body
                       bound-id
                       (eval arg-expr)))]
         [else (error 'eval "`call' expects a function, got: ~s"
                      fval)]))]
    [(Bool b) expr]
    [(Bigger l r) (logic-op > (eval l) (eval r))]
    [(Smaller l r) (logic-op < (eval l) (eval r))]
    [(Equal l r) (logic-op = (eval l) (eval r))]
    [(If l m r)
     (let ([if_condition(flang->bool (eval l))])
       (if if_condition (eval m) (eval r)))]
    [(Not exp) [Bool (not (flang->bool (eval exp)))]]))
 
(: run : String -> (U Number Boolean FLANG))
;; evaluate a FLANG program contained in a string
(define (run str)
  (let ([result (eval (parse str))])
    (cases result
      [(Num n) n]
      [(Bool b) b]
      [else result])))

;; tests
(test (run "{call {fun {x} {+ x 1}} 4}")
      => 5)
(test (run "{with {add3 {fun {x} {+ x 3}}}
                {call add3 1}}")
      => 4)
(test (run "{with {add3 {fun {x} {+ x 3}}}
                {with {add1 {fun {x} {+ x 1}}}
                  {with {x 3}
                    {call add1 {call add3 x}}}}}")
      => 7)

(test (run "True") => true)
(test (run "{not True}") => false)
(test (run "{not {not True}}") => true)
(test (run "False") => false)
(test (run "{not False}") => true)
(test (run "{> 3 44}") => false)
(test (run "{> 44 4}") => true)
(test (run "{< 15 5}") => false)
(test (run "{< 5 15}") => true)
(test (run "{= 8 2}") => false)
(test (run "{= 2 2}") => true)
(test (run "{if {= 30 30} {then-do 14} {else-do 52}}") => 14)
(test (run "{if True  {then-do 8} {else-do 2}}") => 8)
(test (run "{if 1  {then-do 8} {else-do 2}}") => 8)
(test (run "{with {y 3}{if {> y 0} {then-do {/ y 2}} {else-do y}}}") => 3/2)
(test (run "{with {y 0}{if {> y 13} {then-do {* y 3}} {else-do {= 2 y}}}}") => false)
(test (run "{with {y 5}{if {= y 5} {then-do {+ 0 y}} {else-do {* 5 y}}}}") => 5)
(test (run "{with {y 13}{if {< y 1} {then-do {/ 2 y}} {else-do {* y y}}}}") => 169)
(test (run "{if {> 27 12} {then-do True} {else-do {- 7 9}}}") => true)
(test (run "{if {= 8 7} {then-do True} {else-do {not True}}}") => false)
(test (run "{if {< 23 17} {then-do True} {else-do {+ 6 2}}}") => 8)
(test (run "{with {y 1} {if {> y 4} {then-do {* 3 y}} {else-do {- y 4}}}}") => -3)
(test (run "{with {y 3} {if {= y 5} {then-do True} {else-do {not False}}}}") => true)
(test (run "{with {y False} {if y {then-do 5} {else-do 3}}}") => 3)
(test (run "{with {y True} {if y {then-do {> 2 1}} {else-do 2}}}") => true)
(test (run "{with {y 0} {if {= y 0} {then-do {* 5 y}} {else-do {+ 5 y}}}}") => 0)
(test (run "{with {y 2} {if {> y 3} {then-do {* 8 y}} {else-do {+ 8 y}}}}") => 10)

(test (run "{with {foo {fun {y} {if {< y 2} {then-do y} {else-do {/ y 2}}}}} foo}") => (Fun 'y (If (Smaller (Id 'y) (Num 2)) (Id 'y) (Div (Id 'y) (Num 2)))))
(test (run "{with {foo {fun {y} {if {> y 3} {then-do {* 4 y}} {else-do {* 3 y}}}}} foo}") => (Fun 'y (If (Bigger (Id 'y) (Num 3)) (Mul (Num 4) (Id 'y)) (Mul (Num 3) (Id 'y)))))
(test (run "{with {foo {fun {y} {if {= y 2} {then-do True} {else-do {not True}}}}} foo}") => (Fun 'y (If (Equal (Id 'y) (Num 2)) (Bool #t) (Not (Bool #t)))))
(test (run "{with {foo {fun {y} {if {> 8 3} {then-do {* 4 2}} {else-do {* 2 3}}}}} foo}") => (Fun 'y (If (Bigger (Num 8) (Num 3)) (Mul (Num 4) (Num 2)) (Mul (Num 2) (Num 3)))))


;; error tests
(test (run "true") =error> "eval: free identifier: true")
(test (run "{< false 5}") =error> "eval: free identifier: false")
(test (run "{< true 7}") =error> "eval: free identifier: true")
(test (run "{< x 7}") =error> "eval: free identifier: x")
(test (run "{< y y}") =error> "eval: free identifier: y")
(test (run "{< False 5}") =error> "Num->number: expected a number, got: #(struct:Bool #f)")
(test (run "{< 8 True}") =error> "Num->number: expected a number, got: #(struct:Bool #t)")
(test (run "{with {foo {fun {x} {if {= x 7} {then-do {+ 3 8}} {else-do {* 7 2}}}}} {foo {fun {x}}}}") =error> "parse-sexpr: bad syntax in (foo (fun (x)))")
(test (run "{with {x 2} {if {> x 3} {then-do x} {else-do {}}}}") =error> "parse-sexpr: bad syntax in ()")
(test (run "{with {foo {fun {x} {if {= x 1} {then-do {+ x 8}} {else-do {* x 2}}}}} fun}") =error> "eval: free identifier: fun")
(test (run "{with {x 0} {if {> x 0} {/ 2 x} x}}") =error> "parse-sexpr: bad `if' syntax in (if (> x 0) (/ 2 x) x)")


