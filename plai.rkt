#lang plait

(define-type Exp
  [numE (n : Number)]
  [boolE (b : Boolean)]
  [plusE (left : Exp) (right : Exp)]
  [cndE (test : Exp) (then : Exp) (else : Exp)])

(define-type Value
  [numV (the-number : Number)]
  [boolV (the-boolean : Boolean)])

; Add ONLY deals with values
; (add: (Value, Value -> Value)
(define (add v1 v2)
  (type-case Value v1
    [(numV n1)
     (type-case Value v2
       [(numV n2) (numV (+ n1 n2))]
       [else (error '+ "expects RHS to be a number")])]
    [else (error '+ "expects LHS to be a number")]))

; E -> V
; Calc takes an expression and evaluates it to a value
(define (calc e)
  (type-case Exp e
    [(numE n) (numV n)]
    [(boolE b) (boolV b)]
    [(plusE l r) (add (calc l) (calc r))]
    [(cndE c t e) (numV 0)])) ; For now, cndE just returns 0

; parse: s-exp -> Exp
(define (parse s)
  (cond
    [(s-exp-number? s)
     (numE (s-exp->number s))]
    [(s-exp-list? s)
     (let ([l (s-exp->list s)])
       (if (symbol=? '+ (s-exp->symbol (first l)))
           (plusE (parse (second l)) (parse (third l)))
           (error 'parse "list not an addition")))]
    [else (error 'parse "unknown expression")]))

(define (run s)
  (calc (parse s)))

; Test cases
(test (run `1) (numV 1))
(test (run `-1) (numV -1))
(test (run `{+ 1 2}) (numV 3))
