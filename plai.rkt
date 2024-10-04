#lang plait

(define-type Exp
  [numE (n : Number)]
  [plusE (left : Exp) (right : Exp)]
  [varE (name : Symbol)]
  [let1E (var : Symbol)
         (value : Exp)
         (body : Exp)])

(define-type Value
  [numV (the-number : Number)]
  [boolV (the-boolean : Boolean)])

; (boolean-decision (Value -> Value))
(define (boolean-decision v)
  (type-case Value v
    [(boolV b) b]
    [else (error 'if "expects conditional to evaluate to a boolean")]))

; Add ONLY deals with values
; (add: (Value, Value -> Value))
(define (add v1 v2)
  (type-case Value v1
    [(numV n1)
     (type-case Value v2
       [(numV n2) (numV (+ n1 n2))]
       [else (error '+ "expects RHS to be a number")])]
    [else (error '+ "expects LHS to be a number")]))



(define-type-alias Env (Hashof Symbol Value))
(define mt-env (hash empty))

(define (lookup (s : Symbol) (n : Env))
  (type-case (Optionof Value) (hash-ref n s)
    [(none) (error s "not bound")]
    [(some v) v]))

; (extend: (Env Symbol Value -> Env))
(define (extend old-env new-name value)
  (hash-set old-env new-name value))


;; (interp : (Exp Env -> Value))
(define (interp e nv)
  (type-case Exp e
    [(numE n) (numV n)] ; Return a numV instead of raw number (why did the textbook do n???)
    [(varE s) (lookup s nv)]
    [(plusE l r) (add (interp l nv) (interp r nv))]
    [(let1E var val body)
     (let ([new-env (extend nv
                            var
                            (interp val nv))])
       (interp body new-env))]))


; (parse: (s-exp -> Exp))
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
  (interp (parse s) mt-env))

; Test cases
(test (run `1) (numV 1))
(test (run `-1) (numV -1))
(test (run `{+ 1 2}) (numV 3))
