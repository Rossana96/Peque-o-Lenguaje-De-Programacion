;Autor Rossana Palma López
#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "desugar.rkt")
(require "interp.rkt")

(print-only-errors)
(plai-ignore-exn-strings #t)
(test-inexact-epsilon 2)

(define (get-value expresion)
  (match expresion
    [(v*s value store) value]
    [else expresion]))


#| Módulo de pruebas unitarias para la Práctica 8 |#

;; Expresiones de prueba.

(define expr01 'foo)
(define expr02 '1729)
(define expr03 '12.46)
(define expr04 
    '{+ {- 1 2 3} 
        {* 4 5 6} 
        {/ 20 2 2} 
        {% 20 2 3} 
        {min 1 7 2 9} 
        {max 1 8 3 5} 
        {pow 2 3 4} 
        {sqrt 81}})
(define expr05
    '{with {{a 2} {b 3} {c 4}}
        {+ a b c}})
(define expr06
    '{with {{a {+ 2 3}} {b {* 7 4}} {c {max 1 7 2}}}
        {sqrt {% a b c}}})
(define expr07
    '{with {{a 2} {b 3} {c {+ 2 3}} {d {with {{e 1}} e}}}
        {max a b c d}})
(define expr08
    '{with* {{a 2} {b {+ a a}}}
        b})
(define expr09
    '{with* {{a 2} {b {+ a a}} {c {* a b}}}
        {with* {{d c} {e d}}
            {+ a b c d e}}})
(define expr10
    '{with* {{a x} {b {+ a a}}}
        {with {{x b}}
            x}})
(define expr11
    'false)
(define expr12
    'true)
(define expr13
    '{not {and {or {< 1 2 3} {<= 4 5 6} {= 7 7 7}} {or {/= 8 9 10} {> 11 12 13} {>=  14 15 16}}}})
(define expr14
    '{fun {x} x})
(define expr15
    '{fun {b h} {/ {* b h} 2}})
(define expr16
    '{fun {a b c} {/ {+ {* b -1} {sqrt {- {pow b 2} {* 4 a c}}}} {* 2 a}}})
(define expr17
    '{{fun {x} x} 1729})
(define expr18
    '{{fun {b h} {/ {* b h} 2}} 4 2})
(define expr19
    '{{fun {a b c} {/ {+ {* b -1} {sqrt {- {pow b 2} {* 4 a c}}}} {* 2 a}}} 1 7 10})
(define expr20
    '{with* {{a 2} {b 3} {c 4}
            {foo {fun {x y z} {+ a b c x y z}}}}
        {with {{a 5} {b 6} {c 7}}
            {foo 10 20 30}}})
(define expr21
    '{if {> 10 2}
         true
         false})
(define expr22
    '{with {{a 2} {b 4} {c 3}}
        {if {> a 2}
            {if {> a c}
                a
                c}
            {if {> b c}
                b
                c}}})
(define expr23
    '{cond
        {{> 3 5} true}
        {{< 5 3} true}
        {else false}})
(define expr24
    '{with* {{disc {fun {a b c} {- {pow b 2} {* 4 a c}}}}
            {valida {fun {x y z}
              {with* {{d {disc x y z}}}
                {cond
                  {{> d 0} 1}
                  {{= d 0} 2}
                  {else 3}}}}}}
        {valida 5 8 7}})
(define expr25
    '{with {{a 3} {b {/ 10 0}}}
        a})
(define expr26
    'empty)
(define expr27
    '{list 1 2 3 4 5})
(define expr28
    '{list true false true})
(define expr29
    '{rec {
           {fac
              {fun {n}
                 {if {zero? n}
                     1
                     {* n {fac {dec n}}}}}}
           {x 5}}
        {fac x}})
(define expr30
    '{rec {
           {longitud
            {fun {l} 
               {if {empty? l} 
                   0 
                   {+ 1 {longitud {tail l}}}}}}
          {suma
            {fun {l}
                {if {empty? l}
                   0
                   {+ {head l} {suma {tail l}}}}}}
          {lista {list 1 2 3}}}
       {+ {longitud lista} {suma lista}}})
(define expr31
    '{rec {
        {reversa
          {fun {l}
            {if {empty? l}
                l
                {append {reversa {tail l}} {list {head l}}}}}}
            {lista {list 1 2 3 4 5}}}
        {reversa lista}})
(define expr32
	'{with {{foo {fun {x y} {if {zero? y} {throws DivisionPorCero} {/ x y}}}}}
	      {foo 10 0}})
(define expr33
	'{rec {{potencias {fun {l} 
	           {if {empty? l} 
	               {throws ListaVacia} 
	               {append {list {pow {head l} 2}} {potencias {tail l}}}}}}}
	    {try/catch {{ListaVacia empty}}
	        {potencias {list 1 2 3}}}})
(define expr34
  '{newbox 1729})
(define expr35
  '{openbox {newbox 1729}})
(define expr36
  '{setbox {newbox 1729} 1835})
(define expr37
  '{with {{a {newbox 1729}}}
         {seqn {setbox a 1835}
               {setbox a 405}
               {openbox a}}}) 

; ; Pruebas para el ejercicio 8.3

(test/exn (get-value (interp (desugar (parse expr01)) (mtSub) (mtSto))) "Identificador libre")
(test (get-value (interp (desugar (parse expr02)) (mtSub) (mtSto))) (numV 1729))
(test (get-value (interp (desugar (parse expr03)) (mtSub) (mtSto))) (numV 12.46))
(test (get-value (interp (desugar (parse expr04)) (mtSub) (mtSto))) (numV 4235))
(test (get-value (interp (desugar (parse expr05)) (mtSub) (mtSto))) (numV 9))
(test (get-value (interp (desugar (parse expr06)) (mtSub) (mtSto))) (numV 2.23606797749979))
(test (get-value (interp (desugar (parse expr07)) (mtSub) (mtSto))) (numV 5))
(test (get-value (interp (desugar (parse expr08)) (mtSub) (mtSto))) 
    (exprV (op + (list (id 'a) (id 'a))) (aSub 'a 11 (mtSub))))
(test (get-value (interp (desugar (parse expr09)) (mtSub) (mtSto))) (numV 30))
(test (get-value (interp (desugar (parse expr10)) (mtSub) (mtSto)))
    (exprV (id 'b) (aSub 'b 19 (aSub 'a 18 (mtSub)))))
(test (get-value (interp (desugar (parse expr11)) (mtSub) (mtSto))) (boolV #f))
(test (get-value (interp (desugar (parse expr12)) (mtSub) (mtSto))) (boolV #t))
(test (get-value (interp (desugar (parse expr13)) (mtSub) (mtSto))) (boolV #f))
(test (get-value (interp (desugar (parse expr14)) (mtSub) (mtSto))) (closureV '(x) (id 'x) (mtSub)))
(test (get-value (interp (desugar (parse expr15)) (mtSub) (mtSto)))
    (closureV
     '(b h)
     (op / (list (op * (list (id 'b) (id 'h))) (num 2)))
     (mtSub)))
(test (get-value (interp (desugar (parse expr16)) (mtSub) (mtSto)))
    (closureV
     '(a b c)
     (op
      /
      (list
       (op
        +
        (list
         (op * (list (id 'b) (num -1)))
         (op
          sqrt
          (list
           (op
            -
            (list
             (op mexpt (list (id 'b) (num 2)))
             (op * (list (num 4) (id 'a) (id 'c)))))))))
       (op * (list (num 2) (id 'a)))))
     (mtSub)))
(test (get-value (interp (desugar (parse expr17)) (mtSub) (mtSto))) (exprV (num 1729) (mtSub)))
(test (get-value (interp (desugar (parse expr18)) (mtSub) (mtSto))) (numV 4))
(test (get-value (interp (desugar (parse expr19)) (mtSub) (mtSto))) (numV -2))
(test (get-value (interp (desugar (parse expr20)) (mtSub) (mtSto))) (numV 69))
(test (get-value (interp (desugar (parse expr21)) (mtSub) (mtSto))) (boolV #t))
(test (get-value (interp (desugar (parse expr22)) (mtSub) (mtSto))) (exprV (num 4) (mtSub)))
(test (get-value (interp (desugar (parse expr23)) (mtSub) (mtSto))) (boolV #f))
(test (get-value (interp (desugar (parse expr24)) (mtSub) (mtSto))) (numV 3))
(test (get-value (interp (desugar (parse expr25)) (mtSub) (mtSto))) (exprV (num 3) (mtSub)))
(test (get-value (interp (desugar (parse expr26)) (mtSub) (mtSto))) (listV '()))
(test (get-value (interp (desugar (parse expr27)) (mtSub) (mtSto))) (listV (list (numV 1) (numV 2) (numV 3) (numV 4) (numV 5))))
(test (get-value (interp (desugar (parse expr28)) (mtSub) (mtSto))) (listV (list (boolV #t) (boolV #f) (boolV #t))))
(test (get-value (interp (desugar (parse expr29)) (mtSub) (mtSto))) (numV 120))
(test (get-value (interp (desugar (parse expr30)) (mtSub) (mtSto))) (numV 9))
(test (get-value (interp (desugar (parse expr31)) (mtSub) (mtSto))) (listV (list (numV 5) (numV 4) (numV 3) (numV 2) (numV 1))))
(test/pred (get-value (interp (desugar (parse expr32)) (mtSub) (mtSto))) exceptionV?)
(test (get-value (interp (desugar (parse expr33)) (mtSub) (mtSto))) (listV (list (numV 1) (numV 4) (numV 9))))
(test (get-value (interp (desugar (parse expr34)) (mtSub) (mtSto))) (boxV 65))
(test (get-value (interp (desugar (parse expr35)) (mtSub) (mtSto))) (numV 1729))
(test (get-value (interp (desugar (parse expr36)) (mtSub) (mtSto))) (numV 1835))
(test (get-value (interp (desugar (parse expr37)) (mtSub) (mtSto))) (numV 405))