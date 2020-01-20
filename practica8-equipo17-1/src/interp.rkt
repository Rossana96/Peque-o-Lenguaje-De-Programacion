;Autor Rossana Palma López
#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "desugar.rkt")

;; Analizador semántico.
;; Interpreta el árbol de sintaxis abstracta generado por el desendulzador.
;; interp: FBAE -> FBAE-Value
(define (interp expr env store)
    (match expr
      [(id i) (lookup-sto (lookup i env) store)]
      [(num n) (v*s (numV n) store)]
      [(bool b) (v*s (boolV b) store)]
      [(lisT l) (v*s (listV (map (λ (elem)
                              (v*s-value (interp elem env store)))
                            l)) store)]
      [(op f waes)
       (cond
         [(or
           (equal? f +)
           (equal? f -)
           (equal? f *)
           (equal? f /)
           (equal? f mmodulo)
           (equal? f min)
           (equal? f max)
           (equal? f mexpt)
           (equal? f sqrt)
           (equal? f add1)
           (equal? f sub1))
          (let* ([ops (map (λ (arg)
                             (strict (interp arg env store) store))
                           waes)]
                 [ops-vals (map (λ (arg)
                                 (v*s-value arg))
                                ops)]
                 [ops-stos (v*s-store (car ops))]
                 [result (apply f (map (λ (x)
                                        (match x
                                          [(numV n) n]
                                          [(boolV b) b]
                                          [(listV elems) elems]))
                                      ops-vals))])
            (v*s (numV result) ops-stos))]
         [(or
           (equal? f <)
           (equal? f <=)
           (equal? f mequal?)
           (equal? f not-equal?)
           (equal? f >)
           (equal? f >=)
           (equal? f not)
           (equal? f mand)
           (equal? f mor)
           (equal? f zero?)
           (equal? f empty?))
          (let* ([ops (map (λ (arg)
                             (strict (interp arg env store) store))
                           waes)]
                 [ops-vals (map (λ (arg)
                                 (v*s-value arg))
                                ops)]
                 [ops-stos (v*s-store (car ops))]
                 [result (apply f (map (λ (x)
                                        (match x
                                          [(numV n) n]
                                          [(boolV b) b]
                                          [(listV elems) elems]))
                                      ops-vals))])
            (v*s (boolV result) ops-stos))]
         [(or
           (equal? f car)
           (equal? f cdr)
           (equal? f append))
          (let* ([ops (map (λ (arg)
                             (strict (interp arg env store) store))
                           waes)]
                 [ops-vals (map (λ (arg)
                                 (v*s-value arg))
                                ops)]
                 [ops-stos (v*s-store (car ops))]
                 [result (apply f ((λ (x)
                                        (match x
                                          [(numV n) n]
                                          [(boolV b) b]
                                          [(listV elems) elems]))
                                      ops-vals))])
            (v*s (listV result) ops-stos))])]
      [(iF test-expr then-expr else-expr)
       (let* ([condicion (interp test-expr env store)]
              [cond-val (v*s-value condicion)]
              [cond-sto (v*s-store condicion)])
         (if (boolV-b cond-val)
             (interp then-expr env cond-sto)
             (interp else-expr env cond-sto)))]
      [(rec bindings body)
       (let* ([locations (map (λ (lob)
                                (let ([location (next-location)])
                                  location))
                              bindings)]
              [new-env (foldl (λ (name loc new-env)
                        (aSub
                         name
                         loc
                         new-env))
                      env
                      (map (λ (lob)
                             (binding-name lob))
                           bindings)
                      locations)]
              [results (map (λ (value)
                              (interp value new-env store))
                            (map (λ (lob)
                             (binding-value lob))
                           bindings))]
              [res-vals (map (λ (result)
                               (v*s-value result))
                             results)]
              [res-stos (map (λ (result)
                               (v*s-store result))
                             results)]
              [new-store (foldl (λ (loc res-val res-sto)
                                  (cyclically-bind-and-interp loc res-val new-env res-sto))
                                (car res-stos)
                                locations
                                res-vals)])
         (interp body
                 new-env
                 new-store))]
       [(fun params body)
        (v*s (closureV params body env) store)]
       [(app fun-expr waes)
        (let* ([fun-res (strict (interp fun-expr env store) store)]
              [fun-val (v*s-value fun-res)]
              [fun-sto (v*s-store fun-res)]
              [waes-res (map (λ (arg)
                               (exprV arg env))
                             waes)]
              [arg-vals (map (λ (arg-res)
                              arg-res)
                             waes-res)]
              [arg-stos fun-sto]
              [locations (map (λ (loe)
                                (let ([location (next-location)])
                                  location))
                              waes)])
          (interp
           (closureV-body fun-val)
           (foldl
            (lambda (name location new-env)
              (aSub name
                    location
                    new-env))
            (closureV-env fun-val)
            (closureV-params fun-val)
            locations)
           (foldl (λ (loc arg-val arg-sto)
                    (aSto loc arg-val arg-sto))
                  arg-stos
                  locations
                  arg-vals)))]
      [(throws id)
       (exceptionV id (let/cc k k))]
      [(try/catch bindings body)
        (let ([expr-val (strict (interp body env store) store)])
          (if (and (exceptionV? expr-val)(equal? id (exceptionV-id expr-val)))
              (interp (binding-value (last bindings)) env store)
              expr-val))]
      [(newbox content)
       (let* ([location (next-location)]
              [result (interp content env store)]
              [box-value (v*s-value result)]
              [box-store (v*s-store result)])
         (v*s (boxV location) (aSto location box-value box-store)))]
      [(openbox box)
       (let* ([boxv (interp box env store)]
              [box-value (v*s-value boxv)]
              [box-store (v*s-store boxv)])
         (v*s (lookup-sto (boxV-index box-value) box-store) box-store))]
      [(setbox box content)
       (let* ([boxv (interp box env store)]
              [boxv-value (v*s-value boxv)]
              [boxv-store (v*s-store boxv)]
              [val (interp content env boxv-store)]
              [val-value (v*s-value val)]
              [val-store (v*s-store val)])
         (v*s val-value (aSto (boxV-index boxv-value) val-value val-store)))]
      [(seqn actions) (let* ([acts-vals (map (λ (action)
                                               (interp action env store))
                                             (cdr (reverse actions)))]
                             [acts-stos (v*s-store (car acts-vals))])
                        (interp (last actions) env acts-stos))]))

(define index -1)

(define (next-location)
    (begin
        (set! index (add1 index))
        index))

;; Busca el valor de un identificador en el ambiente.
;; Si el identificador no se encuentra, se genera el error "Identificador libre".
;; lookup: symbol Env -> integer
(define (lookup id env)
  (match env
    [(mtSub) (error 'interp "Identificador libre")]
    [(aSub name index rest-env) (if (symbol=? name id) index (lookup id rest-env))]))

;; Busca el valor de un registros en el heap.
;; Si el registro no se encuentra, se genera el error "Valor no almacenado".
;; lookup-sto: integer Store -> BERCFBAEL/L
(define (lookup-sto index store)
    (match store
      [(mtSto) (error 'interp "Valor no almacenado")]
      [(aSto sub-idx value rest-sto) (if (= index sub-idx) value (lookup-sto index rest-sto))]
      [(aRecSto sub-idx value rest-sto) (if (symbol=? sub-idx) (unbox value) (lookup index rest-sto))]))

;; Fuerza la evaluación de un punto estricto.
;; strict: CFBAE/L-Value -> CFBAE/L-Value
(define (strict expr store)
    (match expr
        [(exprV expr env) (strict (interp expr env store) store)]
        [else expr]))

(define (cyclically-bind-and-interp loc value env store)
  (let* ([value-holder (box (numV 1729))]
         [new-sto (aRecSto loc value-holder store)]
         [named-expr-value (interp value env new-sto)])
    (begin
      (set-box! value-holder named-expr-value)
      new-sto)))


