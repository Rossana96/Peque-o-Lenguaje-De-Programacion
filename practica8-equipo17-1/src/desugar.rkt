;Autor Rossana Palma López
#lang plai

(require "grammars.rkt")

;; Desendulzador.
;; Regresa un árbol de sintaxis abstracta sin azúcar sintáctica.
;; desugar: FWBAE -> FBAE
(define (desugar expr)
  (match expr
    [(idS i) (id i)]
    [(numS n) (num n)]
    [(boolS b) (bool b)]
    [(listS l) (lisT (map (λ (elem)
                            (desugar elem))
                          l))]
    [(recS bindings body)
     (rec (map (λ(lob)
                 (binding (bindingS-name lob)
                          (desugar (bindingS-value lob))))
               bindings)
       (desugar body))]
    [(opS f waes) (op f (map desugar waes))]
    [(ifS test-expr then-expr else-expr)
     (iF (desugar test-expr)
         (desugar then-expr)
         (desugar else-expr))]
    [(condS conditions)
     (if (equal? (length conditions) 2)
         (desugar (ifS (condition-test-expr (car conditions))
                       (condition-then-expr (car conditions))
                       (else-cond-else-expr (last conditions))))
         (desugar (ifS (condition-test-expr (car conditions))
                       (condition-then-expr (car conditions))
                       (condS (cdr conditions)))))]
    [(withS bindings body)
     (app (fun (map (lambda (lob)
                      (bindingS-name lob))
                    bindings)
               (desugar body))
          (map (lambda (lob)
                 (desugar (bindingS-value lob)))
               bindings))]
    [(withS* bindings body)
     (if (equal? (length bindings) 1)
         (desugar (withS bindings body))
         (desugar (withS (list (car bindings))
                         (withS* (cdr bindings) body))))]
    [(throwsS id)
     (throws id)]
    [(try/catchS bindings body)
     (try/catch (map (λ(lob)
                       (binding (bindingS-name lob)
                                (desugar (bindingS-value lob))))
                     bindings)
                (desugar body))]  
    [(funS params body) (fun params (desugar body))]
    [(appS fun-expr args)
     (app (desugar fun-expr) (map desugar args))]
    [(newboxS content) (newbox (desugar content))]
    [(openboxS box) (openbox (desugar box))]
    [(setboxS box content) (setbox (desugar box) (desugar content))]
    [(seqnS actions) (seqn (map desugar actions))]))
