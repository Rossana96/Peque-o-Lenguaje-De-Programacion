;Autor Rossana Palma López
#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "desugar.rkt")
(require "interp.rkt")

;; Función encargada de ejecutar el intérprete para que el usuario interactúe con el lenguaje. Para
;; diferenciar el prompt de Racket del nuestro, usamos "(λ)". Aprovechamos los aspectos imperativos
;; del lenguaje para esta función.
;; ejecuta: void
(define (ejecuta)
    (begin
        (display "(λ) ")
        (define lexemas (read))
        (cond 
            [(equal? lexemas '{exit}) (display "")]
            [else
                (begin 
                    (with-handlers ([exn:fail? (lambda (exn) (displayln (exn-message exn)))])
                        (let ([resultado (interp (desugar (parse lexemas)) (mtSub))])
                            (match resultado
                                [(numV n) (displayln n)]
                                [(boolV #t) (displayln "true")]
                                [(boolV #f) (displayln "false")]
                                [(closureV _ _ _) (displayln "#<function>")]
                                [(exprV _ _) (displayln "#<promise>")]
                                [(listV _) (displayln (imprime resultado))]
                                [(boxV _) (displayln "#<box>")]
                                [(exceptionV id _) 
                                     (displayln (string-append "error: " (symbol->string id)))])))
                        (ejecuta))])))

(define (imprime expr)
    (match expr
        [(numV n) (number->string n)]
        [(boolV #t) "true"]
        [(boolV #f) "false"]
        [(closureV _ _ _) "#<function>"]
        [(exprV _ _) "#<promise>"]
        [(listV lst) 
            (let ([lista (map imprime lst)])
                (match lista
                    ['() "empty"]
                    [else (string-append "{list " (string-join lista) "}")]))]
        [(boxV _) ("#<box>")]))

;; Llamada a la función
(ejecuta)
