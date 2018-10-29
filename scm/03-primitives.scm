; install-primitive-procedure λname λaction: install new primitives
(define (install-primitive-procedure name action)
  (cond ; name must be a symbol
        ((not (symbol? name))
          (error "Not a symbol: " name))
        ; action must be a lambda / procedure
        ((not (procedure? action))
          (error "Not a lambda: " action))
        ; action shouldn't already exists in list
        ((assoc name prim-table)
          (error "Action already exists: " name))
        ; if everything checks out
        (else
          ; add to list
          (set! prim-table (cons (cons name action) prim-table))
          ; output name
          name) ))


; prim-table for evaluating in scheme-base level (primitive-procedures)
(define prim-table
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?) ))

; prim-names (primitive-procedure-names)
(define prim-names (map car prim-table))
; prim-obj (primitive-procedure-objects)
(define prim-obj (map (lambda (proc) (list 'primitive (cadr proc)))
                      prim-table) )
