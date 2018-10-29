; install-special-form λname λaction: install new special forms
(define (install-special-form name action)
  (cond ; name must be a symbol
        ((not (symbol? name))
          (error "Not a symbol: " name))
        ; action must be a lambda / procedure
        ((not (procedure? action))
          (error "Not a lambda: " action))
        ; action shouldn't already exists in list
        ((lookup-action name)
          (error "Action already exists: " name))
        ; everything checks out
        (else
          ; add to list
          (set! action-table
                (list '*table*
                      (cons name action)
                      (cdr action-table) ))
          ; output name
          name) ))

;; older version
;(define (install-special-form name action)
;  (if (symbol? name)  ; must be a symbol
;      (if (not (lookup-action name))  ; must not already be in the table
;          (set! action-table
;                (list '*table*
;                      (cons name action)))
;          (error "Already exists: " name))
;      (error "Not a symbol: " name) ))
