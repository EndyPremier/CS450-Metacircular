; RAW LIGHTWEIGHT xeval version
(define (xeval exp env)
  ; lookup action
  (let ((action (lookup-action (type-of exp))))
    (if action
        (action exp env)
        (error "Unknown expression type -- XEVAL " exp) )))
