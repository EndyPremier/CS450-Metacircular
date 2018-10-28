; MODIFIED xeval for data-driven
(define (xeval exp env)
  ; lookup action for special form checks
  (let ((action (lookup-action (type-of exp))))
    ; sift through the xeval
    (cond ((self-evaluating? exp) exp)
          ((variable? exp) (lookup-variable-value exp env))
          ; where the special-form lookups takes place
          ; if action exists, invoke it
          (action (action exp env))
          ((application? exp)
           (xapply (xeval (operator exp) env)
                   (list-of-values (operands exp) env) ))
          (else
           (error "Unknown expression type -- XEVAL " exp) ))))


; lookup-action λtype: lookup callback with given type
(define (lookup-action type)
  (let ((record (assoc type (cdr action-table))))
    (if record
        (cdr record)
        #f) ))

; type-of λexp: output symbol of type given expression
(define (type-of exp)
  (if (pair? exp)
      (car exp)
      #f))


;; CALLBACKS
; quote-callback for quote special form
(define (quote-callback exp env)
  (text-of-quoation exp) )
; lambda-callback for lambda special form
(define (lambda-callback exp env)
  (make-procedure (lambda-parameters exp) (lambda-body exp) env) )
; begin-callback for begin special form
(define (begin-callback exp env)
  (eval-sequence (begin-actions exp) env) )
; cond-callback for cond special form
(define (cond-callback exp env)
  (xeval (cond->if exp) env))

;; FAUXBACK
(define (eval-assignment) 'eval-assignment)
(define (eval-definition) 'eval-definition)
(define (eval-if) 'eval-if)

; action-table for outputting callbacks
(define action-table
  (list '*table*
        (cons 'quote  quote-callback)
        (cons 'set!   eval-assignment)
        (cons 'define eval-definition)
        (cons 'if     eval-if)
        (cons 'lambda lambda-callback)
        (cons 'begin  begin-callback)
        (cons 'cond   cond-callback) ))
