; defined? λsymbol: check if symbol is defined in enviornment
; -> def?-callback λexp λenv: s450 interface
(define (def?-callback exp env)
  ; sift through the var-list and environments
  (define (def-check var-list curr-var base-env)
    (cond ; if reached the-empty-environment, return false
          ((null? base-env) #f)
          ; if all var checked in current frame, go down one environment
          ((null? var-list)
            (def-check (car (first-frame base-env))
                       curr-var
                       (enclosing-environment base-env) ))
          ; still var in var-list, check if current entry is true, return true
          ((eq? (car var-list) curr-var) #t)
          ; not same, go down one entry in list
          (else (cdr var-list) curr-var base-env) ))
  ; main procedure
  (if (null? (cdr exp))
      (error "No arguments exists! -- DEFINED?")
      (def-check '() (cadr exp) env) ))

; locally-defined? λsymbol: check if symbol is defined in current frame
; -> frame-def?-callback λexp λenv: s450 interface
(define (frame-def?-callback exp env)
  ; sift through the var-list and environments
  (define (def-check var-list curr-var)
    (cond ; if all var checked in current frame, return false
          ((null? var-list) #f)
          ; still var in var-list, check if current entry is true, return true
          ((eq? (car var-list) curr-var) #t)
          ; not same, go down one entry in list
          (else (cdr var-list) curr-var) ))
  ; main procedure
  (if (null? (cdr exp))
      (error "No arguments exists! -- LOCALLY-DEFINED?")
      (def-check (car (first-frame base-env)) (cadr exp)) ))

; make-unbound! λsymbol: remove symbol binding from current chain of frame
; -> unb!-callback λexp λenv: s450 interface
(define (unb!-callback exp env)
  ; sift through the table and environments
  (define (unb! curr-env val)
    (cond ((null? curr-env) val)
          (else (set-car! curr-env (delete-in-frame! (car curr-env) val))
                (unb! (cdr curr-env)) )))
  ; main procedure
  (if (null? (cdr exp))
      (error "No arguments exists! -- MAKE-UNBOUND!")
      (unb! env (cadr exp)) ))

; locally-make-unbound! λsymbol: remove symbol binding from current frame
; -> frame-unb!-callback λexp λenv: s450 interface
(define (frame-unb!-callback exp env)
  (if (null? (cdr exp))
      (error "No arguments exists! -- MAKE-UNBOUND!")
      (set-car! curr-env (delete-in-frame! (car curr-env) val)) ))


; helper procedures
(define (delete-in-frame! frame var)
  ; re-frame-iter
  (define (re-frame-iter! in-var in-val out-var out-val)
    (cond ((null? in-var) (cons out-var out-val))
          ((equal? (car in-var) var)
            (cons (append out-var (cdr in-var))
                  (append out-val (cdr in-val)) ))
          (else (re-frame-iter! (cdr in-var) (cdr in-val)
                                (append out-var (list (car in-var)))
                                (append out-val (list (car in-val))) ))))
  ; main procedure
  (re-frame-iter! (car frame) (cdr frame) '() '()) )
