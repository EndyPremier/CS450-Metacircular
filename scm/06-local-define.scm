; defined? λsymbol: check if symbol is defined in enviornment
; -> def?-callback λexp λenv: s450 interface
(define (def?-callback exp env)
  ; WIP OUTPUT
  exp)

; locally-defined? λsymbol: check if symbol is defined in current frame
; -> frame-def?-callback λexp λenv: s450 interface
(define (frame-def?-callback exp env)
  ; WIP OUTPUT
  exp)

; make-unbound! λsymbol: remove symbol binding from current chain of frame
; -> unb!-callback λexp λenv: s450 interface
(define (unb!-callback exp env)
  ; WIP OUTPUT
  exp)

; locally-make-unbound! λsymbol: remove symbol binding from current frame
; -> frame-unb!-callback λexp λenv: s450 interface
(define (frame-unb!-callback exp env)
  ; WIP OUTPUT
  exp)

