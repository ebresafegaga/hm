#lang typed/racket

(provide typ typ:builtin typ:freevar typ:constructor typ:arrow subst!)

(struct typ [] #:transparent)
(struct typ:builtin typ
  [(name : String)]
  #:transparent)
(struct typ:freevar typ
  [(index : Integer)
   (substituted : (Option typ))]
  #:transparent
  #:mutable)
(struct typ:constructor typ
  [(name : String)
   (arg : (Listof typ))]
  #:transparent)

(struct typ:arrow typ
  [(from : typ)
   (to : typ)]
  #:transparent)

(: subst! (-> typ:freevar typ Void))
(define subst!
  (λ (fv s)
    (let ([ns (match s
                ([typ:freevar _ s] s)
                (s s))])
      (set-typ:freevar-substituted! fv ns))))
