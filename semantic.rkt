#lang typed/racket


(provide type/infer)

(require "lang.rkt"
         "typ.rkt")

(: type/infer (-> expr typ))
(define type/infer
  (λ (exp)
    (match exp
      ([expr:int _] (typ:builtin "int"))
      ([expr:bool _] (typ:builtin "bool"))
      ([expr:string _] (typ:builtin "string")))))
