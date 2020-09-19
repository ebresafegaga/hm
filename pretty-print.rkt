#lang typed/racket


(require "typ.rkt")

(provide pretty-print-typ)

(: pretty-print-typ (-> typ String))
(define pretty-print-typ
  (λ (t)
    (match t
      ([typ:freevar idx subst]
       (if subst 
           (pretty-print-typ subst)
           (format "?~a" idx)))
      ([typ:constructor name typ-args]
       (if (empty? typ-args)
           (format "~a" name)
           (let ([j (string-join (map
                                  (λ (typ-arg) (pretty-print-typ typ-arg))
                                  typ-args) " ")])
             (if (string=? name "pair")
                 (format "(~a)" j)
                 (format "(~a ~a)" name j)))))
      ([typ:arrow from to]
       (format "(~a -> ~a)" (pretty-print-typ from)
               (pretty-print-typ to))))))

















