#lang typed/racket

(provide expr
         expr:int
         expr:bool
         expr:string
         expr:list
         expr:variable
         expr:lambda
         expr:application
         expr:let)

(struct expr [] #:transparent)
(struct expr:int expr [(v : Integer)] #:transparent)
(struct expr:bool expr [(v : Boolean)] #:transparent)
(struct expr:string expr [(v : String)] #:transparent)
(struct expr:list expr [(elems : (Listof expr))] #:transparent)
(struct expr:variable expr [(name : String)] #:transparent)
(struct expr:lambda expr
  [(param : (Listof String))
   (body : expr)]
  #:transparent)
(struct expr:application expr
  [(func : expr)
   (args : (Listof expr))]
  #:transparent)
(struct expr:let expr
  [(bindings : (Listof (Pair String expr)))
   (expr : expr)]
  #:transparent)

 
