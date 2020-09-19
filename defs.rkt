#lang racket

(require (for-syntax syntax/parse))

(define/contract (plus a b)
  (number? number? . -> . number?)
  (+ a b))







