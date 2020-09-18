#lang typed/racket


(provide type/infer)

(require "lang.rkt"
         "typ.rkt")

(struct Env
  [(parent : (Option Env))
   (type-env : (Mutable-HashTable String typ))]
  #:transparent
  #:mutable)

(: Env/new (->* () ((Option Env)) Env))
(define (Env/new [parent #f])
  (Env parent (make-hash '())))

(: Env/lookup (-> Env String typ))
(define Env/lookup
  (λ (env name)
    (: lookup-parent (-> typ))
    (define lookup-parent (λ ()
                            (: parent (Option Env))
                            (define parent (Env-parent env))
                            (if parent
                                (Env/lookup parent name)
                                (raise (format "no variable named: `~a`" name))))))
  (let ([typ-env : (Mutable-HashTable String typ) (Env-type-env env)])
    (hash-ref typ-env name
              lookup-parent)))

(: Env/bind-var (-> Env String typ Void))
(define (Env/bind/var env name typ)
  (let ([env (Env-typ-env)])
    (if (hash-has-key? env name)
        (raise (format "redefined: `~a`" name))
        (hash-set! env name typ))))

(struct Context
  [(freevar-counter : Integer)
   (type-env : Env)]
  #:transparent
  #:mutable)

(: Context/new (-> Context))
(define Context/new
  (λ ()
    (Context 0 (Env/new))))

(: Context/new-freevar! (-> Context typ))
(define Context/new-freevar!
  (λ (ctx)
    (let ([cur-count (Context-freevar-counter ctx)])
      (set-Context-freevar-counter! ctx (+ 1 (Context-freevar-counter ctx)))
      (typ:freevar cur-count #f))))

(: occurs? (-> typ typ Boolean))
(define occurs
  (λ (v t)
    (match (cons v t)
      ([cons v (typ:freevar _ _)] (eqv? v t))
      ([cons v (typ:arrow t1 t2)] (or (occurs? v t1) (occurs v t2)))
      ([cons v (typ:contructor _ type-params)]
       (foldl (λ ([t : typ] [pre-bool : Boolean])
                (or pre-bool (occurs? v t)))
              #f
              type-params))
      (_ false))))

(: unify (-> typ typ Void))
(define unify
  (λ (t1 t2)
    (match (cons t1 t2)
      ([cons (typ:constructor a a1) (typ:contructor b b1)]
       #:when (string=? a b)
       (for-each (λ ((ae: typ) (be: typ))
                   (unify ae be))
                 a1 b1))
      ([cons (typ:arrow p1 r1) (typ:arrow p2 r2)]
       (unify p1 p2)
       (unify r1 r2))
      ((and
        [cons _ (typ:freevar _ _)]
        [cons t v])
       (if (or (eqv? v t) (not (occurs? v t)))
           (subst! v t)
           (void))
       (void))
      ([cons (typ:freevar _ _) t2] (unify t2 t1))
      (_ (raise (format "cannot unify type ~a and ~a" (pretty-print-typ t1) (pretty-print-typ t2)))))))

(: type/infer (->* (expr) (Context) typ))
(define type/infer
  (λ (exp ctx)
    (match exp
      ([expr:variable name] (Env/lookup (Context-type-env ctx) name))
      ([expr:int _] (typ:builtin "int"))
      ([expr:bool _] (typ:builtin "bool"))
      ([expr:string _] (typ:builtin "string"))
      ([expr:list elems]
       (typ:contructor "list"
                       (list (if (empty? elems)
                                 (Context/new-freevar ctx)
                                 (let ([elem-typ (type/infer (car elems))])
                                   (for-each (λ ([elem: expr]) (unify elem-type (type/infer expr)))
                                             (cdr elems))
                                   elem-typ)))))
      ([expr:lambda params body]
       (letrec [λ-env: Env (Env/new (Context-type-env ctx))]
         [params-type (typ:constructor
                       "pair"
                       (map (λ ([param-name: String])
                              (let ([r (Context/new-freevar! ctx)])
                                (Env/bind-var λ-env param-name r)
                                r))
                            (params)))])
       (set-Context-type-env! ctx λ-env)
       (define body-typ (type/infer body ctx))
       (typ:arrow param-types body-typ))
      ([expr:let bindings exp]
       (letrec ([let-env : Env (Env/new (Context-type-env ctx))]
                [bind-to-context (λ ([bind : (Pairof String expr)])
                                   (match bind
                                     ([cons name init]
                                      (Env/bind-var let-env name (type/infer init ctx)))))])
         (map bind-to-context bindings)
         (set-Context-type-env! ctx let-env)
         (type/infer exp ctx)))
      ([expr:application fn args]
       (let ([fn-typ (type/infer fn ctx)]
             [args-typ (map (λ ((arg: expr)) (type/infer arg ctx)) args)]
             [fresh (Context/new-freevar ctx)])
         (unify fn-typ (typ:arrow (typ:constructor "pair" args-typ) fresh))
         fresh)))))
















