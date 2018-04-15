#lang racket

;; expressions

(define (const? t)
  (number? t))

(define (op? t)
  (and (list? t)
       (member (car t) '(+ - * /))))

(define (op-op e)
  (car e))

(define (op-args e)
  (cdr e))

(define (op-arg-left e)
  (first (op-args e)))

(define (op-arg-right e)
  (second (op-args e)))

(define (op-cons op args)
  (cons op args))

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]))

(define (let-def? t)
  (and (list? t)
       (= (length t) 2)
       (symbol? (car t))))

(define (let-def-var e)
  (car e))

(define (let-def-expr e)
  (cadr e))

(define (let-def-cons x e)
  (list x e))

(define (let? t)
  (and (list? t)
       (= (length t) 3)
       (eq? (car t) 'let)
       (let-def? (cadr t))))

(define (let-def e)
  (cadr e))

(define (let-expr e)
  (caddr e))

(define (let-cons def e)
  (list 'let def e))

(define (var? t)
  (symbol? t))

(define (var-var e)
  e)

(define (var-cons x)
  x)

(define (arith/let-expr? t)
  (or (const? t)
      (and (op? t)
           (andmap arith/let-expr? (op-args t)))
      (and (let? t)
           (arith/let-expr? (let-expr t))
           (arith/let-expr? (let-def-expr (let-def t))))
      (var? t)))

;; let-lifted expressions

(define (arith-expr? t)
  (or (const? t)
      (and (op? t)
           (andmap arith-expr? (op-args t)))
      (var? t)))

(define (let-lifted-expr? t)
  (or (and (let? t)
           (let-lifted-expr? (let-expr t))
           (arith-expr? (let-def-expr (let-def t))))
      (arith-expr? t)))

;; generating a symbol using a counter

(define (number->symbol i)
  (string->symbol (string-append "x" (number->string i))))

;; environments (could be useful for something)

(define empty-env
  null)

(define (add-to-env x v env)
  (cons (list x v) env))

(define (merge-env e1 e2)
  (if (eq? e1 '()) e2 (merge-env (cdr e1) (cons (car e1) e2))))

(define (add-to-tenv x nx tenv)
  (cons (list x nx) tenv))

(define (find-in-env x env)
  (cond [(null? env) (error "undefined variable" x)]
        [(eq? x (caar env)) (cadar env)]
        [else (find-in-env x (cdr env))]))

(define (find-in-tenv x tenv)
  (cond [(null? tenv) (error "undefinded variable" x)]
        [(eq? x (caar tenv)) (cadar tenv)]
        [else (find-in-tenv x (cdr tenv))]))

;; the let-lift procedure

(define (let-lift e)
  (let ((nvne (first (new-var-names e null 0))))
    (define (reloc-let e env)
      (if (< (length env) 1) e
          (reloc-let
          (let-cons
           (let-def-cons (caar env) (cadar env))
           e)
          (cdr env))))
    (define (nvn-let-lift e)
      (cond [(const? e) (list e '())]
            [(op? e)
             (let ((retl (nvn-let-lift (op-arg-left e)))
                   (retr (nvn-let-lift (op-arg-right e))))
               (list (op-cons (op-op e) (list (first retl) (first retr)))
                     (merge-env (second retl) (second retr))))]
            [(let? e)
             (let ((retd (nvn-let-lift (let-def-expr (let-def e)))))
               (let ((rete (nvn-let-lift (let-expr e))))
                 (list (first rete)
                       (add-to-env (let-def-var (let-def e)) (reloc-let (first retd) (second retd)) (second rete)))))]
            [(var? e) (list e '())]))
    (let ((nvnll (nvn-let-lift nvne)))
      (reloc-let (first nvnll) (second nvnll)))))

(define (new-var-names e tenv c)
  (cond [(const? e) (list e c)]
        [(op? e)
         (let ((retl (new-var-names (op-arg-left e) tenv c)))
           (let ((retr (new-var-names (op-arg-right e) tenv (second retl))))
             (list (op-cons (op-op e) (list (first retl) (first retr))) (second retr))))]
        [(let? e)
         (let ((nv (number->symbol c)))
           (let ((retd (new-var-names
                        (let-def-expr (let-def e))
                        tenv
                        (+ 1 c))))
           (let ((rete (new-var-names
                        (let-expr e)
                        (add-to-tenv (let-def-var (let-def e)) nv tenv)
                        (second retd))))
             (list (let-cons
                    (let-def-cons nv (first retd))
                    (first rete))
                   (second rete)))))]
        [(var? e) (list (var-cons (find-in-tenv (var-var e) tenv)) c)]
        )
  )

(define t1
  '(+ 5 (* 7 (let (x (let (x 5) x)) (let (x 3) x)))))


(let-lift t1)