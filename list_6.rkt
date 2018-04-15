#lang racket

(define (const? t)
  (number? t))

(define (binop? t)
  (and (list? t)
       (= (length t) 3)
       (member (car t) '(+ - * /))))

(define (binop-op e)
  (car e))

(define (binop-left e)
  (cadr e))

(define (binop-right e)
  (caddr e))

(define (binop-cons op l r)
  (list op l r))

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

(define (hole? t)
  (eq? t 'hole))

(define (arith/let/holes? t)
  (or (hole? t)
      (const? t)
      (and (binop? t)
           (arith/let/holes? (binop-left  t))
           (arith/let/holes? (binop-right t)))
      (and (let? t)
           (arith/let/holes? (let-expr t))
           (arith/let/holes? (let-def-expr (let-def t))))
      (var? t)))

(define (num-of-holes t)
  (cond [(hole? t) 1]
        [(const? t) 0]
        [(binop? t)
         (+ (num-of-holes (binop-left  t))
            (num-of-holes (binop-right t)))]
        [(let? t)
         (+ (num-of-holes (let-expr t))
            (num-of-holes (let-def-expr (let-def t))))]
        [(var? t) 0]))

(define (arith/let/hole-expr? t)
  (and (arith/let/holes? t)
       (= (num-of-holes t) 1)))

(define (merge-no-dupls e l)
  (cond [(null? l) (list e)]
        [(eq? e (car l)) l]
        [else (cons (car l)
                    (merge-no-dupls e (cdr l)))]
        )
  )

(define (hole-context e)
  (define (dig-the-holes t vars)
    (cond [(hole? t) (list vars)]
          [(const? t) '()]
          [(binop? t)
           (append (dig-the-holes (binop-left t) vars)
                   (dig-the-holes (binop-right t) vars)
                   )
           ]
          [(let? t)
           (append (dig-the-holes (let-def-expr (let-def t)) vars)
            (dig-the-holes
                    (let-expr t)
                              (merge-no-dupls (let-def-var (let-def t)) vars)
                    )
                   )
           ]
          [(var? t) '()]
          )
    )
  (dig-the-holes e null)
  )

; TESTING PART

(define t1
  '(let (piesek 1)
     (let (kotek (+ 3 hole))
       (let (chomik 5)
         (+ 5
            (let (agama (+ 2 hole))
              hole))))))

(define t2 '(+ 3 hole ))

(define t3 '(let (x 3) ( let (y 7) (+ x hole ))))

(define t4 '( let (x 3) ( let (y hole ) (+ x 3))))

(define t5 '( let (x hole ) ( let (y 7) (+ x 3))))

(define t6 '( let ( piesek 1)
               ( let ( kotek 7)
                  ( let ( piesek 9)
                     ( let ( chomik 5)
                        hole )
                     )
                  )
               )
  )

(define t7 '(+ ( let (x 4) 5) hole ))

(define (equal-context? xs ys)
  (define (equivalent? x y)
    (and (member (car x) y)
         (if (equal? (cdr x) '())
             true
             (equivalent? (cdr x) y)
             )
         )
    )
  (and (equivalent? xs ys)
       (equivalent? ys xs)
       )
  )

(define (test)
  (and (equal-context? (hole-context t1)
            '((piesek) (piesek kotek chomik) (piesek kotek chomik agama)))
       (equal-context? (hole-context t2)
            '(()))
       (equal-context? (hole-context t3)
            '((x y)))
       (equal-context? (hole-context t4)
            '((x)))
       (equal-context? (hole-context t6)
            '((piesek kotek chomik)))
       (equal-context? (hole-context t5)
            '(()))
       )
  )

;; END OF TESTING PART