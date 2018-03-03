#lang racket

;; This procedure returns procedure equivalent to h(x) = f(g(x))
(define (compose f g)
  (lambda (x) (f (g x))))

;; This procedure composes precedure p n-times
(define (repeated p n)
    (if (= n 0) identity
        (compose p (repeated p (- n 1)))))

;; This procedure returns distance between numbers x and y
(define (dist x y)
  (abs (- x y)))

;; This procedure checks if x and y are closer than a given number
(define (close-enough? x y)
  (< (dist x y) 0.00001))

;; Procedure equivalent to one step of finding fix point algorythm
(define (fix-point f x0)
  (let ((x1 (f x0)))
    (if (close-enough? x0 x1)
        x0
        (fix-point f x1))))

;; Damping procedure
(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

;; This procedure returns nth power of a given x
(define (nth-pow x n)
  (if (= n 0) 1.
      (* x (nth-pow x (- n 1)))))

;; This procedure returns nth root of a given x with a-time damping
(define {nth-root-t x n a}
  {fix-point
   [(repeated average-damp a)
    (lambda {y} {/ x [nth-pow y (- n 1)]})] 1.})

;; Testing. All further lines should return 16
(nth-pow (nth-root-t 16 1 1) 1) ;; 15.999985694885254
(nth-pow (nth-root-t 16 2 1) 2) ;; 16.00000509354392
(nth-pow (nth-root-t 16 3 1) 3) ;; 16.000088130077543
(nth-pow (nth-root-t 16 4 2) 4) ;; For a=1 doesn't execute, 16.000054764532656 for a=2, all further don't execute for a=1
(nth-pow (nth-root-t 16 5 2) 5) ;; 16.000364075682125
(nth-pow (nth-root-t 16 7 2) 7) ;; 15.99966442126975
(nth-pow (nth-root-t 16 8 3) 8) ;; For a=2 doesn't execute, 16.000000060548544 for a=3, all further don't execute for a=2
(nth-pow (nth-root-t 16 15 3) 15) ;; 15.999029951565984
(nth-pow (nth-root-t 16 16 4) 16) ;; For a=3 doesn't execute, 16.000002492980588 for a=4, all further don't execute for a=3
(nth-pow (nth-root-t 16 31 4) 31) ;; 15.997690463793731
(nth-pow (nth-root-t 16 32 5) 32) ;; For a=4 doesn't execute, 16.003384130101807 for a=5, all further don't execute for a=4

;; From testing it can be stated that there is needed a-time damping, where a is the lowest integer so for n-th root 2^(a+1)>=n

;; This procedure returns the bth power of a
(define (pow a b)
  (if (= b 0) 1.
      (* a (pow a (- b 1)))))

;; This procedure returns the whole part of log2(a)
(define (log2 a)
  (define (iter i)
    (if (< (pow 2 i) a)
        (iter (+ i 1)) i))
  (iter 1))

;; Upgraded nth-root-t with self estimating a
(define {nth-root x n}
  {fix-point
   [(repeated average-damp {log2 n})
    (lambda {y} {/ x [nth-pow y (- n 1)]})] 1.})

2
(nth-pow (nth-root 16 1) 1) ;; 15.999985694885254
(nth-pow (nth-root 16 2) 2) ;; 16.00000509354392
(nth-pow (nth-root 16 3) 3) ;; 16.0000860404955
(nth-pow (nth-root 16 4) 4) ;; 16.000054764532656
(nth-pow (nth-root 16 5) 5) ;; 16.00072566420477
(nth-pow (nth-root 16 7) 7) ;; 16.000115611408035
(nth-pow (nth-root 16 8) 8) ;; 16.000000060548544
(nth-pow (nth-root 16 15) 15) ;; 16.000205050461492
(nth-pow (nth-root 16 16) 16) ;; 16.000002492980588
(nth-pow (nth-root 16 31) 31) ;; 16.000517348272854
(nth-pow (nth-root 16 32) 32) ;; 16.003384130101807
(nth-pow (nth-root 16 1000) 1000) ;; 16.00501054980072
;;(nth-root-t -5 1001 20) ;; And it doesn't work for roots of negatives