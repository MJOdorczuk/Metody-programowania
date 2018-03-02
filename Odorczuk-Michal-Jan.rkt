#lang racket

;;Continuous fraction approximation function
(define (cont-frac num den e)
  (define (approx a1 b1 a2 b2 k)
    (if (< (abs (- (/ a1 b1) (/ a2 b2))) e) (/ a1 b1)
        (approx (+ (* (den k) a1) (* (num k) a2)) (+ (* (den k) b1) (* (num k) b2)) a1 b1 (+ k 1))))
  (approx (num 1) (den 1) 0 1 2))

;;testing

(define (square a) (* a a))
(define (num-pi k) (square (- (* 2 k) 1)))
(define (den-pi k) 6.)

(+ (cont-frac num-pi den-pi 0.00001) 3) ;;3.1415972057118697 when it should be 3,14159

(define (num-atg x) (lambda (k)
                  (if (= k 1) x (* (- k 1) x x))))
(define (den-atg k) (- (* 2 k) 1))
(cont-frac (num-atg 0.) den-atg 0.00001) ;; 0.0 when it should be 0
(cont-frac (num-atg 1.) den-atg 0.00001) ;; 0.7711715664765247 when it should be 0.78540
(cont-frac (num-atg 2.) den-atg 0.00001) ;; 1.0300786002440991 when it should be 1.10715
(cont-frac (num-atg 3.) den-atg 0.00001) ;; 1.1164517961015095 when it should be 1.24905
(cont-frac (num-atg 50.) den-atg 0.000001) ;; 1.246562783887779 when it should be 1.550800