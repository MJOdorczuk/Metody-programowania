#lang racket

(define (cube-root val)
  (define margin 0.00001)
  (define (cube a) (* a a a))
  (define (approx y) (/ (+ (/ val (* y y)) (* 2. y)) 3.))
  (define (good-enough? app)
    (< (abs (- val (cube app))) margin))
  (define (loop app)
    (if (good-enough? app) app (loop (approx app))))
   (loop 1.))

(cube-root 0) ;;0.017341529915832616 when it should be 0.
(cube-root 1) ;;1.0 and it is perfect
(cube-root 8) ;;2.000000000012062 when it should be 2.
(cube-root 125000) ;;50.0 and it is perfect
(cube-root 17) ;;2.571281590696762 when it shoul be 2.57128159066