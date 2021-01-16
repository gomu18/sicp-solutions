#lang sicp
(#%require sicp-pict)

;Problem 1.1
;10
;12
;8
;3
;6
;define statement
;define statement
;19
;#f
;4
;16
;6
;16

;Problem 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;Problem 1.3 - sum of squares of 2 greatest numbers from input of 3 numbers
(define (sum-of-2-greatest-squares first second third)
  (define (square number) (* number number))
  (cond ((and (< first second) (< first third)) (+ (square second) (square third)))
        ((and (< second first) (< second third)) (+ (square first) (square third)))
        ((and (< third first) (< third second)) (+ (square first) (square second)))
        (else (+ (square first) (square second)))))

;Problem 1.4
;It adds the absolute value of 'b' with 'a'.

;Problem 1.5
;(p) is recursively defined in terms of itself without a base condition.
;In applicative order of evaluation, we will substitute the operand (p) infinitely and never end.
;In normal order of evaluation, we will expand till primitive terms and then evaluate its equivalent to 0 and print 0.
;In this way the order of evaluation gives different results.

;Problem 1.6
;new-if is an expression and not a special form. So, in applicative order of evaluation,
;we will try to recursively evaluate sqrt-iter and never terminate

;Problem 1.7
;0.0001 The lowest number in which sqrt-iter with tolerance 0.001 breaks
;10^21 is where the program gets too slow to compute the values.
(define (sqrt x)
  (define (square number) (* number number))
  (define (average x y) (/ (+ x y) 2))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (good-enough? guess x)
    (< (abs (- (improve guess x) guess)) 0.001))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

;Problem 1.8
(define (cube-root x)
  (define (cube number) (* number number number))
  (define (average x y) (/ (+ x y) 2))
  (define (improve guess x)
    (average guess (/ (+ (/ x (* guess guess)) (* 2 guess)) 3)))
  (define (good-enough? guess x)
    (< (abs (- (cube guess) x)) 0.001))
  (define (cube-root-iter guess x)
    (if (good-enough? guess x)
        guess
        (cube-root-iter (improve guess x) x)))
  (cube-root-iter 1.0 x))
