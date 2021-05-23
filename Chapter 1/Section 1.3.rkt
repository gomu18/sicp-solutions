#lang sicp

; Problem 1.29
; For Simpson Integral with n = 100, we get 0.23078806666666699 compared to .24998750000000042 for plain integral.
; For Simpson Integral with n = 1000, we get 0.24800798800666748 compared to .249999875000001 for plain integral.
; Plain integral seems more accurate than Simpson Integral.
; There was a cleaner solution of using k [0, n] as the term value and calculate each term accordingly.
; Since it's available in other sites, not reimplementing that.
(define (simpson-integral f a b n)
  (define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b))))
  (define h (/ (- b a) n))
  (define (odd-term term)
    (* 4.0 (f term)))
  (define (even-term term)
    (* 2.0 (f term)))
  (define (add-h term)
    (+ term (* 2.0 h)))
  (/ (* (+ (f a)
           (f b)
           (sum odd-term (+ a h) add-h (+ a (* h (- n 1.0))))
           (sum even-term (+ a (* 2.0 h)) add-h (+ a (* h (- n 2.0)))))
        h)
     3.0))

; Problem 1.30
(define (iter-sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

; Problem 1.31
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1.0))

(define (product-recursive term a next b)
  (if (> a b)
      1.0
      (* (term a)
         (product-recursive term (next a) next b))))

(define (factorial n)
  (define  (identity x) x)
  (define (inc x) (+ x 1))
  (product-recursive identity 1 inc n))

(define (pi-approx n)
  (define (pi-term i)
    (/ (- (+ i 2.0) (remainder i 2.0))
       (+ i 1.0 (remainder i 2.0))))
  (define (inc x) (+ x 1.0))
  (product pi-term 1.0 inc n))

; Problem 1.32
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (accumulate-recursive combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-recursive combiner null-value term (next a) next b))))

(define (accumulate-sum term a next b)
  (accumulate-recursive + 0 term a next b))

(define (accumulate-prod term a next b)
  (accumulate * 1 term a next b))

; Problem 1.33
(define (filtered-accumulate combiner null-value term a next b predicate)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (if (predicate a)
                           (combiner result (term a))
                           result))))
  (iter a null-value))

; subdivision a
(define (prime-sum a b)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (define (square x) (* x x))
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (prime? n)
    (and (> n 1) (= n (smallest-divisor n))))
  (filtered-accumulate + 0 identity a inc b prime?))

; subdivision b
(define (coprime-prod n)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
  (define (coprime? i)
    (= (gcd i n) 1))
  (filtered-accumulate * 1 identity 1 inc n coprime?))

; Problem 1.34
; When we keep on substituting we will reach a stage (2 2), and since 2 isn't an operator, we will display the error about the same.

; Problem 1.35
; Rearranging the equation x^2 = x + 1 gives us the function, x = 1 + (1/x) . Hence proved.
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (chi-calculator)
  (fixed-point (lambda (x) (+ 1.0 (/ 1.0 x))) 1.0))

; Problem 1.36
; Without average damping, and initial guess of 2, we take 35 steps.
; With average damping, and initial guess of 2, we take 10 steps. A 67% reduction, mindblowing.
(define (fixed-point-stepper f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))      
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))  
  (display first-guess)
  (newline)
  (try first-guess))

(define (log-solution-finder)
  (define (average x y) (/ (+ x y) 2))
  (fixed-point-stepper (lambda (x) (average x (/ (log 1000.0) (log x)))) 2.0))

; Problem 1.37
; It takes 11 for k, to get value accurate to 4 decimals

;a
(define (cont-frac n d k)
  (define (cont-frac-helper i)
    (if (> i k)
        0.0
        (/ (n i) (+ (d i) (cont-frac-helper (+ i 1))))))
  (cont-frac-helper 1))

;b
(define (cont-frac-iter n d k)
  (define (cont-frac-helper k result)
    (if (= k 0)
        result
        (cont-frac-helper (- k 1) (/ (n k) (+ (d k) result)))))
  (cont-frac-helper k 0))

; Problem 1.38
(define (e-two)
  (cont-frac-iter (lambda (i) 1.0)
                  (lambda (i) (if (= 2 (remainder i 3))                                  
                                  (* 2.0 (/ (+ i 1) 3.0))
                                  1.0))
                  10))

; Problem 1.39
(define (tan-cf x k)
  (cont-frac-iter (lambda (i) (if (= i 1)
                                  x
                                  (- (* x x))))
                  (lambda (i) (- (* i 2) 1))
                  k))

; Problem 1.40
(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (* x x x)
                 (* a x x)
                 (* b x)
                 c)))

; Problem 1.41
; (((double (double double)) inc) 5) returns 16
(define (double f)
  (lambda (x) (f (f x))))

; Problem 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))

; Problem 1.43
(define (repeated f n)
    (if (= n 1)
        f
        (compose f (repeated f (- n 1)))))

; Problem 1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx))) 3)))

(define (n-fold-smooth n)
  (lambda (x) (repeated smooth n)))

; Problem 1.45
; Number of times average damping is to be applied is floor(log2(n))
(define (nth-root x n)
  (define (pow x n)
    (if (= n 0)
        1.0
        (* x (pow x (- n 1)))))
  (define (average x y)
    (/ (+ x y) 2.0))
  (define (average-damp f)
    (lambda (x) (average x (f x))))
  (fixed-point ((repeated average-damp (floor (log n 2))) (lambda (y) (/ x (pow y (- n 1))))) 1.0))

; Problem 1.46
; I had to look at another solution to get the insight. I was stuck on the angle of trying to recursively call a lambda function.
; Whereas the solution lied in the insight that, iterative-improve itself returns the function I was looking for.
; Also the part of number of arguments for good-enough? and improve were not straightforward to reason about.
(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve)
         (improve guess)))))

(define (sqrt x)
  (define (is-sqrt? guess)
    (< (abs (- x (* guess guess))) 0.00001))
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve-sqrt guess)
    (average guess (/ x guess)))
  ((iterative-improve is-sqrt? improve-sqrt) 1.0))

(define (generic-fixed-point f)
  (define (fixed-point-check? guess)
    (< (abs (- (f guess) guess)) 0.000001))
  (define (fixed-point-improve guess)
    (f guess))
  ((iterative-improve fixed-point-check? fixed-point-improve) 1.0))

