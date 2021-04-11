#lang sicp

;Problem 1.9
;Since + is being defined in terms of another operation, the primitive operation here are inc and dec
;(+ 4 5)
;(inc (+ 3 5))
;(inc (inc (+ 2 5)))
;(inc (inc (inc (+ 1 5))))
;(inc (inc (inc (inc (+ 0 5)))))
;(inc (inc (inc (inc 5))))
;(inc (inc (inc 6)))
;(inc (inc 7))
;(inc 8)
;9
;The first definition is a recursive process, since interpreter has to keep track of how many times inc has to be applied.
;
;(+ 4 5)
;(+ 3 6)
;(+ 2 7)
;(+ 1 8)
;(+ 0 9)
;9
;The second definition is an iterative process, since at each step the variables 'a' and 'b' hold the sum to be computed.

;Problem 1.10
;1024
;65536
;65536
;
;f(n) - Returns 2 * n
;g(n) - Returns 2 ** n, except for n = 0, when it returns 0
;h(n) - Returns 2 ** 2 .... 2 ** 2 (number of 2's is n), except for n = 0, when it returns 0

;Problem 1.11
(define (f-recursive n)
  (cond ((< n 3) n)
        (else (+ (f-recursive (- n 1)) (* 2 (f-recursive (- n 2))) (* 3 (f-recursive (- n 3)))))
  )
)

(define (f-iterative fn fn-1 fn-2 n iter)
  (cond ((< n 3) n)
        ((= iter n) fn)
        (else (f-iterative (+ fn (* 2 fn-1) (* 3 fn-2)) fn fn-1 n (+ iter 1)))))

; Sample invocation for value n (f-iterative 2 1 0 n 2)

;Problem 1.12
;Assume triangle is aligned at left hand side, and numbering starts from 1 for both row and column.
;For invalid values, returning 0.
(define (pascal-triangle-value row-number column-number)
  (cond
    ((or (< row-number 1) (< column-number 1) (> column-number row-number)) 0)
    ((or (= row-number 1) (= column-number 1) (= row-number column-number)) 1)    
    (else (+ (pascal-triangle-value (- row-number 1) column-number) (pascal-triangle-value (- row-number 1) (- column-number 1))))
  )
)

;Problem 1.13
;Since proof by induction is hard to write in non-LaTeX environment, skipping solution for this question.
;They are solutions of the characteristic equation and thus are easy to write and reason about.

;Problem 1.14 - Representing tree is hard, so they have been left aligned.
;(count-change 11)
;(cc 11 5)
;(+ (cc 11 4) (cc -39 5))
;(+ (+ (cc 11 3) (cc -14 4)) 0)
;(+ (+ (+ (cc 11 2) (cc 1 3)) 0) 0)
;(+ (+ (+ (+ (cc 11 1) (cc 6 2)) (+ (cc 1 2) (cc -9 3)))))
;(+ (+ (+ (+ (+ (cc 11 0) (cc 10 1)) (+ (cc 6 1) (cc 1 2))) (+ (+ (cc 1 1) (cc -4 2)) 0))))
;(+ (+ (+ (+ (+ 0 (+ (cc 10 0) (cc 9 1))) (+ (+ (cc 6 0) (cc 5 1)) (+ (cc 1 1) (cc -4 2)))) (+ (+ (+ (cc 1 0) (cc 0 1)) 0) 0))))
;(+ (+ (+ (+ (+ 0 (+ 0 (+ (cc 9 0) (cc 8 1)))) (+ (+ 0 (+ (cc 4 1) (cc 5 0))) (+ (+ (cc 0 1) (cc 1 0)) 0))) (+ (+ (+ 0 1) 0) 0))))
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ (cc 8 0) (cc 7 1))))) (+ (+ 0 (+ (+ (cc 4 0) (cc 3 1)) 0)) (+ (+ 1 0) 0))) (+ (+ 1 0) 0))))
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 7 0) (cc 6 1)))))) (+ (+ 0 (+ (+ 0 (+ (cc 2 1) (cc 3 0))) 0)) (+ 1 0))) (+ 1 0))))
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 6 0) (cc 5 1))))))) (+ (+ 0 (+ (+ 0 (+ (+ (cc 2 0) (cc 1 1)) 0)) 0)) 1)) 1)))
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 5 0) (cc 4 1)))))))) (+ (+ 0 (+ (+ 0 (+ (+ 0 (+ (cc 0 1) (cc 1 0))) 0)) 0)) 1)) 1)))
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 4 0) (cc 3 1))))))))) (+ (+ 0 (+ (+ 0 (+ (+ 0 (+ 1 0)) 0)) 0)) 1)) 1)))
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 3 0) (cc 2 1)))))))))) (+ (+ 0 (+ (+ 0 (+ (+ 0 1) 0)) 0)) 1)) 1)))
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 2 0) (cc 1 1))))))))))) (+ (+ 0 (+ (+ 0 (+ 1 0)) 0)) 1)) 1)))
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ (cc 1 0) (cc 0 1)))))))))))) (+ (+ 0 (+ (+ 0 1) 0)) 1)) 1)))
;(+ (+ (+ (+ (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 0 (+ 1 0))))))))))) (+ (+ 0 (+ 1 0)) 1)) 1)))
;.. It is now just reduction, and the tree has been captured till leaves.
;Space - O(number of coins) - because max depth would be due to having all change in terms of penny
;Number of steps - O(number of coins**amount) - exponential

;Problem 1.15 a
;sine(12.15)
;(p (sine 4.05))
;(p (p (sine 1.35)))
;(p (p (p (sine 0.45))))
;(p (p (p (p (sine 0.15)))))
;(p (p (p (p (p (sine 0.05))))))
;p is invoked 5 times
;Problem 1.15 b
;Space - O(log a)
;Number of steps - O(log a)

;Problem 1.16
(define (fast-expt-iter a b n)
  (define (square n)
    (* n n))
  (define (even? n)
  (= (remainder n 2) 0))
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter a (square b) (/ n 2)))
        (else (fast-expt-iter (* a b) b (- n 1)))))
;Sample invocation (fast-expt-iter 1 b n)

;Problem 1.17
(define (fast-mult a b)
  (define (even? n)
    (= (remainder n 2) 0))
  (define (halve n)
    (/ n 2))
  (define (double n)
    (* n 2))
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (- b 1))))))

;Problem 1.18
(define (fast-mult-iter result a b)
  (define (even? n)
    (= (remainder n 2) 0))
  (define (halve n)
    (/ n 2))
  (define (double n)
    (* n 2))
  (cond ((= b 0) result)
        ((even? b) (fast-mult-iter result (double a) (halve b)))
        (else (fast-mult-iter (+ result a) a (- b 1)))))

;Problem 1.19
; a = bq + aq + ap
; b = bp + aq
; a' = (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;    = b(2pq + qq) + a(2pq + qq) + a(qq + pp)
; b' = (bp + aq)p + (bq + aq + ap)q
;    = b(qq + pp) + a(2pq + qq)

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;Problem 1.20
;Normal order of evaluation
;(gcd 206 40)
;(gcd 40 (remainder 206 40))
;(gcd (remainder 206 40) (remainder 40 (remainder 206 40)) It is calculated once during evaluating to 0. (1)
;(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) It is calculated once during evaluating to 0. (2)
;(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) (3)
;(gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) ((remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
;18 times in last execution + 4 times for equating to 0 = 22 times in total

;Applicative order of evaluation
;(gcd 206 40) (1)
;(gcd 40 6)   (2)
;(gcd 6 4)    (3)
;(gcd 4 2)    (4)
;(gcd 2 0)
;We invoked 4 times in total in applicative order of evaluation compared to 22 of normal order of evaluation.

