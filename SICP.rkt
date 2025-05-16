#lang racket
; Example of recursion
(define (fibo n)
    (if (or (= n 0) (= n 1))
        1
        (+ (fibo (- n 1)) (fibo (- n 2)))))

; Recursion version of factorial. Recursive process, recursive procedure
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

; Iteration version of factorial. Iterative process in recursive procedure
(define (factorial-iteration n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
 

(factorial 10)
(fibo 12)
(factorial-iteration 10)
