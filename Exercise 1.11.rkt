#lang racket
; Exercise 1.11
; This is recursive version of f
(define (f n)
  (if (< n 3)
      n
      (+ (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))
            )
         )
      )
  )

; This is iterative version of f
(define (f_i n)
  (if (< n 3)
      n
      (iter 0 1 2 3 n)
      )
  )

(define (iter a b c m n)
  (if (> m n)
      c
      (iter b c (+ c (* 2 b) (* 3 a)) (+ m 1) n)
      )
  )
