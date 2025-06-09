#lang racket

; Exercise 1.16

(define (fast_expo_iter b n)
  (iter b n 1)
  )

(define (iter b counter prod)
  (if (= counter 0)
      prod
      (if (= (remainder counter 2) 0)
          (iter (* b b) (/ counter 2) prod)
          (iter b (- counter 1) (* prod b))
      )
    )
  )

; Exercise 1.17

(define (multiplication a b)
  (if (= b 0)
      0
      (mphelper 0 a b))
  )

(define (mphelper S a b)
  (if (= b 0)
      S
  (if (= (remainder b 2) 0)
      (mphelper S (double a) (halve b))
      (mphelper (+ S a) a (- b 1))
      )
  )
)

(define (double x)
  (+ x x)
  )

(define (halve x)
  (/ x 2)
  )
