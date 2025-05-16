#lang racket
;#SICP Exercise 1.2

(/ (+ 5 (+ 4 (- 2 (- 3 (+ 6 (/ 4 5))))))
   (* 3 (* (- 6 2) (- 2 7)))
)

;#SICP Exercise 1.3

(define (square x)
  (* x x))

(define (TwoBiggestSquareSum x y z)
  (if (> x y)
      (if (> y z)
          (+ (square x) (square y))
          (+ (square x) (square z)))
      (if (> z x)
          (+ (square y) (square z))
          (+ (square y) (square x))
          )
      )
  )

(TwoBiggestSquareSum 4 5 6)
(TwoBiggestSquareSum 4 6 5)
(TwoBiggestSquareSum 5 4 6)
(TwoBiggestSquareSum 5 6 4)
(TwoBiggestSquareSum 6 4 5)
(TwoBiggestSquareSum 6 5 4)

