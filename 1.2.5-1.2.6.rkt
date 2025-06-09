#lang racket
; Example of Euclid's Algorithm in the textbook
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (prime? n)
  (= n (smallest-divisor n)))

; Searching for divisors example code
(define (square m) (* m m))
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))


; Fermat's Little Theorem
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))


; Exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds))
  #t)

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-inexact-milliseconds) start-time))
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes M)
  (define (iter count n)
    (cond ((= count 3) 'done)
          ((prime? n)
           (timed-prime-test n)
           (iter (+ count 1) (+ n 2)))
          (else
           (iter count (+ n 2)))))
  (if (even? M)
      (iter 0 (+ M 1))
      (iter 0 M)))


