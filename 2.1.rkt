#lang racket
; 2.1 Data Abstraction

; 2.1.1 Example of Data Abstraction: Arithmetic Oper.s for Rational Numbers


(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; Cons
(define x (cons 1 2))


; Definition of make-rat
(define (make-rat-ver1 n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

; Definition of printing a rational number
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; Another version of make-rat which reduces the rational numbers to lowest terms
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

; Exercise 2.1
(define (make-rat-ver2 n d)
  (let ((g (gcd (abs n) (abs d))))
    (cond
      ((or (and (> n 0) (> d 0)) (and (< n 0) (< d 0)))
       (cons (/ (abs n) g) (/ (abs d) g)))
      ((or (and (> n 0) (< d 0)) (and (< n 0) (> d 0)))
       (cons (/ (- (abs n)) g) (/ (abs d) g)))
      ((and (= n 0) (not (= d 0)))
       (cons 0 1))
      ((= d 0)
       (error "Cannot divide by zero.")))))
  


(define (make-rat-2 n d) (cons n d))

(define (numer-2 x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))

(define (denom-2 x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))


; 2.1.3

(define (cons-2 x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS" m))))
dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))


; 2.1.4: Example

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
(make-interval (min p1 p2 p3 p4)
               (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

; Exercise 2.9
(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

; Exercise 2.12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (* c (- 1 p)) (* c (+ 1 p))))

(define (percent i)
  (/ (width i) (center i))