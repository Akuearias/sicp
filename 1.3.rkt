#lang racket
; 1.3

; 1.3.1
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))

(define (cube x) (* x x x))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))


#|

General form:

(define (⟨name⟩ a b)
(if (> a b)
0
(+ (⟨term⟩ a)
(⟨name⟩ (⟨next⟩ a) b))))

|#

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes-2 a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-ints a b)
  (sum identity a inc b))


(define (pi-sum-2 a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))


; Exercise 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

; Text 1.3.2 : 'lambda'

(define (pi-sum-3 a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral-2 f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(define (square x) (* x x))

(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

(define (f-2 x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

; 'let'

(define (f-3 x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

#|

(let ((⟨var1⟩ ⟨exp1⟩)
(⟨var2⟩ ⟨exp2⟩)
...
(⟨varn⟩ ⟨expn⟩))
⟨body⟩)

|#

; 1.3.3

(define (average x y)
  (/ (+ x y) 2))

(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))


(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

; Finding fixed points of functions
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
(define (try guess)
  (let ((next (f guess)))
    (if (close-enough? guess next)
        next
        (try next))))
(try first-guess))


; 1.3.4

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-2 x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))

; Exercise 1.40

(define (cubic a b c)
   (lambda (x) (+ c (* b x) (* a (* x x) (* x x x)))))


; Exercise 1.41

(define (double f)
  (lambda (x) (f (f x))))


; Exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))


; Exercise 1.43
(define (repeated f n)
  (if (= n 1)
      f
      (compose (repeated f (- n 1)) f)))

