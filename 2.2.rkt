#lang racket
; These codes convey the same meaning.
(cons 1
      (cons 2
            (cons 3
                  (cons 4 '()))))

(list 1 2 3 4)

(define one-through-four (list 1 2 3 4))

(car one-through-four) ; = 1
(cdr one-through-four) ; = (2 3 4)
(car (cdr one-through-four)) ; = 2
(cons 10 one-through-four) ; = (10 1 2 3 4)
(cons 5 one-through-four) ;= (5 1 2 3 4)

; list-ref function
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))
(list-ref squares 3)

; length function
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))
(length odds)

; length function in iterative process
(define (length-2 items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))
(length-2 odds)



; The append function is like this:
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(append squares odds)
(append odds squares)

; Exercise 2.17
(define (last-pair list)
  (if (null? (cdr list))
         list
         (last-pair (cdr list))))

; Exercise 2.18
(define (reverse list)
  (define (reverse-helper list2 list)
    (if (null? list)
        list2
        (reverse-helper (cons (car list) list2) (cdr list))))
  (reverse-helper '() list))
    
; Exercise 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define (first-denomination coin-values)
  (car coin-values)
  )

(define (except-first-denomination coin-values)
  (cdr coin-values)
  )

(define (no-more? coin-values)
  (null? coin-values)
  )

(cc 100 us-coins) ; = 292

; Exercise 2.20
(define (same-parity . xs)
  (define (helper pred xs)
    (cond ((null? xs) xs)
          ((pred (car xs))
           (cons (car xs)
                 (helper pred (cdr xs))))
          (else (helper pred (cdr xs)))))
  (cond ((null? xs) xs)
        ((even? (car xs)) (helper even? xs))
        (else (helper odd? xs))))

; Mapping over lists - example
(define (scale-list items factor)
  (if (null? items)
      '() 
      (cons (* (car items) factor)
            (scale-list (cdr items)
                        factor))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

; 2.21
(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items))
            (square-list (cdr items)))))

(define (square-list-2 items)
  (map (lambda (x) (* x x)) items))

; 2.2.2
(define x (cons (list 1 2) (list 3 4)))
(length x)

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(count-leaves x)

(define xx (list x x))
(length xx)
(count-leaves xx)

; 2.27

(set! x (list (list 1 2) (list 3 4)))
(reverse x)

(define (deep-reverse items)
  (cond ((null? items) '())
        ((not (pair? items)) items)
        (else (append (deep-reverse (cdr items))
                      (list (deep-reverse (car items)))))))

(deep-reverse x)

; 2.28
(define (fringe items)
  (cond ((null? items) '())
        ((not (pair? items)) (list items))
        (else (append (fringe (car items))
                      (fringe (cdr items))))))

(fringe x)

; Mapping over trees

(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

(define (scale-tree-2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

; 2.30
(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

; 2.31
(define (tree-map f tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (f tree))
        (else (cons (tree-map f (car tree))
                    (tree-map f (cdr tree))))))

; 2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((first (car s))
            (rest (subsets (cdr s))))
        (append rest (map (lambda (set) (cons first set))
                          rest)))))

; Sequences as Conventional Interfaces example
(define (square x) (* x x))

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares-2 tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (fib n)
  (define (iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (iter a
                 b
                 (+ (* p p) (* q q))
                 (+ (* q q) (* 2 p q))
                 (/ count 2)))
          (else (iter (+ (* b q) (* a q) (* a p))
                      (+ (* b p) (* a q))
                      p
                      q
                      (- count 1)))))
  (iter 1 0 0 1 n))

(define (even-fibs n)
  (accumulate
   cons
   '()
   (filter even? (map fib (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate
   cons
   '()
   (map square (map fib (enumerate-interval 0 n)))))

; 2.33
(define (map-2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(define (append-2 seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length-3 sequence)
  (accumulate (lambda (x n) (+ n 1)) 0 sequence))

; 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

; 2.35
(define (count-leaves-2 t)
  (accumulate + 0 (map (lambda (x) 1)
                       (enumerate-tree t))))

; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (u) (dot-product u v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r)
           (map (lambda (c)
                  (dot-product r c))
                cols))
         m)))

; 2.39
(define fold-right accumulate)

(define (fold-left op init xs)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init xs))

(define (reverse-2 sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))
(define (reverse-3 sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

; Nested Mappings
(define (prime? n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (next n)
    (if (= n 2) 3 (+ n 2)))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                           (lambda (i)
                             (map (lambda (j) (list i j))
                                  (enumerate-interval 1 (- i 1))))
                           (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s) ; empty set?
      (list '()) ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

; 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; 2.41
(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (triple-sums n s)
  (filter (lambda (t)
            (= s (+ (car t) (cadr t) (caddr t))))
          (unique-triples n)))

; 2.42
(define make-position list)
(define get-row car)
(define get-col cadr)

(define empty-board '())
(define (adjoin-position row col board)
  (cons (make-position row col) board))

(define (safe? positions)
  (let ((row1 (get-row (car positions))))
    (define (helper rest-of-queens cols-apart)
      (or (null? rest-of-queens)
          (let ((row2 (get-row (car rest-of-queens))))
            (and (not (= row1 row2))
                 (not (= row1 (- row2 cols-apart)))
                 (not (= row1 (+ row2 cols-apart)))
                 (helper (cdr rest-of-queens) (+ cols-apart 1))))))
    (helper (cdr positions) 1)))


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

