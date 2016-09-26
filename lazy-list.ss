; lazy-list.ss
; Noah Brackenbury, Joey Long, Xingfan Xia
; CS 251, Fall 16

; Example of lazy structure
(define lazy-range 
  (lambda (a b) 
    (if (> a b)
      '()
      (cons a
        (lambda () (lazy-range (+ a 1) b))))))
(define a (lazy-range 1 3))
((cdr a))
(define b ((cdr ((cdr a)))))
((cdr b))
; Create a lazy list containing the infinite sequence of values a, a + 1, . . .
(define lazy-infinite-range
  (lambda (a)
  (cons a (lambda () (lazy-infinite-range (+ a 1))))))
; test case
(lazy-infinite-range 100)

; Construct a regular-old (nonlazy) list containing the first n values in the lazy list LL. 
; If LL contains fewer than n values, return all of them.
(define first-n
  (lambda (LL n)
    (if (<= n 0)
       '()
       (if (not(null? LL)) ; or (if (pair? LL)) given LL is still a list
           (cons (car LL) (first-n ((cdr LL)) (- n 1)))
           '()))))
      
; test case
(first-n (lazy-infinite-range 100) 3)
(first-n (lazy-infinite-range 100) 9)
(first-n (lazy-range 50 100) 100)


; Compute the nth value in the lazy list LL. If LL contains fewer than n values, return #f.
(define nth
  (lambda (LL n)
    (if (= n 1)
        (car LL)
        (nth ((cdr LL)) (- n 1)))))
; test case
(nth (lazy-infinite-range 100) 3)

; A a higher-order function, so that applying not-divisible? to an argument k gives a predicate 
; for non-divisibility by k.
(define not-divisible?
  (lambda (k)
  (lambda (n) (if (= (modulo n k) 0) #f #t))))
; test case
(not-divisible? 2)
((not-divisible? 2) 3)
((not-divisible? 2) 4)

; Construct a new lazy list that represents (in order) every element x of LL such that (f x) is true. 
; That is: f is a one-argument function, and LL is a lazy list. You are asked to return a lazy 
; list containing all elements of LL that pass the f test. 
(define filter-lazy-list
  (lambda (f LL)
    (if (f (car LL))
        (cons (car LL)
              (lambda() (filter-lazy-list f ((cdr LL)))))
        (filter-lazy-list f ((cdr LL))))))
; test case
(define a (filter-lazy-list (lambda (x) (= (modulo x 2) 0)) (lazy-infinite-range 10)))
(define b (filter-lazy-list (lambda (x) (= (modulo x 5) 1)) (lazy-range 7 30)))

; A helper function that takes a lazy list LL as a parameter, and produces a new lazy list 
; whose cdr is a thunk representing precisely those elements of LL that are not divisible by (car LL).
(define not-divisible-LL
  (lambda (LL)
    (filter-lazy-list (not-divisible? (car LL)) ((cdr LL)))))
; test case
(define c (not-divisible-LL (lazy-infinite-range 2)))
(car c)
((cdr c))
((cdr((cdr c))))

; Write a zero-argument function sieve that returns a lazy list representing all
; prime numbers, starting at 2
(define sieve
  (lambda ()
    (eratosthenes (lazy-infinite-range 2))))
; helper function that takes input
(define eratosthenes
  (lambda (LL)
    (cons (car LL) (lambda () (eratosthenes (not-divisible-LL LL))))))
; test case
(first-n (sieve) 10)
(nth (sieve) 20)
(nth (sieve) 100)