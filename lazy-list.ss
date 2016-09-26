; Create a lazy list containing the infinite sequence of values a, a + 1, . . .
(define lazy-infinite-range
  (lambda (a)
 	(cons a (lambda () (lazy-ininite-range (+ a 1))))))
; test case
(lazy-infinite-range 100)

; Construct a regular-old (nonlazy) list containing the first n values in the lazy list LL. 
; If LL contains fewer than n values, return all of them.
(define first-n
  (lambda (LL n)
  	(if (> n (length LL))
   		LL
     	(if (= n 0)
          '()
          (cons (car LL) (first-n (cdr LL) (- n 1)))))))
     	
; test case
(first-n '(1 2 3 4 5) 3)

; Compute the nth value in the lazy list LL. If LL contains fewer than n values, return #f.
(define nth
  (lambda (LL n)
    (if (= n 1)
        (car LL)
        (nth (cdr LL) (- n 1)))))
; test case
(nth '(1 2 3 4 5) 3)

(define not-divisible?
  (lambda (k)
  (lambda (n) (if (= (modulo n k) 0) #t #f))))

(not-divisible? 2)

((not-divisible? 2) 3)

((not-divisible? 2) 4)