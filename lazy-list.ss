; Create a lazy list containing the infinite sequence of values a, a + 1, . . .
(define lazy-ininite-range
  (lambda (a)
 	(cons a (lambda () (lazy-ininite-range (+ a 1))))))
; test case
(lazy-ininite-range 100)