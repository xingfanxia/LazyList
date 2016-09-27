;merge sort of a lst based on given predicate 
(define sort
  (lambda (predicate lst)              
    (if (null? lst) lst
       (if (null? (cdr lst))t
            lst
            (merge predicate (sort predicate (odd lst)) (sort predicate (even lst)))))))

;merge two lists based on given predicate
(define merge
      (lambda (predicate lst1 lst2)
        (if (null? lst1) lst2
         (if (null? lst2) lst1
           (if (predicate (car lst1) (car lst2))
               (cons (car lst1) (merge predicate (cdr lst1) lst2))
               (cons (car lst2) (merge predicate (cdr lst2) lst1)))))))

;create a list consisting of the even positions of a given list
(define even
      (lambda (lst)
        (if (null? lst) '()
            (if (null? (cdr lst)) '()
                (cons (car (cdr lst)) (even (cdr (cdr lst))))))))

;create a list consisting of the odd positions of a given list                
    (define odd
      (lambda (lst)
        (if (null? lst) '()
            (if (null? (cdr lst))
                (list(car lst))
                (cons (car lst) (odd (cdr (cdr lst))))))))

(define a (even '(2 7 6 5 4 5 6 7 4)))

(define b (odd '(2 7 6 5 4 5 6 7 4)))

(define c (merge < a b))

;(define d (sort (lambda (x y) #t) '(5 3 6 1 4 9 101 48)))



