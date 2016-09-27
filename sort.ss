; sort.ss
; Noah Brackenbury, Joey Long, Xingfan Xia
; CS 251, Fall 2016

; Given a two-argument function and a list, write a function that returns a list containing
; the elements of list sorted according to the given predicate.
(define mysort
  (lambda (f L)
    (if (null? L)
        L
        (if (null? (cdr L))
            L
            (merge-based-on-f f
             (mysort f (car(split L)))
             (mysort f (cadr(split L))))))))

;function that split a list into 2 list containing its odd and even elements separately
(define split
  (lambda (L)
    (cons (odd-indeces L) (cons (even-indeces L) '()))))

; Helper functions:
; mergesort based on given function f
(define merge-based-on-f
  (lambda (f L1 L2)
    (if (null? L2)
        L1
        (if (null? L1)
            L2
            (if (f (car L1) (car L2))
                (cons (car L1) (merge-based-on-f f (cdr L1) L2))
                (cons (car L2) (merge-based-on-f f (cdr L2) L1)))))))

; simple mergesort in scheme, implemented by splitting the lists not in
; half, but into odd and even indeces. written with help from online references
(define merge-sort
  (lambda (L)
    (if (null? L)
        L
        (if (null? (cdr L))
            L
            (merge-list
             (merge-sort (odd-indeces L))
             (merge-sort (even-indeces L)))))))

; merge two list together in order
(define merge-list
  (lambda (L1 L2)
    (if (null? L2)
        L1
        (if (null? L1)
            L2
            (if (< (car L1) (car L2))
                (cons (car L1) (merge-list (cdr L1) L2))
                (cons (car L2) (merge-list (cdr L2) L1)))))))

; get odd indeces of the given list and return a new list containing them
(define odd-indeces
  (lambda (L)
    (if (null? L)
        '()
        (if (null? (cdr L))
              (list (car L))
              (cons (car L) (odd-indeces (cdr (cdr L))))))))

; get even indeces of the given list and return a new list containing them
(define even-indeces
    (lambda (L)
      (if (null? L)
          '()
          (if (null? (cdr L))
              '()
              (cons (car (cdr L)) (even-indeces (cdr (cdr L))))))))

; test cases
; (merge-sort '(3 4 5 2 3 8 9 70 34 23 12 3 45 34))
(mysort < '(3 5 9 1 125 2 34 1 16 1 14 61 61))
(mysort > '(3 5 9 1 125 2 34 1 16 1 14 61 61))
(mysort (lambda (x y) (< (string-length x) (string-length y)))'("friends" "romans" "countrymen" "lend" "me" "your" "ears"))
(mysort (lambda (x y) (> (string-length x) (string-length y))) '("friends" "romans" "countrymen" "lend" "me" "your" "ears"))
(mysort string-ci<? '("friends" "romans" "countrymen" "lend" "me" "your" "ears"))