#lang lazy
(define print-only-errors #f)

(define (test l r)
  (if (equal? l r)
      (if print-only-errors
          ()
          (printf "Test Passed~n"))
      (printf "Test Failed.~nActual:   ~S ~nExpected: ~S~n" l r)))





;Contract: (take-while p l) -> list of any
;                  p : any -> boolean
;                  l : list of any
;Purpose: to make a new list that contains every element in the
;           list up until p returns false for the first time
{define (take-while p l)
  (aux-take-while p l empty)
  }

{define (aux-take-while p l n)
  (if (empty? l)
      n
      (if (p (first l))
         (append (list (first l)) (aux-take-while p (rest l) n))
         n
       )
      )
  }

;Test an empty list
(test (take-while odd? empty) empty)
;Test terminate on 3rd
(test (take-while (lambda (n) (< n 4)) (list 1 3 4))
      (list 1 3))
;Test terminate on second
(test (take-while odd? (list 1 4 3))
      (list 1))
;Test terminate on first
(test (take-while odd? (list 4 1 3))
      empty)
;test no termination
(test (take-while odd? (list 3 1 3 7))
      empty)
