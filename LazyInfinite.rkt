#lang lazy
(define print-only-errors #f)

;(define print-only-errors #t)
(define (test l r)
  (if (equal? l r)
      (if print-only-errors
          ()
          (printf "Test Passed~n"))
      (printf "Test Failed.~nActual:   ~S ~nExpected: ~S~n" l r)))

;; build-infinite-list : procedure -> listof any/c
;; returns infinite list of results (f n) indexed by n
(define (build-infinite-list f)
  (eval-next f 0))
(define (eval-next f n) 
  (cons (f n) (eval-next f (+ n 1))))
(test (list-ref (build-infinite-list (lambda(x) (* 2 x))) 5) 10)
(test (list-ref (build-infinite-list (lambda(x) (- 3 x))) 0) 3)
(test 3 3)
(test 3 2)


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

(define (is-divisible-by a b) 
  (if (= (modulo a b) 0)
      #t
      #f))

;; cons (p n) (eval-next-prime (+ n 1))
;(define primes (cons 2 (primes/fast)))
(define primes (cons 2 (eval-next-prime 3)))
(define (eval-next-prime n)
  (if (prime? n)
      (cons n (eval-next-prime (+ n 1)))
      (eval-next-prime (+ n 1))))
;; prime? : number -> boolean
;; determines whether n is prime
(define (prime? n) 
  (not (ormap (lambda(z) (is-divisible-by n z)) (take-while (lambda(x) (< x (sqrt n))) primes))))
(test (list-ref primes 3) 5)
(test (list-ref primes 10) 23)