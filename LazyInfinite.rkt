#lang lazy

(define print-only-errors #t)
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

(define (is-divisible-by a b) 
  (if (= (modulo a b) 0)
      true
      false))




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
  (not (ormap (lambda(z) (is-divisible-by n z)) (filter (lambda(x) (< x (sqrt n))) primes))))


