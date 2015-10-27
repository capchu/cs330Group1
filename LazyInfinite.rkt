#lang lazy
(define print-only-errors #f)

;(define print-only-errors #t)
(define (test l r)
  (if (equal? l r)
      (if print-only-errors
          ()
          (printf "Test Passed~n"))
        (printf "Test Failed.~nActual:   ~S ~nExpected: ~S~n" l r)
        
      )
  )

;; build-infinite-list : procedure -> listof any/c
;; returns infinite list of results (f n) indexed by n
(define (build-infinite-list f)
  (eval-next f 0))
(define (eval-next f n) 
  (cons (f n) (eval-next f (+ n 1))))

(test (list-ref (build-infinite-list (lambda(x) (* 2 x))) 5) 10)
(test (list-ref (build-infinite-list (lambda(x) (- 3 x))) 0) 3)
(test 3 3)


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
(test (take-while (lambda (n) (< n 4)) (list 1 3 4)) (list 1 3))
;Test terminate on second
(test (take-while odd? (list 1 4 3)) (list 1))
;Test terminate on first
(test (take-while odd? (list 4 1 3)) empty)
;test no termination
(test (take-while odd? (list 3 1 3 7)) (list 3 1 3 7))





;Contract:
;Purpose:
(define (build-vector num f)
  (apply vector (build-list num f)))


;Contract: (build-table rows cols f) -> (vectorof (vectorof any))
;                            rows : exact-positive-intiger
;                            cols : exact-positive-intiger
;                               f : function taking 2 exact positive intigers and returns any
;Purpose: Lazily constructs a vector such that
;(vector-ref (vector-ref (build-table rows cols f) i) j) equals (f i j) when (< i rows) (< j cols)
{define (build-table rows cols f)
  (build-vector rows (lambda (x) (build-vector cols (lambda (y) (f x y)))))
  }

;Test Basic table
(test (vector-ref (vector-ref (build-table 3 3 (lambda (x y) (+ x y))) 2) 2) 4)
;Test Small table
(test (vector-ref (vector-ref (build-table 2 1 (lambda (x y) (+ x y))) 1) 0) 1)
;Test Large table
(test (vector-ref (vector-ref (build-table 10 20 (lambda (x y) (+ x y))) 7) 13) 20)
;Test 0 size table
(test (vector-ref (vector-ref (build-table 1 1 (lambda (x y) (+ x y))) 0) 0) 0)




;Contract
;Purpose
(define (lcs-length s1 s2)
  (letrec [(lcs-table (build-table (+ 1 (string-length s1))
                                   (+ 1 (string-length s2))
                                   (lambda (i j)
                                     ;This Space will be where our part of the code is
                                     0  ; replace the "0" with a suitable body

                                     ) 
                                   ))]
    (vector-ref (vector-ref lcs-table (string-length s1)) (string-length s2))
    ))


(test (lcs-length "b"  "a" ) "")
(test (lcs-length "b"  "b" ) "b")
(test (lcs-length "bobindah"  "bobelhqz" ) "bobh")








(define (is-divisible-by a b) 
  (if (= (modulo a b) 0)
      #t
      #f))


;; cons (p n) (eval-next-prime (+ n 1))
;(define primes (cons 2 (primes/fast)))
;; primes : (listof number)
;; infinite list of all prime numbers
(define primes (cons 2 (eval-next-prime 3)))
(define (eval-next-prime n)
  (if (prime? n)
      (cons n (eval-next-prime (+ n 1)))
      (eval-next-prime (+ n 1))))



;; prime? : number -> boolean
;; determines whether positive integer n is prime
(define (prime? n) 
  (not (ormap (lambda(z) (is-divisible-by n z)) (take-while (lambda(x) (<= x (sqrt n))) primes))))
(define (primeN n) 
  (list-ref primes n))

;; primes/fast : (listof number)
;; infinite list of all prime numbers
(define primes/fast (build-infinite-list primeN))

;; prime?/fast : number -> boolean
;; determines whether positive integer n is prime
(define (prime?/fast n) 
  (= (list-tail (take-while (lambda(x) (< (+ x 1))) primes/fast)) n))
