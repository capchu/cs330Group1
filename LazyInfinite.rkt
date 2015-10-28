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

;Contract: (build-infinite-list f) -> list of any
;                            f : function that returns any
;Purpose: To build an infinite list containing elements created through f
(define (build-infinite-list f)
  (eval-next f 0))
(define (eval-next f n) 
  (cons (f n) (eval-next f (+ n 1))))

;Basic Infinite List Test
(test (list-ref (build-infinite-list (lambda(x) (* 2 x))) 5) 10)
;Infinite Test Two
(test (list-ref (build-infinite-list (lambda(x) (- 3 x))) 0) 3)








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
;Test infinite List
(test (take-while even? (build-infinite-list (lambda(x) (+ 2 x)))) (list 2))
;Actual infinite list test
;(test (list-ref (take-while even? (build-infinite-list (lambda(x) (+ 2 x)))) 8) 16)








;Contract: (build-vector num f) -> a vector of whatever f returns
;                       num : number if vectors
;                         f : function with any return
;Purpose: To build a Vector containing items based on f
(define (build-vector num f)
  (apply vector (build-list num f)))
;No Test Cases, This was given to us







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









;Contract:  (lcs-length s1 s2) -> exact-positive-integer
;              s1: string
;              s2: string
;Purpose:   Finds and returns the length using Longest common subsequence algorithm
(define (lcs-length s1 s2)
  (letrec [(lcs-table (build-table (+ 1 (string-length s1))
                                   (+ 1 (string-length s2))
                                   (lambda (i j)
                                       (if (or (= i 0) (= j 0))
                                          0
                                          (if (char=? (string-ref s1 (- i 1)) (string-ref s2 (- j 1)))
                                            (+ 1 (lcs-length 
                                              (super-substring s1 0 (- i 1)) 
                                              (super-substring s2 0 (- j 1))))
                                            (max 
                                              (lcs-length 
                                                s1 
                                                (super-substring s2 0 (- j 1)))
                                              (lcs-length 
                                                (super-substring s1 0 (- i 1)) 
                                                s2))))
                                     ) 
                                   ))]
    (vector-ref (vector-ref lcs-table (string-length s1)) (string-length s2))
    ))

(define (super-substring str begin end)
  (if (< end begin)
    ""
    (substring str begin end)))

; Test from Wiki page (generic test)
(test (lcs-length "AGCAT" "GAC") 2)
; Generic test
(test (lcs-length "CAT" "BAT") 2)
; Test with all matching chars
(test (lcs-length "FooBar" "FooBar") 6)
; Test resonably large strings
(test (lcs-length "qwertyuiopasdfghjklzxcvbnm" "qwertyuiopasdfghjklzxcvbnm") 26)
; Test resonably large strings - with couple not matching
(test (lcs-length "zxertyuiopasdfghjklzxcvbnm" "qwertyuiopasdfghjklzxcvbnm") 24)



;I talked to the TA and someone was trying to write the test cases for the primes parts and it looked like you
;need to use the take-while function to test it.
(define (is-divisible-by a b) 
  (if (= (modulo a b) 0)
      #t
      #f))







;; prime? : number -> boolean
;; determines whether positive integer n is prime

;Contract: (prime? n) -> boolean
;                 n : exact-positive-intiger
;Purpose: Returns #t if n is a prime otherwise returns #f
(define (prime? n) 
  (not (ormap (lambda(z) (is-divisible-by n z)) (take-while (lambda(x) (<= x (sqrt n))) primes))))


;; cons (p n) (eval-next-prime (+ n 1))
;(define primes (cons 2 (primes/fast)))
;; primes : (listof number)
;; infinite list of all prime numbers

;Contract: primes : list of exact-positive-intiger 
;Purpose: A list of all prime numbers
(define primes (cons 2 (eval-next-prime 3)))
(define (eval-next-prime n)
  (if (prime? n)
      (cons n (eval-next-prime (+ n 1)))
      (eval-next-prime (+ n 1))))


;Test Smallest Prime
(test (prime? 2) #t)
;Test Largeish Prime
(test (prime? 1051) #t)
;Test Large Prime
(test (prime? 3049) #t)
;Test small composite
(test (prime? 6) #f)
;Test Largeish Composite
(test (prime? 1052) #f)
;Test Large Composite
(test (prime? 3050) #f)


;Test A Small Prime
(test (list-ref primes 4) 11)
;Test A Large Prime
(test (list-ref primes 200) 1229)






(define (primeN n) 
  (list-ref primes n))


;; primes/fast : (listof number)
;; infinite list of all prime numbers

;Contract: primes/fast -> infinite list of prime numbers
;Purpose: To list all the prime numbers lazily
(define primes/fast (build-infinite-list primeN))



;; prime?/fast : number -> boolean
;; determines whether positive integer n is prime
;OLD DEFINITION OF prime?/fast --- Did not work, multiple arity missmathces
;(define (prime?/fast n) 
;  (= (list-tail (take-while (lambda(x) (< (+ x 1))) primes/fast)) n))

;Contract: (prime?/fast n) -> boolean
;                    n : exact-positive-intiger
;Putpose: Tell if a number is prime using primes/fast
(define (prime?/fast n)
  (not (ormap (lambda(z) (is-divisible-by n z)) (take-while (lambda(x) (<= x (sqrt n))) primes/fast)))
  )


;Test Smallest Prime
(test (prime?/fast 2) #t)
;Test Largeish Prime
(test (prime?/fast 1051) #t)
;Test Large Prime
(test (prime?/fast 3049) #t)
;Test small composite
(test (prime?/fast 6) #f)
;Test Largeish Composite
(test (prime?/fast 1052) #f)
;Test Large Composite
(test (prime?/fast 3050) #f)




;Test A Small Prime
(test (list-ref primes/fast 4) 11)
;Test A Large Prime
(test (list-ref primes/fast 200) 1229)
