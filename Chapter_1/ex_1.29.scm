(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (simpsons-integral f a b n)
  (define h (/ (- b a) n))
  (define (yk k) (f (+ a (* k h))))
  (define (simpsons-integral-next k) (+ k 1))
  (define (simpsons-integral-term k)
    (* (cond ((= k 0) 1)
	     ((= k n) 1)
	     ((even? k) 2)
	     (else 4))
       (yk k)))
  (* (/ h 3) (sum simpsons-integral-term 0 simpsons-integral-next n)))

(display (simpsons-integral cube 0.0 1.0 100))
(newline)
(display (simpsons-integral cube 0.0 1.0 1000))
(newline)
