#lang racket
(require racket/list)

(define (idx x i)
  (cond [(string? x) (string-ref x i)]
        [(hash? x) (hash-ref x i)]
        [else (list-ref x i)]))
(define (slice x s e) (if (string? x) (substring x s e) (take (drop x s) (- e s))))
(define (count x)
  (cond [(string? x) (string-length x)]
        [(hash? x) (hash-count x)]
        [else (length x)]))
(define (avg x)
  (let ([n (count x)])
    (if (= n 0) 0
        (/ (for/fold ([s 0.0]) ([v x]) (+ s (real->double-flonum v))) n))))

(define (twoSum nums target)
	(let/ec return
		(define n (count nums))
		(for ([i (in-range 0 n)])
			(for ([j (in-range (+ i 1) n)])
				(if (= (+ (idx nums i) (idx nums j)) target)
					(begin
						(return (list i j))
					)
					(void)
				)
			)
		)
		(return (list (- 1) (- 1)))
		(return (void))
	)
)

(define result (twoSum (list 2 7 11 15) 9))
(displayln (idx result 0))
(displayln (idx result 1))
