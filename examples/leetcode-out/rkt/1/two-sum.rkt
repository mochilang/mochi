#lang racket
(require racket/list)

(define (idx x i) (if (string? x) (string-ref x i) (list-ref x i)))
(define (slice x s e) (if (string? x) (substring x s e) (take (drop x s) (- e s))))

(define (twoSum nums target)
	(let/ec return
		(define n (length nums))
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
