#lang racket
(require racket/list)

(define (idx x i) (if (string? x) (string-ref x i) (list-ref x i)))
(define (slice x s e) (if (string? x) (substring x s e) (take (drop x s) (- e s))))

(define (lengthOfLongestSubstring s)
	(let/ec return
		(define n (length s))
		(define start 0)
		(define best 0)
		(define i 0)
		(let/ec brk0
			(let loop0 ()
				(when (< i n)
					(define j start)
					(let/ec brk1
						(let loop1 ()
							(when (< j i)
								(if (= (idx s j) (idx s i))
									(begin
										(set! start (+ j 1))
										(brk1 (void))
									)
									(void)
								)
								(set! j (+ j 1))
								(loop1))
							)
						)
						(define length (+ (- i start) 1))
						(if (> length best)
							(begin
								(set! best length)
							)
							(void)
						)
						(set! i (+ i 1))
						(loop0))
					)
				)
				(return best)
				(return (void))
			)
		)
		
