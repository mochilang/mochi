#lang racket
(require racket/list)

(define (idx x i) (if (string? x) (string-ref x i) (list-ref x i)))
(define (slice x s e) (if (string? x) (substring x s e) (take (drop x s) (- e s))))

(define (expand s left right)
	(let/ec return
		(define l left)
		(define r right)
		(define n (length s))
		(let/ec brk0
			(let loop0 ()
				(when (and (>= l 0) (< r n))
					(if (not (= (idx s l) (idx s r)))
						(begin
							(brk0 (void))
						)
						(void)
					)
					(set! l (- l 1))
					(set! r (+ r 1))
					(loop0))
				)
			)
			(return (- (- r l) 1))
			(return (void))
		)
	)
	
	(define (longestPalindrome s)
		(let/ec return
			(if (<= (length s) 1)
				(begin
					(return s)
				)
				(void)
			)
			(define start 0)
			(define end 0)
			(define n (length s))
			(for ([i (in-range 0 n)])
				(define len1 (expand s i i))
				(define len2 (expand s i (+ i 1)))
				(define l len1)
				(if (> len2 len1)
					(begin
						(set! l len2)
					)
					(void)
				)
				(if (> l (- end start))
					(begin
						(set! start (- i (/ ((- l 1)) 2)))
						(set! end (+ i (/ l 2)))
					)
					(void)
				)
			)
			(return (slice s start (+ end 1)))
			(return (void))
		)
	)
	
