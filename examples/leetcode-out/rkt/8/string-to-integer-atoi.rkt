#lang racket
(require racket/list)

(define (idx x i)
  (cond [(string? x) (string-ref x i)]
        [(hash? x) (hash-ref x i)]
        [else (list-ref x i)]))
(define (slice x s e) (if (string? x) (substring x s e) (take (drop x s) (- e s))))

(define (myAtoi s)
	(let/ec return
		(define i 0)
		(define n (length s))
		(let loop ()
			(when (and (< i n) (= (idx s i) " "))
				(set! i (+ i 1))
				(loop))
		)
		(define sign 1)
		(if (and (< i n) ((or (= (idx s i) "+") (= (idx s i) "-"))))
			(begin
				(if (= (idx s i) "-")
					(begin
						(set! sign (- 1))
					)
					(void)
				)
				(set! i (+ i 1))
			)
			(void)
		)
		(define digits (make-hash (list (cons "0" 0) (cons "1" 1) (cons "2" 2) (cons "3" 3) (cons "4" 4) (cons "5" 5) (cons "6" 6) (cons "7" 7) (cons "8" 8) (cons "9" 9))))
		(define result 0)
		(let/ec brk0
			(let loop0 ()
				(when (< i n)
					(define ch (idx s i))
					(if (not ((hash-has-key? digits ch)))
						(begin
							(brk0 (void))
						)
						(void)
					)
					(define d (idx digits ch))
					(set! result (+ (* result 10) d))
					(set! i (+ i 1))
					(loop0))
				)
			)
			(set! result (* result sign))
			(if (> result 2147483647)
				(begin
					(return 2147483647)
				)
				(void)
			)
			(if (< result ((- 2147483648)))
				(begin
					(return (- 2147483648))
				)
				(void)
			)
			(return result)
			(return (void))
		)
	)
	
