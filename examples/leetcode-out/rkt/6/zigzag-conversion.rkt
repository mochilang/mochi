#lang racket
(require racket/list)

(define (idx x i)
  (cond [(string? x) (string-ref x i)]
        [(hash? x) (hash-ref x i)]
        [else (list-ref x i)]))
(define (slice x s e) (if (string? x) (substring x s e) (take (drop x s) (- e s))))

(define (convert s numRows)
	(let/ec return
		(if (or (<= numRows 1) (>= numRows (length s)))
			(begin
				(return s)
			)
			(void)
		)
		(define rows (list ))
		(define i 0)
		(let loop ()
			(when (< i numRows)
				(set! rows (+ rows (list "")))
				(set! i (+ i 1))
				(loop))
		)
		(define curr 0)
		(define step 1)
		(for ([ch s])
			(set! rows (if (hash? rows) (hash-set rows curr (+ (idx rows curr) ch)) (list-set rows curr (+ (idx rows curr) ch))))
			(if (= curr 0)
				(begin
					(set! step 1)
				)
				(if (= curr (- numRows 1))
					(begin
						(set! step (- 1))
					)
					(void)
				)
			)
			(set! curr (+ curr step))
		)
		(define result "")
		(for ([row rows])
			(set! result (+ result row))
		)
		(return result)
		(return (void))
	)
)

