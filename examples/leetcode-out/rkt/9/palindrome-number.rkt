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

(define (isPalindrome x)
	(let/ec return
		(if (< x 0)
			(begin
				(return false)
			)
			(void)
		)
		(define s (format "~a" x))
		(define n (count s))
		(for ([i (in-range 0 (/ n 2))])
			(if (not (= (idx s i) (idx s (- (- n 1) i))))
				(begin
					(return false)
				)
				(void)
			)
		)
		(return true)
		(return (void))
	)
)

