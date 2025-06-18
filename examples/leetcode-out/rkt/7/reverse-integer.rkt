#lang racket
(require racket/list)

(define (idx x i)
  (cond [(string? x) (string-ref x i)]
        [(hash? x) (hash-ref x i)]
        [else (list-ref x i)]))
(define (slice x s e) (if (string? x) (substring x s e) (take (drop x s) (- e s))))

(define (reverse x)
	(let/ec return
		(define sign 1)
		(define n x)
		(if (< n 0)
			(begin
				(set! sign (- 1))
				(set! n (- n))
			)
			(void)
		)
		(define rev 0)
		(let loop ()
			(when (not (= n 0))
				(define digit (modulo n 10))
				(set! rev (+ (* rev 10) digit))
				(set! n (/ n 10))
				(loop))
		)
		(set! rev (* rev sign))
		(if (or (< rev ((- (- 2147483647) 1))) (> rev 2147483647))
			(begin
				(return 0)
			)
			(void)
		)
		(return rev)
		(return (void))
	)
)

