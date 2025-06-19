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

(define (addTwoNumbers l1 l2)
	(let/ec return
		(define i 0)
		(define j 0)
		(define carry 0)
		(define result (list ))
		(let loop ()
			(when (or (or (< i (length l1)) (< j (length l2))) (> carry 0))
				(define x 0)
				(if (< i (length l1))
					(begin
						(set! x (idx l1 i))
						(set! i (+ i 1))
					)
					(void)
				)
				(define y 0)
				(if (< j (length l2))
					(begin
						(set! y (idx l2 j))
						(set! j (+ j 1))
					)
					(void)
				)
				(define sum (+ (+ x y) carry))
				(define digit (modulo sum 10))
				(set! carry (/ sum 10))
				(set! result (+ result (list digit)))
				(loop))
		)
		(return result)
		(return (void))
	)
)

