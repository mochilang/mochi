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

(define (isMatch s p)
	(let/ec return
		(define m (count s))
		(define n (count p))
		(define dp (list ))
		(define i 0)
		(let loop ()
			(when (<= i m)
				(define row (list ))
				(define j 0)
				(let loop ()
					(when (<= j n)
						(set! row (+ row (list false)))
						(set! j (+ j 1))
						(loop))
				)
				(set! dp (+ dp (list row)))
				(set! i (+ i 1))
				(loop))
		)
		(set! dp (list-set dp m (list-set (idx dp m) n true)))
		(define i2 m)
		(let loop ()
			(when (>= i2 0)
				(define j2 (- n 1))
				(let loop ()
					(when (>= j2 0)
						(define first false)
						(if (< i2 m)
							(begin
								(if (or ((= (idx p j2) (idx s i2))) ((= (idx p j2) ".")))
									(begin
										(set! first true)
									)
									(void)
								)
							)
							(void)
						)
						(define star false)
						(if (< (+ j2 1) n)
							(begin
								(if (= (idx p (+ j2 1)) "*")
									(begin
										(set! star true)
									)
									(void)
								)
							)
							(void)
						)
						(if star
							(begin
								(define ok false)
								(if (idx (idx dp i2) (+ j2 2))
									(begin
										(set! ok true)
									)
									(begin
										(if first
											(begin
												(if (idx (idx dp (+ i2 1)) j2)
													(begin
														(set! ok true)
													)
													(void)
												)
											)
											(void)
										)
									)
								)
								(set! dp (list-set dp i2 (list-set (idx dp i2) j2 ok)))
							)
							(begin
								(define ok false)
								(if first
									(begin
										(if (idx (idx dp (+ i2 1)) (+ j2 1))
											(begin
												(set! ok true)
											)
											(void)
										)
									)
									(void)
								)
								(set! dp (list-set dp i2 (list-set (idx dp i2) j2 ok)))
							)
						)
						(set! j2 (- j2 1))
						(loop))
				)
				(set! i2 (- i2 1))
				(loop))
		)
		(return (idx (idx dp 0) 0))
		(return (void))
	)
)

