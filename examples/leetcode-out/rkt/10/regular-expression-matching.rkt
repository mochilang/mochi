#lang racket
(require racket/list)

(define (idx x i)
  (cond [(string? x) (string-ref x i)]
        [(hash? x) (hash-ref x i)]
        [else (list-ref x i)]))
(define (slice x s e) (if (string? x) (substring x s e) (take (drop x s) (- e s))))

(define (isMatch s p)
	(let/ec return
		(define m (length s))
		(define n (length p))
		(define memo (make-hash (list )))
		(define (dfs i j)
			(let/ec return
				(define key (+ (* i ((+ n 1))) j))
				(if (hash-has-key? memo key)
					(begin
						(return (idx memo key))
					)
					(void)
				)
				(if (= j n)
					(begin
						(return (= i m))
					)
					(void)
				)
				(define first false)
				(if (< i m)
					(begin
						(if (or ((= (idx p j) (idx s i))) ((= (idx p j) ".")))
							(begin
								(set! first true)
							)
							(void)
						)
					)
					(void)
				)
				(define ans false)
				(if (< (+ j 1) n)
					(begin
						(if (= (idx p (+ j 1)) "*")
							(begin
								(if (dfs i (+ j 2))
									(begin
										(set! ans true)
									)
									(if (and first (dfs (+ i 1) j))
										(begin
											(set! ans true)
										)
										(void)
									)
								)
							)
							(begin
								(if (and first (dfs (+ i 1) (+ j 1)))
									(begin
										(set! ans true)
									)
									(void)
								)
							)
						)
					)
					(begin
						(if (and first (dfs (+ i 1) (+ j 1)))
							(begin
								(set! ans true)
							)
							(void)
						)
					)
				)
				(set! memo (if (hash? memo) (hash-set memo key ans) (list-set memo key ans)))
				(return ans)
				(return (void))
			)
		)
		(return (dfs 0 0))
		(return (void))
	)
)

