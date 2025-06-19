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

(define (expand s left right)
	(let/ec return
		(define l left)
		(define r right)
		(define n (count s))
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
			(if (<= (count s) 1)
				(begin
					(return s)
				)
				(void)
			)
			(define start 0)
			(define end 0)
			(define n (count s))
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
				(if (> l ((- end start)))
					(begin
						(set! start (- i ((/ ((- l 1)) 2))))
						(set! end (+ i ((/ l 2))))
					)
					(void)
				)
			)
			(return (slice s start (+ end 1)))
			(return (void))
		)
	)
	
