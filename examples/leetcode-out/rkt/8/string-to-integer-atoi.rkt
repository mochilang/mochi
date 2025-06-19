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

(define (digit ch)
	(let/ec return
		(if (= ch "0")
			(begin
				(return 0)
			)
			(void)
		)
		(if (= ch "1")
			(begin
				(return 1)
			)
			(void)
		)
		(if (= ch "2")
			(begin
				(return 2)
			)
			(void)
		)
		(if (= ch "3")
			(begin
				(return 3)
			)
			(void)
		)
		(if (= ch "4")
			(begin
				(return 4)
			)
			(void)
		)
		(if (= ch "5")
			(begin
				(return 5)
			)
			(void)
		)
		(if (= ch "6")
			(begin
				(return 6)
			)
			(void)
		)
		(if (= ch "7")
			(begin
				(return 7)
			)
			(void)
		)
		(if (= ch "8")
			(begin
				(return 8)
			)
			(void)
		)
		(if (= ch "9")
			(begin
				(return 9)
			)
			(void)
		)
		(return (- 1))
		(return (void))
	)
)

(define (myAtoi s)
	(let/ec return
		(define i 0)
		(define n (length s))
		(let loop ()
			(when (and (< i n) (= (idx s i) (idx " " 0)))
				(set! i (+ i 1))
				(loop))
		)
		(define sign 1)
		(if (and (< i n) ((or (= (idx s i) (idx "+" 0)) (= (idx s i) (idx "-" 0)))))
			(begin
				(if (= (idx s i) (idx "-" 0))
					(begin
						(set! sign (- 1))
					)
					(void)
				)
				(set! i (+ i 1))
			)
			(void)
		)
		(define result 0)
		(let/ec brk0
			(let loop0 ()
				(when (< i n)
					(define ch (slice s i (+ i 1)))
					(define d (digit ch))
					(if (< d 0)
						(begin
							(brk0 (void))
						)
						(void)
					)
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
	
