#lang racket
(require racket/list)

(define (idx x i) (if (string? x) (string-ref x i) (list-ref x i)))
(define (slice x s e) (if (string? x) (substring x s e) (take (drop x s) (- e s))))

(define (findMedianSortedArrays nums1 nums2)
	(let/ec return
		(define merged (list ))
		(define i 0)
		(define j 0)
		(let loop ()
			(when (or (< i (length nums1)) (< j (length nums2)))
				(if (>= j (length nums2))
					(begin
						(set! merged (+ merged (list (idx nums1 i))))
						(set! i (+ i 1))
					)
					(if (>= i (length nums1))
						(begin
							(set! merged (+ merged (list (idx nums2 j))))
							(set! j (+ j 1))
						)
						(if (<= (idx nums1 i) (idx nums2 j))
							(begin
								(set! merged (+ merged (list (idx nums1 i))))
								(set! i (+ i 1))
							)
							(begin
								(set! merged (+ merged (list (idx nums2 j))))
								(set! j (+ j 1))
							)
						)
					)
				)
				(loop))
		)
		(define total (length merged))
		(if (= (modulo total 2) 1)
			(begin
				(return (idx merged (/ total 2)))
			)
			(void)
		)
		(define mid1 (idx merged (- (/ total 2) 1)))
		(define mid2 (idx merged (/ total 2)))
		(return (/ ((+ mid1 mid2)) 2))
		(return (void))
	)
)

