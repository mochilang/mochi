(define data
  (list (list (cons 'a 1) (cons 'b 2))
        (list (cons 'a 1) (cons 'b 1))
        (list (cons 'a 0) (cons 'b 5))))

(define sorted
  (sort data
        (lambda (x y)
          (let ((a1 (cdr (assoc 'a x)))
                (b1 (cdr (assoc 'b x)))
                (a2 (cdr (assoc 'a y)))
                (b2 (cdr (assoc 'b y))))
            (if (= a1 a2)
                (< b1 b2)
                (< a1 a2)))))
  )

(write sorted)
(newline)
