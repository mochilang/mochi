(define people
  (list (list (cons 'name "Alice") (cons 'age 30))
        (list (cons 'name "Bob") (cons 'age 15))
        (list (cons 'name "Charlie") (cons 'age 65))
        (list (cons 'name "Diana") (cons 'age 45))))

(define adults
  (filter (lambda (p) (>= (cdr (assoc 'age p)) 18)) people))

(display "--- Adults ---")
(newline)
(for-each
 (lambda (p)
   (let ((age (cdr (assoc 'age p)))
         (name (cdr (assoc 'name p))))
     (display name)
     (display " is ")
     (display age)
     (when (>= age 60) (display " (senior)"))
     (newline)))
 adults)
