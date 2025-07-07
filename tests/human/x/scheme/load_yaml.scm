(define people
  (list
    (list (cons 'name "Alice") (cons 'age 30) (cons 'email "alice@example.com"))
    (list (cons 'name "Bob") (cons 'age 15) (cons 'email "bob@example.com"))
    (list (cons 'name "Charlie") (cons 'age 20) (cons 'email "charlie@example.com"))))

(define adults
  (filter (lambda (p) (>= (cdr (assoc 'age p)) 18)) people))

(for-each (lambda (a)
            (display (cdr (assoc 'name a)))
            (display " ")
            (display (cdr (assoc 'email a)))
            (newline))
          adults)
