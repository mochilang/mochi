(define products
  (list (list (cons 'name "Laptop") (cons 'price 1500))
        (list (cons 'name "Smartphone") (cons 'price 900))
        (list (cons 'name "Tablet") (cons 'price 600))
        (list (cons 'name "Monitor") (cons 'price 300))
        (list (cons 'name "Keyboard") (cons 'price 100))
        (list (cons 'name "Mouse") (cons 'price 50))
        (list (cons 'name "Headphones") (cons 'price 200))))

(define sorted
  (sort products (lambda (a b)
                   (> (cdr (assoc 'price a))
                      (cdr (assoc 'price b))))))

(define (take lst n)
  (if (or (zero? n) (null? lst)) '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(define (drop lst n)
  (if (or (zero? n) (null? lst)) lst
      (drop (cdr lst) (- n 1))))

(define expensive (take (drop sorted 1) 3))

(display "--- Top products (excluding most expensive) ---")
(newline)
(for-each (lambda (item)
            (display (cdr (assoc 'name item)))
            (display " costs $")
            (display (cdr (assoc 'price item)))
            (newline))
          expensive)
