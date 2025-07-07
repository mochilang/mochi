(define people
  (list
    (list (cons 'name "Alice") (cons 'city "Paris"))
    (list (cons 'name "Bob") (cons 'city "Hanoi"))
    (list (cons 'name "Charlie") (cons 'city "Paris"))
    (list (cons 'name "Diana") (cons 'city "Hanoi"))
    (list (cons 'name "Eve") (cons 'city "Paris"))
    (list (cons 'name "Frank") (cons 'city "Hanoi"))
    (list (cons 'name "George") (cons 'city "Paris"))))

(define groups '())

(define (add-person p)
  (let* ((city (cdr (assoc 'city p)))
         (entry (assoc city groups)))
    (if entry
        (set-cdr! entry (cons p (cdr entry)))
        (set! groups (cons (cons city (list p)) groups))))

(for-each add-person people)

(define big
  (filter (lambda (e) (>= (length (cdr e)) 4)) groups))

(define result
  (map (lambda (e)
         (list (cons 'city (car e))
               (cons 'num (length (cdr e)))))
       big))

(write result)
(newline)
