(define people
  (list
    (list (cons 'name "Alice") (cons 'age 30) (cons 'city "Paris"))
    (list (cons 'name "Bob") (cons 'age 15) (cons 'city "Hanoi"))
    (list (cons 'name "Charlie") (cons 'age 65) (cons 'city "Paris"))
    (list (cons 'name "Diana") (cons 'age 45) (cons 'city "Hanoi"))
    (list (cons 'name "Eve") (cons 'age 70) (cons 'city "Paris"))
    (list (cons 'name "Frank") (cons 'age 22) (cons 'city "Hanoi"))))

(define stats '())

(define (update-stats city age)
  (let ((entry (assoc city stats)))
    (if entry
        (let ((pair (cdr entry)))
          (set-car! pair (+ (car pair) age))
          (set-cdr! pair (+ (cdr pair) 1)))
        (set! stats (cons (cons city (cons age 1)) stats))))

(for-each (lambda (p)
            (update-stats (cdr (assoc 'city p))
                          (cdr (assoc 'age p))))
          people)

(define result
  (map (lambda (entry)
         (let* ((city (car entry))
                (pair (cdr entry))
                (total (car pair))
                (count (cdr pair)))
           (list (cons 'city city)
                 (cons 'count count)
                 (cons 'avg_age (/ total count)))))
       stats))

(display "--- People grouped by city ---")
(newline)
(for-each
 (lambda (s)
   (let ((city (cdr (assoc 'city s)))
         (count (cdr (assoc 'count s)))
         (avg (cdr (assoc 'avg_age s))))
     (display city)
     (display ": count =")
     (display count)
     (display ", avg_age =")
     (display avg)
     (newline)))
 result)
