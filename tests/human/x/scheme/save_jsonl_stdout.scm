(define people
  (list (list (cons 'name "Alice") (cons 'age 30))
        (list (cons 'name "Bob") (cons 'age 25))))

(for-each (lambda (p)
            (let ((line (string-append "{\"name\":\"" (cdr (assoc 'name p)) "\",\"age\":"
                                       (number->string (cdr (assoc 'age p))) "}")))
              (display line)
              (newline)))
          people)
