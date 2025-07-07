(define scores '((alice . 1)))
(set! scores (cons '(bob . 2) scores))
(display (cdr (assoc 'bob scores)))
(newline)
