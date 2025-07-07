(define items '(((cat . "a") (val . 3))
               ((cat . "a") (val . 1))
               ((cat . "b") (val . 5))
               ((cat . "b") (val . 2))))

;; accumulate totals
(define totals '())
(for-each (lambda (item)
            (let* ((cat (cdr (assoc 'cat item)))
                   (val (cdr (assoc 'val item)))
                   (entry (assoc cat totals)))
              (if entry
                  (set-cdr! entry (+ (cdr entry) val))
                  (set! totals (append totals (list (cons cat val)))))))
          items)

;; sort descending by total
(define sorted (sort totals (lambda (a b) (> (cdr a) (cdr b)))))
(define result (map (lambda (p) (list (cons 'cat (car p)) (cons 'total (cdr p))))
                    sorted))
(display result)
(newline)
