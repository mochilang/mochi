(define items
  (list
    (list (cons 'cat "a") (cons 'val 10) (cons 'flag #t))
    (list (cons 'cat "a") (cons 'val 5) (cons 'flag #f))
    (list (cons 'cat "b") (cons 'val 20) (cons 'flag #t))))

(define groups '())

(define (add-item i)
  (let* ((cat (cdr (assoc 'cat i)))
         (val (cdr (assoc 'val i)))
         (flag (cdr (assoc 'flag i)))
         (entry (assoc cat groups)))
    (if entry
        (let ((pair (cdr entry)))
          (set-car! pair (+ (car pair) (if flag val 0)))
          (set-cdr! pair (+ (cdr pair) val)))
        (set! groups (cons (cons cat (cons (if flag val 0) val)) groups)))))

(for-each add-item items)

(define result
  (sort (map (lambda (e)
               (let* ((cat (car e))
                      (num (car (cdr e)))
                      (den (cdr (cdr e))))
                 (list (cons 'cat cat)
                       (cons 'share (/ num den)))))
             groups)
        (lambda (a b)
          (string<? (cdr (assoc 'cat a)) (cdr (assoc 'cat b))))))

(write result)
(newline)
