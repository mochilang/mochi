(define (_count v)
  (cond
    ((string? v) (string-length v))
    ((and (pair? v) (assq 'Items v)) (length (cdr (assq 'Items v))))
    ((list? v) (length v))
    (else 0)))

(define (_sum v)
  (let* ((lst (cond
               ((and (pair? v) (assq 'Items v)) (cdr (assq 'Items v)))
               ((list? v) v)
               (else '())))
         (s (if (null? lst) 0 (apply + lst))))
    s))

(define (_avg v)
  (let ((lst (cond
               ((and (pair? v) (assq 'Items v)) (cdr (assq 'Items v)))
               ((list? v) v)
               (else '())))
        (n 0))
    (set! n (length lst))
    (if (= n 0) 0 (/ (_sum lst) n)))
)

(define (_exists v)
  (cond
    ((and (pair? v) (assq 'Items v)) (not (null? (cdr (assq 'Items v)))))
    ((string? v) (> (string-length v) 0))
    ((list? v) (not (null? v)))
    (else #f)))

(define (_max v)
  (let ((lst (cond
               ((and (pair? v) (assq 'Items v)) (cdr (assq 'Items v)))
               ((list? v) v)
               (else '())))
        (m 0))
    (when (not (null? lst))
      (set! m (car lst))
      (for-each (lambda (n)
                  (when (> n m) (set! m n)))
                (cdr lst)))
    m))

(define (_min v)
  (let ((lst (cond
               ((and (pair? v) (assq 'Items v)) (cdr (assq 'Items v)))
               ((list? v) v)
               (else '())))
        (m 0))
    (when (not (null? lst))
      (set! m (car lst))
      (for-each (lambda (n)
                  (when (< n m) (set! m n)))
                (cdr lst)))
    m))
(define (_group_by src keyfn)
  (let ((groups '()) (order '()))
    (for-each (lambda (it)
                (let* ((key (keyfn it))
                       (ks (format "~a" key))
                       (pair (assoc ks groups)))
                  (if pair
                      (let* ((grp (cdr pair))
                             (items (cdr (assq 'Items grp))))
                        (set-cdr! (assq 'Items grp) (append items (list it))))
                      (begin
                        (set! groups (append groups (list (cons ks (list (cons 'key key) (cons 'Items (list it)))))))
                        (set! order (append order (list ks))))))
              src)
    (map (lambda (k) (cdr (assoc k groups))) order))))

(begin (display (_count (list 1 2 3))) (newline))
