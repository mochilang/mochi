(define (map-get m k)
    (let ((p (assoc k m)))
        (if p (cdr p) '()))
)
(define (map-set m k v)
    (let ((p (assoc k m)))
        (if p
            (begin (set-cdr! p v) m)
            (cons (cons k v) m)))
)
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
                       (ks (_to_string key))
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

(define customers (list (list (cons 'id 1) (cons 'name "Alice")) (list (cons 'id 2) (cons 'name "Bob")) (list (cons 'id 3) (cons 'name "Charlie"))))
(define orders (list (list (cons 'id 100) (cons 'customerId 1)) (list (cons 'id 101) (cons 'customerId 1)) (list (cons 'id 102) (cons 'customerId 2))))
(define stats (let ((_tmp '()))
  (for-each (lambda (c)
    (let ((_ms0 '()) (_m0 #f))
      (for-each (lambda (o)
        (when (equal? (map-get o 'customerId) (map-get c 'id))
          (set! _ms0 (append _ms0 (list o)))
          (set! _m0 #t))
) (if (string? orders) (string->list orders) orders))
      (if _m0
          (for-each (lambda (o)
            (set! _tmp (append _tmp (list c)))
          ) _ms0)
          (let ((o '()))
            (set! _tmp (append _tmp (list c)))
          ))
    )
  ) (if (string? customers) (string->list customers) customers))
  (let ((_res '()))
    (for-each (lambda (g)
      (set! _res (append _res (list (list (cons 'name (map-get g 'key)) (cons 'count (_count (let ((_res '()))
  (for-each (lambda (r)
    (when (map-get r 'o)
      (set! _res (append _res (list r)))
    )
  ) (if (string? g) (string->list g) g))
  _res)))))))
    ) (_group_by _tmp (lambda (c) (map-get c 'name))))
    _res)))
(begin (display "--- Group Left Join ---") (newline))
(let loop ((s_idx 0))
  (if (< s_idx (length stats))
    (begin
      (let ((s (list-ref stats s_idx)))
        (begin (display (map-get s 'name)) (display " ") (display "orders:") (display " ") (display (map-get s 'count)) (newline))
      )
      (loop (+ s_idx 1))
    )
  '()
  )
)
