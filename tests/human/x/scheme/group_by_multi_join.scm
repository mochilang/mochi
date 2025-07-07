(define (find pred lst) (cond ((null? lst) #f) ((pred (car lst)) (car lst)) (else (find pred (cdr lst)))))
(define nations
  (list
    (list (cons 'id 1) (cons 'name "A"))
    (list (cons 'id 2) (cons 'name "B"))))

(define suppliers
  (list
    (list (cons 'id 1) (cons 'nation 1))
    (list (cons 'id 2) (cons 'nation 2))))

(define partsupp
  (list
    (list (cons 'part 100) (cons 'supplier 1) (cons 'cost 10.0) (cons 'qty 2))
    (list (cons 'part 100) (cons 'supplier 2) (cons 'cost 20.0) (cons 'qty 1))
    (list (cons 'part 200) (cons 'supplier 1) (cons 'cost 5.0) (cons 'qty 3))))

(define filtered '())
(for-each (lambda (ps)
            (let* ((s (find (lambda (x) (= (cdr (assoc 'id x)) (cdr (assoc 'supplier ps)))) suppliers))
                   (n (find (lambda (x) (= (cdr (assoc 'id x)) (cdr (assoc 'nation s)))) nations)))
              (when (and s n (string=? (cdr (assoc 'name n)) "A"))
                (set! filtered (cons (list (cons 'part (cdr (assoc 'part ps)))
                                            (cons 'value (* (cdr (assoc 'cost ps))
                                                            (cdr (assoc 'qty ps)))))
                                     filtered)))))
          partsupp)

(define totals '())
(for-each (lambda (x)
            (let* ((part (cdr (assoc 'part x)))
                   (entry (assoc part totals))
                   (val (cdr (assoc 'value x))))
              (if entry
                  (set-cdr! entry (+ (cdr entry) val))
                  (set! totals (cons (cons part val) totals))))
          )
          filtered)

(define result
  (map (lambda (e)
         (list (cons 'part (car e)) (cons 'total (cdr e))))
       totals))

(write result)
(newline)
