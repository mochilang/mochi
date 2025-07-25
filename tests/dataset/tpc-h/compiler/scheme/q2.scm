; Generated by Mochi compiler v0.10.25 on 2025-07-13T18:03:49Z
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
(import (srfi 1) (srfi 95) (chibi json) (chibi io) (chibi process) (chibi) (chibi string))

(define (_fmt . parts)
  (apply string-append (map _to_string parts)))

(define (_to_string v)
  (call-with-output-string (lambda (p) (write v p))))

(define (_yaml_value v)
  (let ((n (string->number v)))
    (if n n v)))

(define (_parse_yaml text)
  (let ((rows '()) (cur '()))
    (for-each (lambda (ln)
                (when (and (>= (string-length ln) 2) (string-prefix? "- " ln))
                  (when (not (null? cur))
                    (set! rows (append rows (list cur))))
                  (set! cur '())
                  (set! ln (substring ln 2 (string-length ln))))
                (when (string-contains ln ":")
                  (let* ((p (string-split ln #\:))
                         (k (string-trim (car p)))
                         (val (string-trim (string-join (cdr p) ":"))))
                    (set! cur (append cur (list (cons k (_yaml_value val))))))))
              (string-split text #\newline))
    (when (not (null? cur))
      (set! rows (append rows (list cur))))
    rows))

(define (_fetch url opts)
  (let* ((method (if (and opts (assq 'method opts)) (cdr (assq 'method opts)) "GET"))
         (args (list "curl" "-s" "-X" method)))
    (when (and opts (assq 'headers opts))
      (for-each (lambda (p)
                  (set! args (append args (list "-H" (_fmt (car p) ": " (cdr p))))))
                (cdr (assq 'headers opts))))
    (when (and opts (assq 'query opts))
      (let* ((q (cdr (assq 'query opts)))
             (qs (string-join (map (lambda (p) (_fmt (car p) "=" (cdr p))) q) "&")))
        (set! url (string-append url (if (string-contains url "?") "&" "?") qs))))
    (when (and opts (assq 'body opts))
      (set! args (append args (list "-d" (json->string (cdr (assq 'body opts)))))))
    (when (and opts (assq 'timeout opts))
      (set! args (append args (list "--max-time" (_to_string (cdr (assq 'timeout opts)))))))
    (set! args (append args (list url)))
    (let* ((p (open-input-pipe (string-join args " ")))
           (txt (port->string p)))
      (close-input-port p)
      (string->json txt))))

(define (_load path opts)
  (let* ((fmt (if (and opts (assq 'format opts)) (cdr (assq 'format opts)) "json"))
         (in (if (or (not path) (string=? path "") (string=? path "-"))
                 (current-input-port)
                 (open-input-file path)))
         (text (port->string in)))
    (when (not (eq? in (current-input-port)))
      (close-input-port in))
    (cond ((string=? fmt "jsonl")
           (map string->json
                (filter (lambda (l) (not (string=? l "")))
                        (string-split text #\newline))))
          ((string=? fmt "yaml")
           (_parse_yaml text))
          (else
           (let ((d (string->json text)))
             (if (list? d) d (list d)))))))

(define (_save rows path opts)
  (let* ((fmt (if (and opts (assq 'format opts)) (cdr (assq 'format opts)) "json"))
         (out (if (or (not path) (string=? path "") (string=? path "-"))
                  (current-output-port)
                  (open-output-file path))))
  (cond ((string=? fmt "jsonl")
           (for-each (lambda (r) (write-string (json->string r) out) (newline out)) rows))
          (else
           (write-string (json->string rows) out)))
    (when (not (eq? out (current-output-port)))
      (close-output-port out))))

(define (_date_number s)
  (let ((parts (string-split s #\-)))
    (if (= (length parts) 3)
        (+ (* (string->number (list-ref parts 0)) 10000)
           (* (string->number (list-ref parts 1)) 100)
           (string->number (list-ref parts 2)))
        #f)))

(define (_lt a b)
  (cond
    ((and (number? a) (number? b)) (< a b))
    ((and (string? a) (string? b))
      (let ((da (_date_number a))
            (db (_date_number b)))
        (if (and da db)
            (< da db)
            (string<? a b))))
    ((and (pair? a) (pair? b))
      (cond
        ((null? a) (not (null? b)))
        ((null? b) #f)
        (else (let ((ka (car a)) (kb (car b)))
                (if (equal? ka kb)
                    (_lt (cdr a) (cdr b))
                    (_lt ka kb)))))
    )
    (else (string<? (_to_string a) (_to_string b)))))

(define (_le a b)
  (or (_lt a b) (equal? a b)))

(define (_gt a b)
  (_lt b a))

(define (_ge a b)
  (or (_gt a b) (equal? a b)))

(define (_sort pairs)
  (letrec ((cmp (lambda (a b) (_lt (cdr a) (cdr b))))
           (insert (lambda (x lst)
                     (cond ((null? lst) (list x))
                           ((cmp x (car lst)) (cons x lst))
                           (else (cons (car lst) (insert x (cdr lst)))))))
           (loop (lambda (xs out)
                   (if (null? xs)
                       out
                       (loop (cdr xs) (insert (car xs) out))))) )
    (loop pairs '())))
(import (scheme base))

(define (_count v)
  (cond
    ((string? v) (string-length v))
    ((and (pair? v) (assq 'Items v)) (length (cdr (assq 'Items v))))
    ((list? v) (length v))
    (else 0)))

(define (_sum v)
  (let* ((lst (cond
               ((number? v) (list v))
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
                  (when (_gt n m) (set! m n)))
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
                  (when (_lt n m) (set! m n)))
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
(define (_json v)
  (cond
    ;; list of objects
    ((and (list? v) (pair? v) (pair? (car v)) (pair? (caar v)))
     (display "[")
     (let loop ((xs v) (first #t))
       (unless (null? xs)
         (unless first (display ","))
         (display (json->string (car xs)))
         (loop (cdr xs) #f)))
     (display "]"))
    ;; single object or other value
    (else
     (display (json->string v))))
  (newline))
(define failures 0)
(define (print-test-start name)
  (display "   test ") (display name) (display " ..."))
(define (print-test-pass) (display " ok") (newline))
(define (print-test-fail err) (display " fail ") (display err) (newline))
(define (run-test name thunk)
  (print-test-start name)
  (let ((ok #t))
    (with-exception-handler
      (lambda (e)
        (set! ok #f)
        (set! failures (+ failures 1))
        (print-test-fail e))
      (lambda () (thunk)))
    (when ok (print-test-pass))))

(define (test_Q2_returns_only_supplier_with_min_cost_in_Europe_for_brass_part)
  (when (not (equal? result (list (list (cons 's_acctbal 1000.0) (cons 's_name "BestSupplier") (cons 'n_name "FRANCE") (cons 'p_partkey 1000) (cons 'p_mfgr "M1") (cons 's_address "123 Rue") (cons 's_phone "123") (cons 's_comment "Fast and reliable") (cons 'ps_supplycost 10.0))))) (error "expect failed"))
)

(define region (list (list (cons 'r_regionkey 1) (cons 'r_name "EUROPE")) (list (cons 'r_regionkey 2) (cons 'r_name "ASIA"))))
(define nation (list (list (cons 'n_nationkey 10) (cons 'n_regionkey 1) (cons 'n_name "FRANCE")) (list (cons 'n_nationkey 20) (cons 'n_regionkey 2) (cons 'n_name "CHINA"))))
(define supplier (list (list (cons 's_suppkey 100) (cons 's_name "BestSupplier") (cons 's_address "123 Rue") (cons 's_nationkey 10) (cons 's_phone "123") (cons 's_acctbal 1000.0) (cons 's_comment "Fast and reliable")) (list (cons 's_suppkey 200) (cons 's_name "AltSupplier") (cons 's_address "456 Way") (cons 's_nationkey 20) (cons 's_phone "456") (cons 's_acctbal 500.0) (cons 's_comment "Slow"))))
(define part (list (list (cons 'p_partkey 1000) (cons 'p_type "LARGE BRASS") (cons 'p_size 15) (cons 'p_mfgr "M1")) (list (cons 'p_partkey 2000) (cons 'p_type "SMALL COPPER") (cons 'p_size 15) (cons 'p_mfgr "M2"))))
(define partsupp (list (list (cons 'ps_partkey 1000) (cons 'ps_suppkey 100) (cons 'ps_supplycost 10.0)) (list (cons 'ps_partkey 1000) (cons 'ps_suppkey 200) (cons 'ps_supplycost 15.0))))
(define europe_nations (let ((_res '()))
  (for-each (lambda (r)
    (for-each (lambda (n)
      (when (equal? (map-get n 'n_regionkey) (map-get r 'r_regionkey))
        (when (equal? (map-get r 'r_name) "EUROPE")
          (set! _res (append _res (list n)))
        )
      )) (if (string? nation) (string->list nation) nation))
  ) (if (string? region) (string->list region) region))
  _res))
(define europe_suppliers (let ((_res '()))
  (for-each (lambda (s)
    (for-each (lambda (n)
      (when (equal? (map-get s 's_nationkey) (map-get n 'n_nationkey))
        (set! _res (append _res (list (list (cons 's s) (cons 'n n)))))
      )) (if (string? europe_nations) (string->list europe_nations) europe_nations))
  ) (if (string? supplier) (string->list supplier) supplier))
  _res))
(define target_parts (let ((_res '()))
  (for-each (lambda (p)
    (when (and (equal? (map-get p 'p_size) 15) (equal? (map-get p 'p_type) "LARGE BRASS"))
      (set! _res (append _res (list p)))
    )
  ) (if (string? part) (string->list part) part))
  _res))
(define target_partsupp (let ((_res '()))
  (for-each (lambda (ps)
    (for-each (lambda (p)
      (when (equal? (map-get ps 'ps_partkey) (map-get p 'p_partkey))
        (for-each (lambda (s)
          (when (equal? (map-get ps 'ps_suppkey) (map-get (map-get s 's) 's_suppkey))
            (set! _res (append _res (list (list (cons 's_acctbal (map-get (map-get s 's) 's_acctbal)) (cons 's_name (map-get (map-get s 's) 's_name)) (cons 'n_name (map-get (map-get s 'n) 'n_name)) (cons 'p_partkey (map-get p 'p_partkey)) (cons 'p_mfgr (map-get p 'p_mfgr)) (cons 's_address (map-get (map-get s 's) 's_address)) (cons 's_phone (map-get (map-get s 's) 's_phone)) (cons 's_comment (map-get (map-get s 's) 's_comment)) (cons 'ps_supplycost (map-get ps 'ps_supplycost))))))
          )) (if (string? europe_suppliers) (string->list europe_suppliers) europe_suppliers))
      )) (if (string? target_parts) (string->list target_parts) target_parts))
  ) (if (string? partsupp) (string->list partsupp) partsupp))
  _res))
(define costs (let ((_res '()))
  (for-each (lambda (x)
    (set! _res (append _res (list (map-get x 'ps_supplycost))))
  ) (if (string? target_partsupp) (string->list target_partsupp) target_partsupp))
  _res))
(define min_cost (_min costs))
(define result (let ((_res '()) (_tmp '()))
  (for-each (lambda (x)
    (when (equal? (map-get x 'ps_supplycost) min_cost)
      (set! _tmp (append _tmp (list (cons x (- (map-get x 's_acctbal))))))
    )
  ) (if (string? target_partsupp) (string->list target_partsupp) target_partsupp))
  (set! _res (_sort _tmp))
  (set! _res (map car _res))
  _res))
(_json result)
(run-test "Q2 returns only supplier with min cost in Europe for brass part" test_Q2_returns_only_supplier_with_min_cost_in_Europe_for_brass_part)
(when (> failures 0) (display "\n[FAIL] ") (display failures) (display " test(s) failed.\n"))
