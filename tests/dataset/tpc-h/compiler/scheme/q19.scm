; Generated by Mochi compiler v0.10.25 on 2025-07-13T18:04:39Z
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

(define (test_Q19_returns_total_revenue_from_qualifying_branded_parts)
  (when (not (equal? result 2800.0)) (error "expect failed"))
)

(define part (list (list (cons 'p_partkey 1) (cons 'p_brand "Brand#12") (cons 'p_container "SM BOX") (cons 'p_size 3)) (list (cons 'p_partkey 2) (cons 'p_brand "Brand#23") (cons 'p_container "MED BOX") (cons 'p_size 5)) (list (cons 'p_partkey 3) (cons 'p_brand "Brand#34") (cons 'p_container "LG BOX") (cons 'p_size 15))))
(define lineitem (list (list (cons 'l_partkey 1) (cons 'l_quantity 5) (cons 'l_extendedprice 1000.0) (cons 'l_discount 0.1) (cons 'l_shipmode "AIR") (cons 'l_shipinstruct "DELIVER IN PERSON")) (list (cons 'l_partkey 2) (cons 'l_quantity 15) (cons 'l_extendedprice 2000.0) (cons 'l_discount 0.05) (cons 'l_shipmode "AIR REG") (cons 'l_shipinstruct "DELIVER IN PERSON")) (list (cons 'l_partkey 3) (cons 'l_quantity 35) (cons 'l_extendedprice 1500.0) (cons 'l_discount 0.0) (cons 'l_shipmode "AIR") (cons 'l_shipinstruct "DELIVER IN PERSON"))))
(define revenues (let ((_res '()))
  (for-each (lambda (l)
    (for-each (lambda (p)
      (when (equal? (map-get p 'p_partkey) (map-get l 'l_partkey))
        (when (and (and (or (or (and (and (and (equal? (map-get p 'p_brand) "Brand#12") (if (member (map-get p 'p_container) (list "SM CASE" "SM BOX" "SM PACK" "SM PKG")) #t #f)) (and (_ge (map-get l 'l_quantity) 1) (_le (map-get l 'l_quantity) 11))) (and (_ge (map-get p 'p_size) 1) (_le (map-get p 'p_size) 5))) (and (and (and (equal? (map-get p 'p_brand) "Brand#23") (if (member (map-get p 'p_container) (list "MED BAG" "MED BOX" "MED PKG" "MED PACK")) #t #f)) (and (_ge (map-get l 'l_quantity) 10) (_le (map-get l 'l_quantity) 20))) (and (_ge (map-get p 'p_size) 1) (_le (map-get p 'p_size) 10)))) (and (and (and (equal? (map-get p 'p_brand) "Brand#34") (if (member (map-get p 'p_container) (list "LG CASE" "LG BOX" "LG PACK" "LG PKG")) #t #f)) (and (_ge (map-get l 'l_quantity) 20) (_le (map-get l 'l_quantity) 30))) (and (_ge (map-get p 'p_size) 1) (_le (map-get p 'p_size) 15)))) (if (member (map-get l 'l_shipmode) (list "AIR" "AIR REG")) #t #f)) (equal? (map-get l 'l_shipinstruct) "DELIVER IN PERSON"))
          (set! _res (append _res (list (* (map-get l 'l_extendedprice) (- 1 (map-get l 'l_discount))))))
        )
      )) (if (string? part) (string->list part) part))
  ) (if (string? lineitem) (string->list lineitem) lineitem))
  _res))
(define result (_sum revenues))
(_json result)
(run-test "Q19 returns total revenue from qualifying branded parts" test_Q19_returns_total_revenue_from_qualifying_branded_parts)
(when (> failures 0) (display "\n[FAIL] ") (display failures) (display " test(s) failed.\n"))
