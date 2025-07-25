; Generated by Mochi compiler v0.10.25 on 2025-07-13T13:08:35Z
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
(import (chibi string))
(import (scheme base))

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

(define (test_Q17_finds_US_character_name_movie_with_actor_starting_with_B)
  (when (not (equal? result (list (list (cons 'member_in_charnamed_american_movie "Bob Smith") (cons 'a1 "Bob Smith"))))) (error "expect failed"))
)

(define cast_info (list (list (cons 'movie_id 1) (cons 'person_id 1)) (list (cons 'movie_id 2) (cons 'person_id 2))))
(define company_name (list (list (cons 'id 1) (cons 'country_code "[us]")) (list (cons 'id 2) (cons 'country_code "[ca]"))))
(define keyword (list (list (cons 'id 10) (cons 'keyword "character-name-in-title")) (list (cons 'id 20) (cons 'keyword "other"))))
(define movie_companies (list (list (cons 'movie_id 1) (cons 'company_id 1)) (list (cons 'movie_id 2) (cons 'company_id 2))))
(define movie_keyword (list (list (cons 'movie_id 1) (cons 'keyword_id 10)) (list (cons 'movie_id 2) (cons 'keyword_id 20))))
(define name (list (list (cons 'id 1) (cons 'name "Bob Smith")) (list (cons 'id 2) (cons 'name "Alice Jones"))))
(define title (list (list (cons 'id 1) (cons 'title "Bob's Journey")) (list (cons 'id 2) (cons 'title "Foreign Film"))))
(define matches (let ((_res '()))
  (for-each (lambda (n)
    (for-each (lambda (ci)
      (when (equal? (map-get ci 'person_id) (map-get n 'id))
        (for-each (lambda (t)
          (when (equal? (map-get t 'id) (map-get ci 'movie_id))
            (for-each (lambda (mk)
              (when (equal? (map-get mk 'movie_id) (map-get t 'id))
                (for-each (lambda (k)
                  (when (equal? (map-get k 'id) (map-get mk 'keyword_id))
                    (for-each (lambda (mc)
                      (when (equal? (map-get mc 'movie_id) (map-get t 'id))
                        (for-each (lambda (cn)
                          (when (equal? (map-get cn 'id) (map-get mc 'company_id))
                            (when (and (and (and (and (and (equal? (map-get cn 'country_code) "[us]") (equal? (map-get k 'keyword) "character-name-in-title")) (if (string-prefix? "B" (map-get n 'name)) #t #f)) (equal? (map-get ci 'movie_id) (map-get mk 'movie_id))) (equal? (map-get ci 'movie_id) (map-get mc 'movie_id))) (equal? (map-get mc 'movie_id) (map-get mk 'movie_id)))
                              (set! _res (append _res (list (map-get n 'name))))
                            )
                          )) (if (string? company_name) (string->list company_name) company_name))
                      )) (if (string? movie_companies) (string->list movie_companies) movie_companies))
                  )) (if (string? keyword) (string->list keyword) keyword))
              )) (if (string? movie_keyword) (string->list movie_keyword) movie_keyword))
          )) (if (string? title) (string->list title) title))
      )) (if (string? cast_info) (string->list cast_info) cast_info))
  ) (if (string? name) (string->list name) name))
  _res))
(define result (list (list (cons 'member_in_charnamed_american_movie (_min matches)) (cons 'a1 (_min matches)))))
(_json result)
(run-test "Q17 finds US character-name movie with actor starting with B" test_Q17_finds_US_character_name_movie_with_actor_starting_with_B)
(when (> failures 0) (display "\n[FAIL] ") (display failures) (display " test(s) failed.\n"))
