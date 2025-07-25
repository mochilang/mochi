; Generated by Mochi compiler v0.10.25 on 2025-07-13T13:08:28Z
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

(define (test_Q13_finds_earliest_German_movie_info)
  (when (not (equal? result (list (cons 'release_date "1997-05-10") (cons 'rating "6.0") (cons 'german_movie "Alpha")))) (error "expect failed"))
)

(define company_name (list (list (cons 'id 1) (cons 'country_code "[de]")) (list (cons 'id 2) (cons 'country_code "[us]"))))
(define company_type (list (list (cons 'id 1) (cons 'kind "production companies")) (list (cons 'id 2) (cons 'kind "distributors"))))
(define info_type (list (list (cons 'id 1) (cons 'info "rating")) (list (cons 'id 2) (cons 'info "release dates"))))
(define kind_type (list (list (cons 'id 1) (cons 'kind "movie")) (list (cons 'id 2) (cons 'kind "video"))))
(define title (list (list (cons 'id 10) (cons 'kind_id 1) (cons 'title "Alpha")) (list (cons 'id 20) (cons 'kind_id 1) (cons 'title "Beta")) (list (cons 'id 30) (cons 'kind_id 2) (cons 'title "Gamma"))))
(define movie_companies (list (list (cons 'movie_id 10) (cons 'company_id 1) (cons 'company_type_id 1)) (list (cons 'movie_id 20) (cons 'company_id 1) (cons 'company_type_id 1)) (list (cons 'movie_id 30) (cons 'company_id 2) (cons 'company_type_id 1))))
(define movie_info (list (list (cons 'movie_id 10) (cons 'info_type_id 2) (cons 'info "1997-05-10")) (list (cons 'movie_id 20) (cons 'info_type_id 2) (cons 'info "1998-03-20")) (list (cons 'movie_id 30) (cons 'info_type_id 2) (cons 'info "1999-07-30"))))
(define movie_info_idx (list (list (cons 'movie_id 10) (cons 'info_type_id 1) (cons 'info "6.0")) (list (cons 'movie_id 20) (cons 'info_type_id 1) (cons 'info "7.5")) (list (cons 'movie_id 30) (cons 'info_type_id 1) (cons 'info "5.5"))))
(define candidates (let ((_res '()))
  (for-each (lambda (cn)
    (for-each (lambda (mc)
      (when (equal? (map-get mc 'company_id) (map-get cn 'id))
        (for-each (lambda (ct)
          (when (equal? (map-get ct 'id) (map-get mc 'company_type_id))
            (for-each (lambda (t)
              (when (equal? (map-get t 'id) (map-get mc 'movie_id))
                (for-each (lambda (kt)
                  (when (equal? (map-get kt 'id) (map-get t 'kind_id))
                    (for-each (lambda (mi)
                      (when (equal? (map-get mi 'movie_id) (map-get t 'id))
                        (for-each (lambda (it2)
                          (when (equal? (map-get it2 'id) (map-get mi 'info_type_id))
                            (for-each (lambda (miidx)
                              (when (equal? (map-get miidx 'movie_id) (map-get t 'id))
                                (for-each (lambda (it)
                                  (when (equal? (map-get it 'id) (map-get miidx 'info_type_id))
                                    (when (and (and (and (and (equal? (map-get cn 'country_code) "[de]") (equal? (map-get ct 'kind) "production companies")) (equal? (map-get it 'info) "rating")) (equal? (map-get it2 'info) "release dates")) (equal? (map-get kt 'kind) "movie"))
                                      (set! _res (append _res (list (list (cons 'release_date (map-get mi 'info)) (cons 'rating (map-get miidx 'info)) (cons 'german_movie (map-get t 'title))))))
                                    )
                                  )) (if (string? info_type) (string->list info_type) info_type))
                              )) (if (string? movie_info_idx) (string->list movie_info_idx) movie_info_idx))
                          )) (if (string? info_type) (string->list info_type) info_type))
                      )) (if (string? movie_info) (string->list movie_info) movie_info))
                  )) (if (string? kind_type) (string->list kind_type) kind_type))
              )) (if (string? title) (string->list title) title))
          )) (if (string? company_type) (string->list company_type) company_type))
      )) (if (string? movie_companies) (string->list movie_companies) movie_companies))
  ) (if (string? company_name) (string->list company_name) company_name))
  _res))
(define result (list (cons 'release_date (list-ref (let ((_res '()) (_tmp '()))
  (for-each (lambda (x)
    (set! _tmp (append _tmp (list (cons (map-get x 'release_date) (map-get x 'release_date)))))
  ) (if (string? candidates) (string->list candidates) candidates))
  (set! _res (_sort _tmp))
  (set! _res (map car _res))
  _res) 0)) (cons 'rating (list-ref (let ((_res '()) (_tmp '()))
  (for-each (lambda (x)
    (set! _tmp (append _tmp (list (cons (map-get x 'rating) (map-get x 'rating)))))
  ) (if (string? candidates) (string->list candidates) candidates))
  (set! _res (_sort _tmp))
  (set! _res (map car _res))
  _res) 0)) (cons 'german_movie (list-ref (let ((_res '()) (_tmp '()))
  (for-each (lambda (x)
    (set! _tmp (append _tmp (list (cons (map-get x 'german_movie) (map-get x 'german_movie)))))
  ) (if (string? candidates) (string->list candidates) candidates))
  (set! _res (_sort _tmp))
  (set! _res (map car _res))
  _res) 0))))
(_json result)
(run-test "Q13 finds earliest German movie info" test_Q13_finds_earliest_German_movie_info)
(when (> failures 0) (display "\n[FAIL] ") (display failures) (display " test(s) failed.\n"))
