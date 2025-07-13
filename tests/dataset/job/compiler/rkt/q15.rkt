#lang racket
(require json)
(define (_date_number s)
  (let ([parts (string-split s "-")])
    (if (= (length parts) 3)
        (+ (* (string->number (list-ref parts 0)) 10000)
           (* (string->number (list-ref parts 1)) 100)
           (string->number (list-ref parts 2)))
        #f)))

(define (_to_string v) (format "~a" v))

(define (_lt a b)
  (cond
    [(and (number? a) (number? b)) (< a b)]
    [(and (string? a) (string? b))
     (let ([da (_date_number a)]
           [db (_date_number b)])
       (if (and da db)
           (< da db)
           (string<? a b)))]
    [(and (list? a) (list? b))
     (cond [(null? a) (not (null? b))]
           [(null? b) #f]
           [else (let ([ka (car a)] [kb (car b)])
                   (if (equal? ka kb)
                       (_lt (cdr a) (cdr b))
                       (_lt ka kb)))])]
    [else (string<? (_to_string a) (_to_string b))]))

(define (_gt a b) (_lt b a))
(define (_le a b) (or (_lt a b) (equal? a b)))
(define (_ge a b) (or (_gt a b) (equal? a b)))

(define (_min v)
  (let* ([lst (cond [(and (hash? v) (hash-has-key? v 'items)) (hash-ref v 'items)]
                    [(list? v) v]
                    [else '()])]
         [m 0])
    (when (not (null? lst))
      (set! m (car lst))
      (for ([n (cdr lst)])
        (when (_lt n m) (set! m n))))
    m))

(define (_max v)
  (let* ([lst (cond [(and (hash? v) (hash-has-key? v 'items)) (hash-ref v 'items)]
                    [(list? v) v]
                    [else '()])]
         [m 0])
    (when (not (null? lst))
      (set! m (car lst))
      (for ([n (cdr lst)])
        (when (_gt n m) (set! m n))))
    m))

(define aka_title (list (hash 'movie_id 1) (hash 'movie_id 2)))
(define company_name (list (hash 'id 1 'country_code "[us]") (hash 'id 2 'country_code "[gb]")))
(define company_type (list (hash 'id 10) (hash 'id 20)))
(define info_type (list (hash 'id 5 'info "release dates") (hash 'id 6 'info "other")))
(define keyword (list (hash 'id 100) (hash 'id 200)))
(define movie_companies (list (hash 'movie_id 1 'company_id 1 'company_type_id 10 'note "release (2005) (worldwide)") (hash 'movie_id 2 'company_id 2 'company_type_id 20 'note "release (1999) (worldwide)")))
(define movie_info (list (hash 'movie_id 1 'info_type_id 5 'note "internet" 'info "USA: March 2005") (hash 'movie_id 2 'info_type_id 5 'note "theater" 'info "USA: May 1999")))
(define movie_keyword (list (hash 'movie_id 1 'keyword_id 100) (hash 'movie_id 2 'keyword_id 200)))
(define title (list (hash 'id 1 'title "Example Movie" 'production_year 2005) (hash 'id 2 'title "Old Movie" 'production_year 1999)))
(define rows (for*/list ([t title] [at aka_title] [mi movie_info] [mk movie_keyword] [mc movie_companies] [k keyword] [it1 info_type] [cn company_name] [ct company_type] #:when (and (equal? (hash-ref at 'movie_id) (hash-ref t 'id)) (equal? (hash-ref mi 'movie_id) (hash-ref t 'id)) (equal? (hash-ref mk 'movie_id) (hash-ref t 'id)) (equal? (hash-ref mc 'movie_id) (hash-ref t 'id)) (equal? (hash-ref k 'id) (hash-ref mk 'keyword_id)) (equal? (hash-ref it1 'id) (hash-ref mi 'info_type_id)) (equal? (hash-ref cn 'id) (hash-ref mc 'company_id)) (equal? (hash-ref ct 'id) (hash-ref mc 'company_type_id)) (and (and (and (and (and (and (and (string=? (hash-ref cn 'country_code) "[us]") (string=? (hash-ref it1 'info) "release dates")) (regexp-match? (regexp (regexp-quote "200")) (hash-ref mc 'note))) (regexp-match? (regexp (regexp-quote "worldwide")) (hash-ref mc 'note))) (regexp-match? (regexp (regexp-quote "internet")) (hash-ref mi 'note))) (regexp-match? (regexp (regexp-quote "USA:")) (hash-ref mi 'info))) (regexp-match? (regexp (regexp-quote "200")) (hash-ref mi 'info))) (> (hash-ref t 'production_year) 2000)))) (hash 'release_date (hash-ref mi 'info) 'internet_movie (hash-ref t 'title))))
(define result (list (hash 'release_date (_min (for*/list ([r rows]) (hash-ref r 'release_date))) 'internet_movie (_min (for*/list ([r rows]) (hash-ref r 'internet_movie))))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'release_date "USA: March 2005" 'internet_movie "Example Movie"))) (displayln "ok"))
