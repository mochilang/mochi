#lang racket
(require racket/list)
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

(define company_type (list (hash 'ct_id 1 'kind "production companies") (hash 'ct_id 2 'kind "other")))
(define info_type (list (hash 'it_id 10 'info "languages")))
(define title (list (hash 't_id 100 'title "B Movie" 'production_year 2010) (hash 't_id 200 'title "A Film" 'production_year 2012) (hash 't_id 300 'title "Old Movie" 'production_year 2000)))
(define movie_companies (list (hash 'movie_id 100 'company_type_id 1 'note "ACME (France) (theatrical)") (hash 'movie_id 200 'company_type_id 1 'note "ACME (France) (theatrical)") (hash 'movie_id 300 'company_type_id 1 'note "ACME (France) (theatrical)")))
(define movie_info (list (hash 'movie_id 100 'info "German" 'info_type_id 10) (hash 'movie_id 200 'info "Swedish" 'info_type_id 10) (hash 'movie_id 300 'info "German" 'info_type_id 10)))
(define candidate_titles (for*/list ([ct company_type] [mc movie_companies] [mi movie_info] [it info_type] [t title] #:when (and (equal? (hash-ref mc 'company_type_id) (hash-ref ct 'ct_id)) (equal? (hash-ref mi 'movie_id) (hash-ref mc 'movie_id)) (equal? (hash-ref it 'it_id) (hash-ref mi 'info_type_id)) (equal? (hash-ref t 't_id) (hash-ref mc 'movie_id)) (and (and (and (and (string=? (hash-ref ct 'kind) "production companies") (cond [(string? (hash-ref mc 'note)) (regexp-match? (regexp "(theatrical)") (hash-ref mc 'note))] [(hash? (hash-ref mc 'note)) (hash-has-key? (hash-ref mc 'note) "(theatrical)")] [else (member "(theatrical)" (hash-ref mc 'note))])) (cond [(string? (hash-ref mc 'note)) (regexp-match? (regexp "(France)") (hash-ref mc 'note))] [(hash? (hash-ref mc 'note)) (hash-has-key? (hash-ref mc 'note) "(France)")] [else (member "(France)" (hash-ref mc 'note))])) (> (hash-ref t 'production_year) 2005)) (cond [(string? '("Sweden" "Norway" "Germany" "Denmark" "Swedish" "Denish" "Norwegian" "German")) (regexp-match? (regexp (hash-ref mi 'info)) '("Sweden" "Norway" "Germany" "Denmark" "Swedish" "Denish" "Norwegian" "German"))] [(hash? '("Sweden" "Norway" "Germany" "Denmark" "Swedish" "Denish" "Norwegian" "German")) (hash-has-key? '("Sweden" "Norway" "Germany" "Denmark" "Swedish" "Denish" "Norwegian" "German") (hash-ref mi 'info))] [else (member (hash-ref mi 'info) '("Sweden" "Norway" "Germany" "Denmark" "Swedish" "Denish" "Norwegian" "German"))])))) (hash-ref t 'title)))
(define result (list (hash 'typical_european_movie (_min candidate_titles))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'typical_european_movie "A Film"))) (displayln "ok"))
