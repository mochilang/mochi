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

(define cast_info (list (hash 'movie_id 1 'person_id 1) (hash 'movie_id 2 'person_id 2)))
(define company_name (list (hash 'id 1 'country_code "[us]") (hash 'id 2 'country_code "[ca]")))
(define keyword (list (hash 'id 10 'keyword "character-name-in-title") (hash 'id 20 'keyword "other")))
(define movie_companies (list (hash 'movie_id 1 'company_id 1) (hash 'movie_id 2 'company_id 2)))
(define movie_keyword (list (hash 'movie_id 1 'keyword_id 10) (hash 'movie_id 2 'keyword_id 20)))
(define name (list (hash 'id 1 'name "Bob Smith") (hash 'id 2 'name "Alice Jones")))
(define title (list (hash 'id 1 'title "Bob's Journey") (hash 'id 2 'title "Foreign Film")))
(define matches (for*/list ([n name] [ci cast_info] [t title] [mk movie_keyword] [k keyword] [mc movie_companies] [cn company_name] #:when (and (equal? (hash-ref ci 'person_id) (hash-ref n 'id)) (equal? (hash-ref t 'id) (hash-ref ci 'movie_id)) (equal? (hash-ref mk 'movie_id) (hash-ref t 'id)) (equal? (hash-ref k 'id) (hash-ref mk 'keyword_id)) (equal? (hash-ref mc 'movie_id) (hash-ref t 'id)) (equal? (hash-ref cn 'id) (hash-ref mc 'company_id)) (and (and (and (and (and (string=? (hash-ref cn 'country_code) "[us]") (string=? (hash-ref k 'keyword) "character-name-in-title")) (regexp-match? (regexp (string-append "^" (regexp-quote "B"))) (hash-ref n 'name))) (equal? (hash-ref ci 'movie_id) (hash-ref mk 'movie_id))) (equal? (hash-ref ci 'movie_id) (hash-ref mc 'movie_id))) (equal? (hash-ref mc 'movie_id) (hash-ref mk 'movie_id))))) (hash-ref n 'name)))
(define result (list (hash 'member_in_charnamed_american_movie (_min matches) 'a1 (_min matches))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'member_in_charnamed_american_movie "Bob Smith" 'a1 "Bob Smith"))) (displayln "ok"))
