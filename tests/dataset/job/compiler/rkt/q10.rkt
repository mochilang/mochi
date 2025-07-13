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

(define char_name (list (hash 'id 1 'name "Ivan") (hash 'id 2 'name "Alex")))
(define cast_info (list (hash 'movie_id 10 'person_role_id 1 'role_id 1 'note "Soldier (voice) (uncredited)") (hash 'movie_id 11 'person_role_id 2 'role_id 1 'note "(voice)")))
(define company_name (list (hash 'id 1 'country_code "[ru]") (hash 'id 2 'country_code "[us]")))
(define company_type (list (hash 'id 1) (hash 'id 2)))
(define movie_companies (list (hash 'movie_id 10 'company_id 1 'company_type_id 1) (hash 'movie_id 11 'company_id 2 'company_type_id 1)))
(define role_type (list (hash 'id 1 'role "actor") (hash 'id 2 'role "director")))
(define title (list (hash 'id 10 'title "Vodka Dreams" 'production_year 2006) (hash 'id 11 'title "Other Film" 'production_year 2004)))
(define matches (for*/list ([chn char_name] [ci cast_info] [rt role_type] [t title] [mc movie_companies] [cn company_name] [ct company_type] #:when (and (equal? (hash-ref chn 'id) (hash-ref ci 'person_role_id)) (equal? (hash-ref rt 'id) (hash-ref ci 'role_id)) (equal? (hash-ref t 'id) (hash-ref ci 'movie_id)) (equal? (hash-ref mc 'movie_id) (hash-ref t 'id)) (equal? (hash-ref cn 'id) (hash-ref mc 'company_id)) (equal? (hash-ref ct 'id) (hash-ref mc 'company_type_id)) (and (and (and (and (regexp-match? (regexp "(voice)") (hash-ref ci 'note)) (regexp-match? (regexp "(uncredited)") (hash-ref ci 'note))) (string=? (hash-ref cn 'country_code) "[ru]")) (string=? (hash-ref rt 'role) "actor")) (> (hash-ref t 'production_year) 2005)))) (hash 'character (hash-ref chn 'name) 'movie (hash-ref t 'title))))
(define result (list (hash 'uncredited_voiced_character (_min (for*/list ([x matches]) (hash-ref x 'character))) 'russian_movie (_min (for*/list ([x matches]) (hash-ref x 'movie))))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'uncredited_voiced_character "Ivan" 'russian_movie "Vodka Dreams"))) (displayln "ok"))
