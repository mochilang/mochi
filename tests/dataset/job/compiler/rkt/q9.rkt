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

(define aka_name (list (hash 'person_id 1 'name "A. N. G.") (hash 'person_id 2 'name "J. D.")))
(define char_name (list (hash 'id 10 'name "Angel") (hash 'id 20 'name "Devil")))
(define cast_info (list (hash 'person_id 1 'person_role_id 10 'movie_id 100 'role_id 1000 'note "(voice)") (hash 'person_id 2 'person_role_id 20 'movie_id 200 'role_id 1000 'note "(voice)")))
(define company_name (list (hash 'id 100 'country_code "[us]") (hash 'id 200 'country_code "[gb]")))
(define movie_companies (list (hash 'movie_id 100 'company_id 100 'note "ACME Studios (USA)") (hash 'movie_id 200 'company_id 200 'note "Maple Films")))
(define name (list (hash 'id 1 'name "Angela Smith" 'gender "f") (hash 'id 2 'name "John Doe" 'gender "m")))
(define role_type (list (hash 'id 1000 'role "actress") (hash 'id 2000 'role "actor")))
(define title (list (hash 'id 100 'title "Famous Film" 'production_year 2010) (hash 'id 200 'title "Old Movie" 'production_year 1999)))
(define matches (for*/list ([an aka_name] [n name] [ci cast_info] [chn char_name] [t title] [mc movie_companies] [cn company_name] [rt role_type] #:when (and (equal? (hash-ref an 'person_id) (hash-ref n 'id)) (equal? (hash-ref ci 'person_id) (hash-ref n 'id)) (equal? (hash-ref chn 'id) (hash-ref ci 'person_role_id)) (equal? (hash-ref t 'id) (hash-ref ci 'movie_id)) (equal? (hash-ref mc 'movie_id) (hash-ref t 'id)) (equal? (hash-ref cn 'id) (hash-ref mc 'company_id)) (equal? (hash-ref rt 'id) (hash-ref ci 'role_id)) (and (and (and (and (and (and (and (cond [(string? '("(voice)" "(voice: Japanese version)" "(voice) (uncredited)" "(voice: English version)")) (regexp-match? (regexp (hash-ref ci 'note)) '("(voice)" "(voice: Japanese version)" "(voice) (uncredited)" "(voice: English version)"))] [(hash? '("(voice)" "(voice: Japanese version)" "(voice) (uncredited)" "(voice: English version)")) (hash-has-key? '("(voice)" "(voice: Japanese version)" "(voice) (uncredited)" "(voice: English version)") (hash-ref ci 'note))] [else (member (hash-ref ci 'note) '("(voice)" "(voice: Japanese version)" "(voice) (uncredited)" "(voice: English version)"))]) (string=? (hash-ref cn 'country_code) "[us]")) (or (regexp-match? (regexp "(USA)") (hash-ref mc 'note)) (regexp-match? (regexp "(worldwide)") (hash-ref mc 'note)))) (string=? (hash-ref n 'gender) "f")) (regexp-match? (regexp "Ang") (hash-ref n 'name))) (string=? (hash-ref rt 'role) "actress")) (>= (hash-ref t 'production_year) 2005)) (<= (hash-ref t 'production_year) 2015)))) (hash 'alt (hash-ref an 'name) 'character (hash-ref chn 'name) 'movie (hash-ref t 'title))))
(define result (list (hash 'alternative_name (_min (for*/list ([x matches]) (hash-ref x 'alt))) 'character_name (_min (for*/list ([x matches]) (hash-ref x 'character))) 'movie (_min (for*/list ([x matches]) (hash-ref x 'movie))))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'alternative_name "A. N. G." 'character_name "Angel" 'movie "Famous Film"))) (displayln "ok"))
