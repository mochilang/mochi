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

(define aka_name (list (hash 'person_id 1 'name "A. Stone") (hash 'person_id 2 'name "J. Doe")))
(define char_name (list (hash 'id 1 'name "Protagonist") (hash 'id 2 'name "Extra")))
(define cast_info (list (hash 'movie_id 1 'person_role_id 1 'person_id 1 'role_id 1 'note "(voice)") (hash 'movie_id 2 'person_role_id 2 'person_id 2 'role_id 2 'note "Cameo")))
(define company_name (list (hash 'id 10 'country_code "[us]") (hash 'id 20 'country_code "[gb]")))
(define info_type (list (hash 'id 100 'info "release dates")))
(define movie_companies (list (hash 'movie_id 1 'company_id 10 'note "Studio (USA)") (hash 'movie_id 2 'company_id 20 'note "Other (worldwide)")))
(define movie_info (list (hash 'movie_id 1 'info_type_id 100 'info "USA: June 2006") (hash 'movie_id 2 'info_type_id 100 'info "UK: 1999")))
(define name (list (hash 'id 1 'name "Angela Stone" 'gender "f") (hash 'id 2 'name "Bob Angstrom" 'gender "m")))
(define role_type (list (hash 'id 1 'role "actress") (hash 'id 2 'role "actor")))
(define title (list (hash 'id 1 'title "Voiced Movie" 'production_year 2006) (hash 'id 2 'title "Other Movie" 'production_year 2010)))
(define matches (for*/list ([an aka_name] [n name] [ci cast_info] [chn char_name] [rt role_type] [t title] [mc movie_companies] [cn company_name] [mi movie_info] [it info_type] #:when (and (equal? (hash-ref n 'id) (hash-ref an 'person_id)) (equal? (hash-ref ci 'person_id) (hash-ref an 'person_id)) (equal? (hash-ref chn 'id) (hash-ref ci 'person_role_id)) (equal? (hash-ref rt 'id) (hash-ref ci 'role_id)) (equal? (hash-ref t 'id) (hash-ref ci 'movie_id)) (equal? (hash-ref mc 'movie_id) (hash-ref t 'id)) (equal? (hash-ref cn 'id) (hash-ref mc 'company_id)) (equal? (hash-ref mi 'movie_id) (hash-ref t 'id)) (equal? (hash-ref it 'id) (hash-ref mi 'info_type_id)) (and (and (and (and (and (and (and (and (and (and (and (cond [(string? '("(voice)" "(voice: Japanese version)" "(voice) (uncredited)" "(voice: English version)")) (regexp-match? (regexp (hash-ref ci 'note)) '("(voice)" "(voice: Japanese version)" "(voice) (uncredited)" "(voice: English version)"))] [(hash? '("(voice)" "(voice: Japanese version)" "(voice) (uncredited)" "(voice: English version)")) (hash-has-key? '("(voice)" "(voice: Japanese version)" "(voice) (uncredited)" "(voice: English version)") (hash-ref ci 'note))] [else (member (hash-ref ci 'note) '("(voice)" "(voice: Japanese version)" "(voice) (uncredited)" "(voice: English version)"))]) (string=? (hash-ref cn 'country_code) "[us]")) (string=? (hash-ref it 'info) "release dates")) (not (equal? (hash-ref mc 'note) '()))) (or (regexp-match? (regexp (regexp-quote "(USA)")) (hash-ref mc 'note)) (regexp-match? (regexp (regexp-quote "(worldwide)")) (hash-ref mc 'note)))) (not (equal? (hash-ref mi 'info) '()))) (or (and (regexp-match? (regexp (regexp-quote "Japan:")) (hash-ref mi 'info)) (regexp-match? (regexp (regexp-quote "200")) (hash-ref mi 'info))) (and (regexp-match? (regexp (regexp-quote "USA:")) (hash-ref mi 'info)) (regexp-match? (regexp (regexp-quote "200")) (hash-ref mi 'info))))) (string=? (hash-ref n 'gender) "f")) (regexp-match? (regexp (regexp-quote "Ang")) (hash-ref n 'name))) (string=? (hash-ref rt 'role) "actress")) (>= (hash-ref t 'production_year) 2005)) (<= (hash-ref t 'production_year) 2009)))) (hash 'actress (hash-ref n 'name) 'movie (hash-ref t 'title))))
(define result (list (hash 'voicing_actress (_min (for*/list ([r matches]) (hash-ref r 'actress))) 'voiced_movie (_min (for*/list ([r matches]) (hash-ref r 'movie))))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'voicing_actress "Angela Stone" 'voiced_movie "Voiced Movie"))) (displayln "ok"))
