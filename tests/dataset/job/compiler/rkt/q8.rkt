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

(define aka_name (list (hash 'person_id 1 'name "Y. S.")))
(define cast_info (list (hash 'person_id 1 'movie_id 10 'note "(voice: English version)" 'role_id 1000)))
(define company_name (list (hash 'id 50 'country_code "[jp]")))
(define movie_companies (list (hash 'movie_id 10 'company_id 50 'note "Studio (Japan)")))
(define name (list (hash 'id 1 'name "Yoko Ono") (hash 'id 2 'name "Yuichi")))
(define role_type (list (hash 'id 1000 'role "actress")))
(define title (list (hash 'id 10 'title "Dubbed Film")))
(define eligible (for*/list ([an1 aka_name] [n1 name] [ci cast_info] [t title] [mc movie_companies] [cn company_name] [rt role_type] #:when (and (equal? (hash-ref n1 'id) (hash-ref an1 'person_id)) (equal? (hash-ref ci 'person_id) (hash-ref an1 'person_id)) (equal? (hash-ref t 'id) (hash-ref ci 'movie_id)) (equal? (hash-ref mc 'movie_id) (hash-ref ci 'movie_id)) (equal? (hash-ref cn 'id) (hash-ref mc 'company_id)) (equal? (hash-ref rt 'id) (hash-ref ci 'role_id)) (and (and (and (and (and (and (string=? (hash-ref ci 'note) "(voice: English version)") (string=? (hash-ref cn 'country_code) "[jp]")) (regexp-match? (regexp "(Japan)") (hash-ref mc 'note))) (not (regexp-match? (regexp "(USA)") (hash-ref mc 'note)))) (regexp-match? (regexp "Yo") (hash-ref n1 'name))) (not (regexp-match? (regexp "Yu") (hash-ref n1 'name)))) (string=? (hash-ref rt 'role) "actress")))) (hash 'pseudonym (hash-ref an1 'name) 'movie_title (hash-ref t 'title))))
(define result (list (hash 'actress_pseudonym (_min (for*/list ([x eligible]) (hash-ref x 'pseudonym))) 'japanese_movie_dubbed (_min (for*/list ([x eligible]) (hash-ref x 'movie_title))))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'actress_pseudonym "Y. S." 'japanese_movie_dubbed "Dubbed Film"))) (displayln "ok"))
