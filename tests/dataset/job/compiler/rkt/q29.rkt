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

(define aka_name (list (hash 'person_id 1) (hash 'person_id 2)))
(define complete_cast (list (hash 'movie_id 1 'subject_id 1 'status_id 2) (hash 'movie_id 2 'subject_id 1 'status_id 2)))
(define comp_cast_type (list (hash 'id 1 'kind "cast") (hash 'id 2 'kind "complete+verified") (hash 'id 3 'kind "other")))
(define char_name (list (hash 'id 1 'name "Queen") (hash 'id 2 'name "Princess")))
(define cast_info (list (hash 'movie_id 1 'person_id 1 'role_id 1 'person_role_id 1 'note "(voice)") (hash 'movie_id 2 'person_id 2 'role_id 1 'person_role_id 2 'note "(voice)")))
(define company_name (list (hash 'id 1 'country_code "[us]") (hash 'id 2 'country_code "[uk]")))
(define info_type (list (hash 'id 1 'info "release dates") (hash 'id 2 'info "trivia") (hash 'id 3 'info "other")))
(define keyword (list (hash 'id 1 'keyword "computer-animation") (hash 'id 2 'keyword "action")))
(define movie_companies (list (hash 'movie_id 1 'company_id 1) (hash 'movie_id 2 'company_id 2)))
(define movie_info (list (hash 'movie_id 1 'info_type_id 1 'info "USA:2004") (hash 'movie_id 2 'info_type_id 1 'info "USA:1995")))
(define movie_keyword (list (hash 'movie_id 1 'keyword_id 1) (hash 'movie_id 2 'keyword_id 2)))
(define name (list (hash 'id 1 'name "Angela Aniston" 'gender "f") (hash 'id 2 'name "Bob Brown" 'gender "m")))
(define person_info (list (hash 'person_id 1 'info_type_id 2) (hash 'person_id 2 'info_type_id 2)))
(define role_type (list (hash 'id 1 'role "actress") (hash 'id 2 'role "actor")))
(define title (list (hash 'id 1 'title "Shrek 2" 'production_year 2004) (hash 'id 2 'title "Old Film" 'production_year 1999)))
(define matches (for*/list ([an aka_name] [cc complete_cast] [cct1 comp_cast_type] [cct2 comp_cast_type] [chn char_name] [ci cast_info] [cn company_name] [it info_type] [it3 info_type] [k keyword] [mc movie_companies] [mi movie_info] [mk movie_keyword] [n name] [pi person_info] [rt role_type] [t title] #:when (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (string=? (hash-ref cct1 'kind) "cast") (string=? (hash-ref cct2 'kind) "complete+verified")) (string=? (hash-ref chn 'name) "Queen")) (or (or (string=? (hash-ref ci 'note) "(voice)") (string=? (hash-ref ci 'note) "(voice) (uncredited)")) (string=? (hash-ref ci 'note) "(voice: English version)"))) (string=? (hash-ref cn 'country_code) "[us]")) (string=? (hash-ref it 'info) "release dates")) (string=? (hash-ref it3 'info) "trivia")) (string=? (hash-ref k 'keyword) "computer-animation")) (or (regexp-match? (regexp (string-append "^" (regexp-quote "Japan:200"))) (hash-ref mi 'info)) (regexp-match? (regexp (string-append "^" (regexp-quote "USA:200"))) (hash-ref mi 'info)))) (string=? (hash-ref n 'gender) "f")) (regexp-match? (regexp (regexp-quote "An")) (hash-ref n 'name))) (string=? (hash-ref rt 'role) "actress")) (string=? (hash-ref t 'title) "Shrek 2")) (>= (hash-ref t 'production_year) 2000)) (<= (hash-ref t 'production_year) 2010)) (equal? (hash-ref t 'id) (hash-ref mi 'movie_id))) (equal? (hash-ref t 'id) (hash-ref mc 'movie_id))) (equal? (hash-ref t 'id) (hash-ref ci 'movie_id))) (equal? (hash-ref t 'id) (hash-ref mk 'movie_id))) (equal? (hash-ref t 'id) (hash-ref cc 'movie_id))) (equal? (hash-ref mc 'movie_id) (hash-ref ci 'movie_id))) (equal? (hash-ref mc 'movie_id) (hash-ref mi 'movie_id))) (equal? (hash-ref mc 'movie_id) (hash-ref mk 'movie_id))) (equal? (hash-ref mc 'movie_id) (hash-ref cc 'movie_id))) (equal? (hash-ref mi 'movie_id) (hash-ref ci 'movie_id))) (equal? (hash-ref mi 'movie_id) (hash-ref mk 'movie_id))) (equal? (hash-ref mi 'movie_id) (hash-ref cc 'movie_id))) (equal? (hash-ref ci 'movie_id) (hash-ref mk 'movie_id))) (equal? (hash-ref ci 'movie_id) (hash-ref cc 'movie_id))) (equal? (hash-ref mk 'movie_id) (hash-ref cc 'movie_id))) (equal? (hash-ref cn 'id) (hash-ref mc 'company_id))) (equal? (hash-ref it 'id) (hash-ref mi 'info_type_id))) (equal? (hash-ref n 'id) (hash-ref ci 'person_id))) (equal? (hash-ref rt 'id) (hash-ref ci 'role_id))) (equal? (hash-ref n 'id) (hash-ref an 'person_id))) (equal? (hash-ref ci 'person_id) (hash-ref an 'person_id))) (equal? (hash-ref chn 'id) (hash-ref ci 'person_role_id))) (equal? (hash-ref n 'id) (hash-ref pi 'person_id))) (equal? (hash-ref ci 'person_id) (hash-ref pi 'person_id))) (equal? (hash-ref it3 'id) (hash-ref pi 'info_type_id))) (equal? (hash-ref k 'id) (hash-ref mk 'keyword_id))) (equal? (hash-ref cct1 'id) (hash-ref cc 'subject_id))) (equal? (hash-ref cct2 'id) (hash-ref cc 'status_id))))) (hash 'voiced_char (hash-ref chn 'name) 'voicing_actress (hash-ref n 'name) 'voiced_animation (hash-ref t 'title))))
(define result (list (hash 'voiced_char (_min (for*/list ([x matches]) (hash-ref x 'voiced_char))) 'voicing_actress (_min (for*/list ([x matches]) (hash-ref x 'voicing_actress))) 'voiced_animation (_min (for*/list ([x matches]) (hash-ref x 'voiced_animation))))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'voiced_char "Queen" 'voicing_actress "Angela Aniston" 'voiced_animation "Shrek 2"))) (displayln "ok"))
