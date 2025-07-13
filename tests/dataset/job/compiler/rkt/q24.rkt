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

(define aka_name (list (hash 'person_id 1)))
(define char_name (list (hash 'id 1 'name "Hero Character")))
(define cast_info (list (hash 'movie_id 1 'person_id 1 'person_role_id 1 'role_id 1 'note "(voice)")))
(define company_name (list (hash 'id 1 'country_code "[us]")))
(define info_type (list (hash 'id 1 'info "release dates")))
(define keyword (list (hash 'id 1 'keyword "hero")))
(define movie_companies (list (hash 'movie_id 1 'company_id 1)))
(define movie_info (list (hash 'movie_id 1 'info_type_id 1 'info "Japan: Feb 2015")))
(define movie_keyword (list (hash 'movie_id 1 'keyword_id 1)))
(define name (list (hash 'id 1 'name "Ann Actress" 'gender "f")))
(define role_type (list (hash 'id 1 'role "actress")))
(define title (list (hash 'id 1 'title "Heroic Adventure" 'production_year 2015)))
(define matches (for*/list ([an aka_name] [chn char_name] [ci cast_info] [cn company_name] [it info_type] [k keyword] [mc movie_companies] [mi movie_info] [mk movie_keyword] [n name] [rt role_type] [t title] #:when (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (cond [(string? '("(voice)" "(voice: Japanese version)" "(voice) (uncredited)" "(voice: English version)")) (regexp-match? (regexp (hash-ref ci 'note)) '("(voice)" "(voice: Japanese version)" "(voice) (uncredited)" "(voice: English version)"))] [(hash? '("(voice)" "(voice: Japanese version)" "(voice) (uncredited)" "(voice: English version)")) (hash-has-key? '("(voice)" "(voice: Japanese version)" "(voice) (uncredited)" "(voice: English version)") (hash-ref ci 'note))] [else (member (hash-ref ci 'note) '("(voice)" "(voice: Japanese version)" "(voice) (uncredited)" "(voice: English version)"))]) (string=? (hash-ref cn 'country_code) "[us]")) (string=? (hash-ref it 'info) "release dates")) (cond [(string? '("hero" "martial-arts" "hand-to-hand-combat")) (regexp-match? (regexp (hash-ref k 'keyword)) '("hero" "martial-arts" "hand-to-hand-combat"))] [(hash? '("hero" "martial-arts" "hand-to-hand-combat")) (hash-has-key? '("hero" "martial-arts" "hand-to-hand-combat") (hash-ref k 'keyword))] [else (member (hash-ref k 'keyword) '("hero" "martial-arts" "hand-to-hand-combat"))])) (not (equal? (hash-ref mi 'info) '()))) (or (and (regexp-match? (regexp (string-append "^" (regexp-quote "Japan:"))) (hash-ref mi 'info)) (regexp-match? (regexp (regexp-quote "201")) (hash-ref mi 'info))) (and (regexp-match? (regexp (string-append "^" (regexp-quote "USA:"))) (hash-ref mi 'info)) (regexp-match? (regexp (regexp-quote "201")) (hash-ref mi 'info))))) (string=? (hash-ref n 'gender) "f")) (regexp-match? (regexp (regexp-quote "An")) (hash-ref n 'name))) (string=? (hash-ref rt 'role) "actress")) (> (hash-ref t 'production_year) 2010)) (equal? (hash-ref t 'id) (hash-ref mi 'movie_id))) (equal? (hash-ref t 'id) (hash-ref mc 'movie_id))) (equal? (hash-ref t 'id) (hash-ref ci 'movie_id))) (equal? (hash-ref t 'id) (hash-ref mk 'movie_id))) (equal? (hash-ref mc 'movie_id) (hash-ref ci 'movie_id))) (equal? (hash-ref mc 'movie_id) (hash-ref mi 'movie_id))) (equal? (hash-ref mc 'movie_id) (hash-ref mk 'movie_id))) (equal? (hash-ref mi 'movie_id) (hash-ref ci 'movie_id))) (equal? (hash-ref mi 'movie_id) (hash-ref mk 'movie_id))) (equal? (hash-ref ci 'movie_id) (hash-ref mk 'movie_id))) (equal? (hash-ref cn 'id) (hash-ref mc 'company_id))) (equal? (hash-ref it 'id) (hash-ref mi 'info_type_id))) (equal? (hash-ref n 'id) (hash-ref ci 'person_id))) (equal? (hash-ref rt 'id) (hash-ref ci 'role_id))) (equal? (hash-ref n 'id) (hash-ref an 'person_id))) (equal? (hash-ref ci 'person_id) (hash-ref an 'person_id))) (equal? (hash-ref chn 'id) (hash-ref ci 'person_role_id))) (equal? (hash-ref k 'id) (hash-ref mk 'keyword_id))))) (hash 'voiced_char_name (hash-ref chn 'name) 'voicing_actress_name (hash-ref n 'name) 'voiced_action_movie_jap_eng (hash-ref t 'title))))
(define result (list (hash 'voiced_char_name (_min (for*/list ([x matches]) (hash-ref x 'voiced_char_name))) 'voicing_actress_name (_min (for*/list ([x matches]) (hash-ref x 'voicing_actress_name))) 'voiced_action_movie_jap_eng (_min (for*/list ([x matches]) (hash-ref x 'voiced_action_movie_jap_eng))))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'voiced_char_name "Hero Character" 'voicing_actress_name "Ann Actress" 'voiced_action_movie_jap_eng "Heroic Adventure"))) (displayln "ok"))
