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

(define aka_name (list (hash 'person_id 1 'name "Anna Mae") (hash 'person_id 2 'name "Chris")))
(define cast_info (list (hash 'person_id 1 'movie_id 10) (hash 'person_id 2 'movie_id 20)))
(define info_type (list (hash 'id 1 'info "mini biography") (hash 'id 2 'info "trivia")))
(define link_type (list (hash 'id 1 'link "features") (hash 'id 2 'link "references")))
(define movie_link (list (hash 'linked_movie_id 10 'link_type_id 1) (hash 'linked_movie_id 20 'link_type_id 2)))
(define name (list (hash 'id 1 'name "Alan Brown" 'name_pcode_cf "B" 'gender "m") (hash 'id 2 'name "Zoe" 'name_pcode_cf "Z" 'gender "f")))
(define person_info (list (hash 'person_id 1 'info_type_id 1 'note "Volker Boehm") (hash 'person_id 2 'info_type_id 1 'note "Other")))
(define title (list (hash 'id 10 'title "Feature Film" 'production_year 1990) (hash 'id 20 'title "Late Film" 'production_year 2000)))
(define rows (for*/list ([an aka_name] [n name] [pi person_info] [it info_type] [ci cast_info] [t title] [ml movie_link] [lt link_type] #:when (and (equal? (hash-ref n 'id) (hash-ref an 'person_id)) (equal? (hash-ref pi 'person_id) (hash-ref an 'person_id)) (equal? (hash-ref it 'id) (hash-ref pi 'info_type_id)) (equal? (hash-ref ci 'person_id) (hash-ref n 'id)) (equal? (hash-ref t 'id) (hash-ref ci 'movie_id)) (equal? (hash-ref ml 'linked_movie_id) (hash-ref t 'id)) (equal? (hash-ref lt 'id) (hash-ref ml 'link_type_id)) (and (and (and (and (and (and (and (and (and (and (and (and (regexp-match? (regexp (regexp-quote "a")) (hash-ref an 'name)) (string=? (hash-ref it 'info) "mini biography")) (string=? (hash-ref lt 'link) "features")) (string>=? (hash-ref n 'name_pcode_cf) "A")) (string<=? (hash-ref n 'name_pcode_cf) "F")) (or (string=? (hash-ref n 'gender) "m") (and (string=? (hash-ref n 'gender) "f") (regexp-match? (regexp (string-append "^" (regexp-quote "B"))) (hash-ref n 'name))))) (string=? (hash-ref pi 'note) "Volker Boehm")) (>= (hash-ref t 'production_year) 1980)) (<= (hash-ref t 'production_year) 1995)) (equal? (hash-ref pi 'person_id) (hash-ref an 'person_id))) (equal? (hash-ref pi 'person_id) (hash-ref ci 'person_id))) (equal? (hash-ref an 'person_id) (hash-ref ci 'person_id))) (equal? (hash-ref ci 'movie_id) (hash-ref ml 'linked_movie_id))))) (hash 'person_name (hash-ref n 'name) 'movie_title (hash-ref t 'title))))
(define result (list (hash 'of_person (_min (for*/list ([r rows]) (hash-ref r 'person_name))) 'biography_movie (_min (for*/list ([r rows]) (hash-ref r 'movie_title))))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'of_person "Alan Brown" 'biography_movie "Feature Film"))) (displayln "ok"))
