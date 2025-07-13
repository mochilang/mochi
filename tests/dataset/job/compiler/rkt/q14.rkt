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

(define info_type (list (hash 'id 1 'info "countries") (hash 'id 2 'info "rating")))
(define keyword (list (hash 'id 1 'keyword "murder") (hash 'id 2 'keyword "blood") (hash 'id 3 'keyword "romance")))
(define kind_type (list (hash 'id 1 'kind "movie")))
(define title (list (hash 'id 1 'kind_id 1 'production_year 2012 'title "A Dark Movie") (hash 'id 2 'kind_id 1 'production_year 2013 'title "Brutal Blood") (hash 'id 3 'kind_id 1 'production_year 2008 'title "Old Film")))
(define movie_info (list (hash 'movie_id 1 'info_type_id 1 'info "Sweden") (hash 'movie_id 2 'info_type_id 1 'info "USA") (hash 'movie_id 3 'info_type_id 1 'info "USA")))
(define movie_info_idx (list (hash 'movie_id 1 'info_type_id 2 'info 7) (hash 'movie_id 2 'info_type_id 2 'info 7.5) (hash 'movie_id 3 'info_type_id 2 'info 9.1)))
(define movie_keyword (list (hash 'movie_id 1 'keyword_id 1) (hash 'movie_id 2 'keyword_id 2) (hash 'movie_id 3 'keyword_id 3)))
(define allowed_keywords '("murder" "murder-in-title" "blood" "violence"))
(define allowed_countries '("Sweden" "Norway" "Germany" "Denmark" "Swedish" "Denish" "Norwegian" "German" "USA" "American"))
(define matches (for*/list ([it1 info_type] [it2 info_type] [k keyword] [kt kind_type] [mi movie_info] [mi_idx movie_info_idx] [mk movie_keyword] [t title] #:when (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (string=? (hash-ref it1 'info) "countries") (string=? (hash-ref it2 'info) "rating")) (cond [(string? allowed_keywords) (regexp-match? (regexp (hash-ref k 'keyword)) allowed_keywords)] [(hash? allowed_keywords) (hash-has-key? allowed_keywords (hash-ref k 'keyword))] [else (member (hash-ref k 'keyword) allowed_keywords)])) (string=? (hash-ref kt 'kind) "movie")) (cond [(string? allowed_countries) (regexp-match? (regexp (hash-ref mi 'info)) allowed_countries)] [(hash? allowed_countries) (hash-has-key? allowed_countries (hash-ref mi 'info))] [else (member (hash-ref mi 'info) allowed_countries)])) (< (hash-ref mi_idx 'info) 8.5)) (> (hash-ref t 'production_year) 2010)) (equal? (hash-ref kt 'id) (hash-ref t 'kind_id))) (equal? (hash-ref t 'id) (hash-ref mi 'movie_id))) (equal? (hash-ref t 'id) (hash-ref mk 'movie_id))) (equal? (hash-ref t 'id) (hash-ref mi_idx 'movie_id))) (equal? (hash-ref mk 'movie_id) (hash-ref mi 'movie_id))) (equal? (hash-ref mk 'movie_id) (hash-ref mi_idx 'movie_id))) (equal? (hash-ref mi 'movie_id) (hash-ref mi_idx 'movie_id))) (equal? (hash-ref k 'id) (hash-ref mk 'keyword_id))) (equal? (hash-ref it1 'id) (hash-ref mi 'info_type_id))) (equal? (hash-ref it2 'id) (hash-ref mi_idx 'info_type_id))))) (hash 'rating (hash-ref mi_idx 'info) 'title (hash-ref t 'title))))
(define result (hash 'rating (_min (for*/list ([x matches]) (hash-ref x 'rating))) 'northern_dark_movie (_min (for*/list ([x matches]) (hash-ref x 'title)))))
(displayln (jsexpr->string result))
(when (equal? result (hash 'rating 7 'northern_dark_movie "A Dark Movie")) (displayln "ok"))
