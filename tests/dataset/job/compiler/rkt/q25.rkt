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

(define cast_info (list (hash 'movie_id 1 'person_id 1 'note "(writer)") (hash 'movie_id 2 'person_id 2 'note "(writer)")))
(define info_type (list (hash 'id 1 'info "genres") (hash 'id 2 'info "votes")))
(define keyword (list (hash 'id 1 'keyword "murder") (hash 'id 2 'keyword "romance")))
(define movie_info (list (hash 'movie_id 1 'info_type_id 1 'info "Horror") (hash 'movie_id 2 'info_type_id 1 'info "Comedy")))
(define movie_info_idx (list (hash 'movie_id 1 'info_type_id 2 'info 100) (hash 'movie_id 2 'info_type_id 2 'info 50)))
(define movie_keyword (list (hash 'movie_id 1 'keyword_id 1) (hash 'movie_id 2 'keyword_id 2)))
(define name (list (hash 'id 1 'name "Mike" 'gender "m") (hash 'id 2 'name "Sue" 'gender "f")))
(define title (list (hash 'id 1 'title "Scary Movie") (hash 'id 2 'title "Funny Movie")))
(define allowed_notes '("(writer)" "(head writer)" "(written by)" "(story)" "(story editor)"))
(define allowed_keywords '("murder" "blood" "gore" "death" "female-nudity"))
(define matches (for*/list ([ci cast_info] [it1 info_type] [it2 info_type] [k keyword] [mi movie_info] [mi_idx movie_info_idx] [mk movie_keyword] [n name] [t title] #:when (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (cond [(string? allowed_notes) (regexp-match? (regexp (hash-ref ci 'note)) allowed_notes)] [(hash? allowed_notes) (hash-has-key? allowed_notes (hash-ref ci 'note))] [else (member (hash-ref ci 'note) allowed_notes)]) (string=? (hash-ref it1 'info) "genres")) (string=? (hash-ref it2 'info) "votes")) (cond [(string? allowed_keywords) (regexp-match? (regexp (hash-ref k 'keyword)) allowed_keywords)] [(hash? allowed_keywords) (hash-has-key? allowed_keywords (hash-ref k 'keyword))] [else (member (hash-ref k 'keyword) allowed_keywords)])) (string=? (hash-ref mi 'info) "Horror")) (string=? (hash-ref n 'gender) "m")) (equal? (hash-ref t 'id) (hash-ref mi 'movie_id))) (equal? (hash-ref t 'id) (hash-ref mi_idx 'movie_id))) (equal? (hash-ref t 'id) (hash-ref ci 'movie_id))) (equal? (hash-ref t 'id) (hash-ref mk 'movie_id))) (equal? (hash-ref ci 'movie_id) (hash-ref mi 'movie_id))) (equal? (hash-ref ci 'movie_id) (hash-ref mi_idx 'movie_id))) (equal? (hash-ref ci 'movie_id) (hash-ref mk 'movie_id))) (equal? (hash-ref mi 'movie_id) (hash-ref mi_idx 'movie_id))) (equal? (hash-ref mi 'movie_id) (hash-ref mk 'movie_id))) (equal? (hash-ref mi_idx 'movie_id) (hash-ref mk 'movie_id))) (equal? (hash-ref n 'id) (hash-ref ci 'person_id))) (equal? (hash-ref it1 'id) (hash-ref mi 'info_type_id))) (equal? (hash-ref it2 'id) (hash-ref mi_idx 'info_type_id))) (equal? (hash-ref k 'id) (hash-ref mk 'keyword_id))))) (hash 'budget (hash-ref mi 'info) 'votes (hash-ref mi_idx 'info) 'writer (hash-ref n 'name) 'title (hash-ref t 'title))))
(define result (list (hash 'movie_budget (_min (for*/list ([x matches]) (hash-ref x 'budget))) 'movie_votes (_min (for*/list ([x matches]) (hash-ref x 'votes))) 'male_writer (_min (for*/list ([x matches]) (hash-ref x 'writer))) 'violent_movie_title (_min (for*/list ([x matches]) (hash-ref x 'title))))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'movie_budget "Horror" 'movie_votes 100 'male_writer "Mike" 'violent_movie_title "Scary Movie"))) (displayln "ok"))
