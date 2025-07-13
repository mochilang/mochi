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

(define info_type (list (hash 'id 1 'info "budget") (hash 'id 2 'info "votes") (hash 'id 3 'info "rating")))
(define name (list (hash 'id 1 'name "Big Tim" 'gender "m") (hash 'id 2 'name "Slim Tim" 'gender "m") (hash 'id 3 'name "Alice" 'gender "f")))
(define title (list (hash 'id 10 'title "Alpha") (hash 'id 20 'title "Beta") (hash 'id 30 'title "Gamma")))
(define cast_info (list (hash 'movie_id 10 'person_id 1 'note "(producer)") (hash 'movie_id 20 'person_id 2 'note "(executive producer)") (hash 'movie_id 30 'person_id 3 'note "(producer)")))
(define movie_info (list (hash 'movie_id 10 'info_type_id 1 'info 90) (hash 'movie_id 20 'info_type_id 1 'info 120) (hash 'movie_id 30 'info_type_id 1 'info 110)))
(define movie_info_idx (list (hash 'movie_id 10 'info_type_id 2 'info 500) (hash 'movie_id 20 'info_type_id 2 'info 400) (hash 'movie_id 30 'info_type_id 2 'info 800)))
(define rows (for*/list ([ci cast_info] [n name] [t title] [mi movie_info] [mi_idx movie_info_idx] [it1 info_type] [it2 info_type] #:when (and (equal? (hash-ref n 'id) (hash-ref ci 'person_id)) (equal? (hash-ref t 'id) (hash-ref ci 'movie_id)) (equal? (hash-ref mi 'movie_id) (hash-ref t 'id)) (equal? (hash-ref mi_idx 'movie_id) (hash-ref t 'id)) (equal? (hash-ref it1 'id) (hash-ref mi 'info_type_id)) (equal? (hash-ref it2 'id) (hash-ref mi_idx 'info_type_id)) (and (and (and (and (and (and (and (and (cond [(string? '("(producer)" "(executive producer)")) (regexp-match? (regexp (hash-ref ci 'note)) '("(producer)" "(executive producer)"))] [(hash? '("(producer)" "(executive producer)")) (hash-has-key? '("(producer)" "(executive producer)") (hash-ref ci 'note))] [else (member (hash-ref ci 'note) '("(producer)" "(executive producer)"))]) (string=? (hash-ref it1 'info) "budget")) (string=? (hash-ref it2 'info) "votes")) (string=? (hash-ref n 'gender) "m")) (regexp-match? (regexp (regexp-quote "Tim")) (hash-ref n 'name))) (equal? (hash-ref t 'id) (hash-ref ci 'movie_id))) (equal? (hash-ref ci 'movie_id) (hash-ref mi 'movie_id))) (equal? (hash-ref ci 'movie_id) (hash-ref mi_idx 'movie_id))) (equal? (hash-ref mi 'movie_id) (hash-ref mi_idx 'movie_id))))) (hash 'budget (hash-ref mi 'info) 'votes (hash-ref mi_idx 'info) 'title (hash-ref t 'title))))
(define result (hash 'movie_budget (_min (for*/list ([r rows]) (hash-ref r 'budget))) 'movie_votes (_min (for*/list ([r rows]) (hash-ref r 'votes))) 'movie_title (_min (for*/list ([r rows]) (hash-ref r 'title)))))
(displayln (jsexpr->string result))
(when (equal? result (hash 'movie_budget 90 'movie_votes 400 'movie_title "Alpha")) (displayln "ok"))
