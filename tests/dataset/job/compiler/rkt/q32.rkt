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

(define keyword (list (hash 'id 1 'keyword "10,000-mile-club") (hash 'id 2 'keyword "character-name-in-title")))
(define link_type (list (hash 'id 1 'link "sequel") (hash 'id 2 'link "remake")))
(define movie_keyword (list (hash 'movie_id 100 'keyword_id 1) (hash 'movie_id 200 'keyword_id 2)))
(define movie_link (list (hash 'movie_id 100 'linked_movie_id 300 'link_type_id 1) (hash 'movie_id 200 'linked_movie_id 400 'link_type_id 2)))
(define title (list (hash 'id 100 'title "Movie A") (hash 'id 200 'title "Movie B") (hash 'id 300 'title "Movie C") (hash 'id 400 'title "Movie D")))
(define joined (for*/list ([k keyword] [mk movie_keyword] [t1 title] [ml movie_link] [t2 title] [lt link_type] #:when (and (equal? (hash-ref mk 'keyword_id) (hash-ref k 'id)) (equal? (hash-ref t1 'id) (hash-ref mk 'movie_id)) (equal? (hash-ref ml 'movie_id) (hash-ref t1 'id)) (equal? (hash-ref t2 'id) (hash-ref ml 'linked_movie_id)) (equal? (hash-ref lt 'id) (hash-ref ml 'link_type_id)) (string=? (hash-ref k 'keyword) "10,000-mile-club"))) (hash 'link_type (hash-ref lt 'link) 'first_movie (hash-ref t1 'title) 'second_movie (hash-ref t2 'title))))
(define result (hash 'link_type (_min (for*/list ([r joined]) (hash-ref r 'link_type))) 'first_movie (_min (for*/list ([r joined]) (hash-ref r 'first_movie))) 'second_movie (_min (for*/list ([r joined]) (hash-ref r 'second_movie)))))
(displayln (jsexpr->string (list result)))
(when (equal? result (hash 'link_type "sequel" 'first_movie "Movie A" 'second_movie "Movie C")) (displayln "ok"))
