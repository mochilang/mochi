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

(define keyword (list (hash 'id 1 'keyword "amazing sequel") (hash 'id 2 'keyword "prequel")))
(define movie_info (list (hash 'movie_id 10 'info "Germany") (hash 'movie_id 30 'info "Sweden") (hash 'movie_id 20 'info "France")))
(define movie_keyword (list (hash 'movie_id 10 'keyword_id 1) (hash 'movie_id 30 'keyword_id 1) (hash 'movie_id 20 'keyword_id 1) (hash 'movie_id 10 'keyword_id 2)))
(define title (list (hash 'id 10 'title "Alpha" 'production_year 2006) (hash 'id 30 'title "Beta" 'production_year 2008) (hash 'id 20 'title "Gamma" 'production_year 2009)))
(define allowed_infos '("Sweden" "Norway" "Germany" "Denmark" "Swedish" "Denish" "Norwegian" "German"))
(define candidate_titles (for*/list ([k keyword] [mk movie_keyword] [mi movie_info] [t title] #:when (and (equal? (hash-ref mk 'keyword_id) (hash-ref k 'id)) (equal? (hash-ref mi 'movie_id) (hash-ref mk 'movie_id)) (equal? (hash-ref t 'id) (hash-ref mi 'movie_id)) (and (and (and (regexp-match? (regexp "sequel") (hash-ref k 'keyword)) (cond [(string? allowed_infos) (regexp-match? (regexp (hash-ref mi 'info)) allowed_infos)] [(hash? allowed_infos) (hash-has-key? allowed_infos (hash-ref mi 'info))] [else (member (hash-ref mi 'info) allowed_infos)])) (> (hash-ref t 'production_year) 2005)) (equal? (hash-ref mk 'movie_id) (hash-ref mi 'movie_id))))) (hash-ref t 'title)))
(define result (list (hash 'movie_title (_min candidate_titles))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'movie_title "Alpha"))) (displayln "ok"))
