(define (make-person name age status)
  (vector name age status))
(define (person-name p) (vector-ref p 0))
(define (person-age p) (vector-ref p 1))
(define (person-status p) (vector-ref p 2))
(define (set-person-age! p val) (vector-set! p 1 val))
(define (set-person-status! p val) (vector-set! p 2 val))
(define (person->list p) (list (person-name p) (person-age p) (person-status p)))

(define people (list
  (make-person "Alice" 17 "minor")
  (make-person "Bob" 25 "unknown")
  (make-person "Charlie" 18 "unknown")
  (make-person "Diana" 16 "minor")))

(set! people
  (map (lambda (p)
         (if (>= (person-age p) 18)
             (begin
               (set-person-status! p "adult")
               (set-person-age! p (+ (person-age p) 1))
               p)
             p))
       people))

(define expected (list
  (make-person "Alice" 17 "minor")
  (make-person "Bob" 26 "adult")
  (make-person "Charlie" 19 "adult")
  (make-person "Diana" 16 "minor")))

(define ok (equal? (map person->list people)
                   (map person->list expected)))
(display ok)
(newline)
(display "ok")
(newline)
