//go:build slow

package racket

const helperDateNumber = `(define (_date_number s)
  (let ([parts (string-split s "-")])
    (if (= (length parts) 3)
        (+ (* (string->number (list-ref parts 0)) 10000)
           (* (string->number (list-ref parts 1)) 100)
           (string->number (list-ref parts 2)))
        #f)))`

const helperToString = `(define (_to_string v)
  (cond
    [(eq? v #t) "true"]
    [(eq? v #f) "false"]
    [(void? v) "<nil>"]
    [(list? v) (string-join (map _to_string v) " ")]
    [else (format "~a" v)]))`

const helperLt = `(define (_lt a b)
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
    [else (string<? (_to_string a) (_to_string b))]))`

const helperGt = `(define (_gt a b) (_lt b a))`
const helperLe = `(define (_le a b) (or (_lt a b) (equal? a b)))`
const helperGe = `(define (_ge a b) (or (_gt a b) (equal? a b)))`

const helperMin = `(define (_min v)
  (let* ([lst (cond [(and (hash? v) (hash-has-key? v 'items)) (hash-ref v 'items)]
                    [(list? v) v]
                    [else '()])]
         [m 0])
    (when (not (null? lst))
      (set! m (car lst))
      (for ([n (cdr lst)])
        (when (_lt n m) (set! m n))))
    m))`

const helperMax = `(define (_max v)
  (let* ([lst (cond [(and (hash? v) (hash-has-key? v 'items)) (hash-ref v 'items)]
                    [(list? v) v]
                    [else '()])]
         [m 0])
    (when (not (null? lst))
      (set! m (car lst))
      (for ([n (cdr lst)])
        (when (_gt n m) (set! m n))))
    m))`

const helperJSONFix = `(define (_json-fix v)
  (cond
    [(and (number? v) (rational? v) (not (integer? v))) (real->double-flonum v)]
    [(list? v) (map _json-fix v)]
    [(hash? v) (for/hash ([(k val) v]) (values k (_json-fix val)))]
    [else v]))`

const helperSimpleYAMLParse = `(define (_simple-yaml-parse s)
  (define items '())
  (define current #f)
  (for ([ln (string-split s "\n")])
    (define t (string-trim ln))
    (cond
      [(regexp-match? #px"^-" t)
       (when current (set! items (cons current items)))
       (set! current (make-hash))
       (set! t (string-trim (substring t 1)))
       (let ([m (regexp-match #px"^([^:]+):\\s*(.*)$" t)])
         (when m
           (define k (string->symbol (string-trim (list-ref m 1))))
           (define v (string-trim (list-ref m 2)))
           (hash-set! current k (if (regexp-match? #px"^[0-9]+$" v) (string->number v) v))))]
      [current
       (let ([m (regexp-match #px"^([^:]+):\\s*(.*)$" t)])
         (when m
           (define k (string->symbol (string-trim (list-ref m 1))))
           (define v (string-trim (list-ref m 2)))
           (hash-set! current k (if (regexp-match? #px"^[0-9]+$" v) (string->number v) v))))]))
  (when current (set! items (cons current items)))
  (reverse items))`

var runtimeHelperMap = map[string]string{
	"_date_number":       helperDateNumber,
	"_to_string":         helperToString,
	"_lt":                helperLt,
	"_gt":                helperGt,
	"_le":                helperLe,
	"_ge":                helperGe,
	"_min":               helperMin,
	"_max":               helperMax,
	"_json-fix":          helperJSONFix,
	"_simple-yaml-parse": helperSimpleYAMLParse,
}
