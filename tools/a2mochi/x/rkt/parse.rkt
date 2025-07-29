#lang racket
(require json)

(define src-lines (string-split (port->string (current-input-port)) "\n"))
(define (drop-header ls)
  (cond
    [(null? ls) '()]
    [(regexp-match? #"^#lang" (car ls)) (cdr ls)]
    [else (drop-header (cdr ls))]))
(define body (string-join (drop-header src-lines) "\n"))
(define in (open-input-string body))
(define (datum->json d)
  (cond [(symbol? d) (hash 'sym (symbol->string d))]
        [(pair? d) (map datum->json d)]
        [(vector? d) (map datum->json (vector->list d))]
        [(hash? d) (for/hash ([(k v) (in-hash d)])
                           (values (datum->json k) (datum->json v)))]
        [else d]))

(define forms '())
(parameterize ([read-accept-reader #t])
  (let loop ()
    (define stx (with-handlers ([exn:fail:read? (lambda (e) eof)])
                  (read-syntax 'source in)))
    (unless (eof-object? stx)
      (set! forms (append forms
                          (list (hash 'datum (datum->json (syntax->datum stx))
                                      'line (or (syntax-line stx) 0)
                                      'col (or (syntax-column stx) 0)))))
      (loop))))
(write-json forms)
