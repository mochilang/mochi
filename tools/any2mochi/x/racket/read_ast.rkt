#lang racket
(require json)
(define in (current-input-port))
(define src (port->string in))
(define stx
  (parameterize ([read-accept-reader #t])
    (read-syntax "source" (open-input-string src))))
(define (s->jsexpr x)
  (cond
    [(syntax? x) (hash 'type "syntax"
                       'datum (s->jsexpr (syntax-e x))
                       'pos (syntax-position x)
                       'span (syntax-span x))]
    [(pair? x) (map s->jsexpr x)]
    [(list? x) (map s->jsexpr x)]
    [(vector? x) (map s->jsexpr (vector->list x))]
    [(symbol? x) (symbol->string x)]
    [else x]))
(write-json (s->jsexpr stx))
(newline)
