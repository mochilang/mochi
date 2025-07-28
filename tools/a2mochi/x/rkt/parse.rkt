(require racket/base)
(require json)
(define (to-json x)
  (cond [(symbol? x) (symbol->string x)]
        [(pair? x) (map to-json x)]
        [(vector? x) (list->vector (map to-json (vector->list x)))]
        [else x]))
(define stx (read-syntax 'stdin (current-input-port)))
(write-json (to-json (syntax->datum stx)))
