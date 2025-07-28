#lang racket
(require json)

(define src (port->string (current-input-port)))
(define in (open-input-string src))
(define forms '())
(let loop ()
  (define stx (with-handlers ([exn:fail:read? (lambda (e) eof)])
                (read-syntax 'source in)))
  (unless (eof-object? stx)
    (set! forms (append forms
                        (list (hash 'datum (syntax->datum stx)
                                    'line (or (syntax-line stx) 0)
                                    'col (or (syntax-column stx) 0)))))
    (loop)))
(write-json forms)
