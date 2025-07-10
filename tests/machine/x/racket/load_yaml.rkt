#lang racket
(require yaml)
(require file/paths)
(struct Person (name age email) #:transparent #:mutable)
(define people (yaml-load (file->string (build-path (path-only (current-load-relative-directory)) ".." "interpreter" "valid" "people.yaml"))))
(define adults (for*/list ([p people] #:when (and (cond [(string? (hash-ref p 'age)) (string>=? (hash-ref p 'age) 18)] [(string? 18) (string>=? (hash-ref p 'age) 18)] [else (>= (hash-ref p 'age) 18)]))) (hash 'name (hash-ref p 'name) 'email (hash-ref p 'email))))
(for ([a (if (hash? adults) (hash-keys adults) adults)])
(displayln (string-join (map ~a (list (hash-ref a 'name) (hash-ref a 'email))) " "))
)
