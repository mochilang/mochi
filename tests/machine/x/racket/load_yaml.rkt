#lang racket
(require yaml)
(struct Person (name age email) #:transparent #:mutable)
(define people (yaml-load (file->string "../interpreter/valid/people.yaml")))
(define adults (for*/list ([p people] #:when (and (>= (hash-ref p 'age) 18))) (hash 'name (hash-ref p 'name) 'email (hash-ref p 'email))))
(for ([a (if (hash? adults) (hash-keys adults) adults)])
(displayln (string-join (map ~a (list (hash-ref a 'name) (hash-ref a 'email))) " "))
)
