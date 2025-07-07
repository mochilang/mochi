#lang racket
(require yaml)
(require file/paths)

(define path
  (build-path (path-only (current-load-relative-directory))
              ".." ".." ".." "tests" "interpreter" "valid" "people.yaml"))

(define people (yaml-load (file->string path)))
(define adults
  (for/list ([p people] #:when (>= (hash-ref p 'age) 18))
    (hash 'name (hash-ref p 'name) 'email (hash-ref p 'email))))

(for ([a adults])
  (displayln (format "~a ~a" (hash-ref a 'name) (hash-ref a 'email))))
