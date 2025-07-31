;; Generated on 2025-07-25 08:58 +0700
(import
  (only
    (scheme base) call/cc when list-ref list-set! list))
(import
  (scheme time))
(import
  (chibi string))
(import
  (only
    (scheme char) string-upcase string-downcase))
(define
  (to-str x)
  (cond
    ((pair? x)
      (string-append "["
        (string-join
          (map to-str x) ", ") "]"))
    ((string? x) x)
    ((boolean? x)
      (if x "1" "0"))
    (else
      (number->string x))))
(define
  (upper s)
  (string-upcase s))
(define
  (lower s)
  (string-downcase s))
(define
  (fmod a b)
  (- a
    (*
      (floor
        (/ a b)) b)))
(define
  (boom)
  (call/cc
    (lambda
      (ret9)
      (begin
        (display
          (to-str "boom"))
        (newline)
        (ret9 #t)))))
(display
  (to-str
    (if
      (and
        (and
          (< 1 2)
          (< 2 3))
        (< 3 4)) #t #f)))
(newline)
(display
  (to-str
    (if
      (and
        (and
          (< 1 2)
          (> 2 3))
        (boom)) #t #f)))
(newline)
(display
  (to-str
    (if
      (and
        (and
          (and
            (< 1 2)
            (< 2 3))
          (> 3 4))
        (boom)) #t #f)))
(newline)
