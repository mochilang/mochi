;; Generated on 2025-07-25 08:58 +0700
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
(define a
  (- 10 3))
(define b
  (+ 2 2))
(display
  (to-str a))
(newline)
(display
  (to-str
    (if
      (equal? a 7) #t #f)))
(newline)
(display
  (to-str
    (if
      (< b 5) #t #f)))
(newline)
