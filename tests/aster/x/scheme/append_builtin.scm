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
  (list 1 2))
(display
  (to-str
    (append a
      (list 3))))
(newline)
