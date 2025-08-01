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
(display
  (to-str
    (if
      (string<? "a" "b") #t #f)))
(newline)
(display
  (to-str
    (if
      (string<=? "a" "a") #t #f)))
(newline)
(display
  (to-str
    (if
      (string>? "b" "a") #t #f)))
(newline)
(display
  (to-str
    (if
      (string>=? "b" "b") #t #f)))
(newline)
