;; Generated on 2025-07-22 09:24 +0700
(define
  (to-str x)
  (cond
    ((pair? x)
      (string-append "["
        (string-join
          (map to-str x) ", ") "]"))
    ((string? x) x)
    ((boolean? x)
      (if x "true" "false"))
    (else
      (number->string x))))
(define x 2)
(define label
  (let
    ((match1 x))
    (cond
      ((equal? match1 1) "one")
      ((equal? match1 2) "two")
      ((equal? match1 3) "three")
      (else "unknown"))))
(display
  (to-str label))
(newline)
