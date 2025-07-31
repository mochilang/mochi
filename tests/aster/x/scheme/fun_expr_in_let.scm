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
(define square
  (lambda
    (x)
    (* x x)))
(display
  (to-str
    (square 6)))
(newline)
