;; Generated on 2025-07-22 10:25 +0700
(import (srfi 1) (srfi 69) (scheme sort))
(define (join xs sep)
  (if (null? xs)
      ""
      (if (null? (cdr xs))
          (car xs)
          (string-append (car xs) sep (join (cdr xs) sep)))))

(define (to-str x)
  (cond ((pair? x)
         (string-append "[" (join (map to-str x) ", ") "]"))
        ((string? x) x)
        ((boolean? x) (if x "true" "false"))
        (else (number->string x))))
(define y 0)
(display (to-str y)
)
(newline)
