;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(define x 8)
(define msg
  (if
    (> x 10) "big"
    (if
      (> x 5) "medium" "small")))
(display msg)
(newline)
