;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(define
  (boom a b)
  (begin
    (display "boom")
    (newline) "true"))
(display
  (if
    (and "false"
      (boom 1 2)) 1 0))
(newline)
(display
  (if
    (or "true"
      (boom 1 2)) 1 0))
(newline)
