;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(define nums
  (list 1 2))
(list-set! nums 1 3)
(display
  (list-ref nums 1))
(newline)
