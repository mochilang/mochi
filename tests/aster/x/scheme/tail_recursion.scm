;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(define
  (sum_rec n acc)
  (begin
    (if
      (= n 0)
      (begin acc)
      (quote nil))
    (sum_rec
      (- n 1)
      (+ acc n))))
(display
  (sum_rec 10 0))
(newline)
