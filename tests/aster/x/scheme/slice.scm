;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(display
  (take
    (drop
      (list 1 2 3) 1)
    (- 3 1)))
(newline)
(display
  (take
    (drop
      (list 1 2 3) 0)
    (- 2 0)))
(newline)
(display
  (substring "hello" 1 4))
(newline)
