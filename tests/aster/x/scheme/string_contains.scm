;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(define s "catch")
(display
  (if
    (if
      (string-contains s "cat") "true" "false") 1 0))
(newline)
(display
  (if
    (if
      (string-contains s "dog") "true" "false") 1 0))
(newline)
