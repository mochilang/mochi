;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(define s "catch")
(display
  (if
    (cond
      ((string? s)
        (if
          (string-contains s "cat") "true" "false"))
      ((hash-table? s)
        (if
          (hash-table-exists? s "cat") "true" "false"))
      (else
        (if
          (member "cat" s) "true" "false"))) 1 0))
(newline)
(display
  (if
    (cond
      ((string? s)
        (if
          (string-contains s "dog") "true" "false"))
      ((hash-table? s)
        (if
          (hash-table-exists? s "dog") "true" "false"))
      (else
        (if
          (member "dog" s) "true" "false"))) 1 0))
(newline)
