;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(define xs
  (list 1 2 3))
(define ys
  (let
    ((res35
        (list)))
    (begin
      (for-each
        (lambda
          (x)
          (if
            (=
              (modulo x 2) 1)
            (set! res35
              (append res35
                (list x)))
            (quote nil))) xs) res35)))
(display
  (if
    (cond
      ((string? ys)
        (if
          (string-contains ys 1) "true" "false"))
      ((hash-table? ys)
        (if
          (hash-table-exists? ys 1) "true" "false"))
      (else
        (if
          (member 1 ys) "true" "false"))) 1 0))
(newline)
(display
  (if
    (cond
      ((string? ys)
        (if
          (string-contains ys 2) "true" "false"))
      ((hash-table? ys)
        (if
          (hash-table-exists? ys 2) "true" "false"))
      (else
        (if
          (member 2 ys) "true" "false"))) 1 0))
(newline)
(define m
  (alist->hash-table
    (list
      (cons "a" 1))))
(display
  (if
    (cond
      ((string? m)
        (if
          (string-contains m "a") "true" "false"))
      ((hash-table? m)
        (if
          (hash-table-exists? m "a") "true" "false"))
      (else
        (if
          (member "a" m) "true" "false"))) 1 0))
(newline)
(display
  (if
    (cond
      ((string? m)
        (if
          (string-contains m "b") "true" "false"))
      ((hash-table? m)
        (if
          (hash-table-exists? m "b") "true" "false"))
      (else
        (if
          (member "b" m) "true" "false"))) 1 0))
(newline)
(define s "hello")
(display
  (if
    (cond
      ((string? s)
        (if
          (string-contains s "ell") "true" "false"))
      ((hash-table? s)
        (if
          (hash-table-exists? s "ell") "true" "false"))
      (else
        (if
          (member "ell" s) "true" "false"))) 1 0))
(newline)
(display
  (if
    (cond
      ((string? s)
        (if
          (string-contains s "foo") "true" "false"))
      ((hash-table? s)
        (if
          (hash-table-exists? s "foo") "true" "false"))
      (else
        (if
          (member "foo" s) "true" "false"))) 1 0))
(newline)
