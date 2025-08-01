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
(display
  (to-str
    (delete-duplicates
      (append
        (list 1 2)
        (list 2 3)))))
(newline)
(display
  (to-str
    (filter
      (lambda
        (x)
        (not
          (member x
            (list 2))))
      (list 1 2 3))))
(newline)
(display
  (to-str
    (filter
      (lambda
        (x)
        (member x
          (list 2 4)))
      (list 1 2 3))))
(newline)
(display
  (to-str
    (length
      (append
        (list 1 2)
        (list 2 3)))))
(newline)
