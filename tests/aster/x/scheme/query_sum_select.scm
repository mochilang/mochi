;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(define nums
  (list 1 2 3))
(define result
  (let
    ((res42
        (list)))
    (begin
      (for-each
        (lambda
          (n)
          (if
            (> n 1)
            (set! res42
              (append res42
                (list
                  (apply + n))))
            (quote nil))) nums) res42)))
(display result)
(newline)
