;; Generated on 2025-07-22 10:01 +0700
(import
  (only
    (scheme base) call/cc))
(define
  (to-str x)
  (cond
    ((pair? x)
      (string-append "["
        (string-join
          (map to-str x) ", ") "]"))
    ((string? x) x)
    ((boolean? x)
      (if x "1" "0"))
    (else
      (number->string x))))
(define i 0)
(call/cc
  (lambda
    (break2)
    (letrec
      ((loop1
          (lambda
            (if
              (< i 3)
              (begin
                (display
                  (to-str i))
                (newline)
                (set! i
                  (+ i 1))
                (loop1))
              (quote nil)))))
      (loop1))))
