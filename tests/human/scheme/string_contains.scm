(define (string-contains? s sub)
  (let* ((n (string-length s))
         (m (string-length sub)))
    (let loop ((i 0))
      (cond ((> i (- n m)) #f)
            ((string=? sub (substring s i (+ i m))) #t)
            (else (loop (+ i 1)))))))

(define (display-bool b) (display (if b "true" "false")) (newline))

(define s "catch")
(display-bool (string-contains? s "cat"))
(display-bool (string-contains? s "dog"))
