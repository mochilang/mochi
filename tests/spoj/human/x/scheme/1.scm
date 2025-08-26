;; Solution for SPOJ TEST - Life, the Universe, and Everything
;; https://www.spoj.com/problems/TEST
(let loop ()
  (let ((x (read)))
    (cond
      ((eof-object? x) #t)
      ((= x 42) #t)
      (else
        (display x)
        (newline)
        (loop)))))
