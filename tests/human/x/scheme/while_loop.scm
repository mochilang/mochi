(let loop ((i 0))
  (when (< i 3)
    (begin (display i) (newline))
    (loop (+ i 1))))
