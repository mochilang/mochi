(let loop ((i 1))
  (if (< i 4)
    (begin
      (begin (display i) (newline))
      (loop (+ i 1))
    )
  '()
)
)
