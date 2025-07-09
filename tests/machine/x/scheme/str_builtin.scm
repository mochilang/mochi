(begin (display (let ((s (open-output-string))) (write 123 s) (get-output-string s))) (newline))
