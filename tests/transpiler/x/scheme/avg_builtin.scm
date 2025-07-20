;; Generated on 2025-07-20 12:19 +0700
(import (srfi 1))
(display (let ((xs (list 1 2 3))) (exact->inexact (/ (apply + xs) (length xs)))))
(newline)
