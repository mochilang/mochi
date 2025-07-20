;; Generated on 2025-07-20 10:18 +0700
(import (srfi 1))
(define nums (list 1 2 3))
(display (if (string? nums) (string-contains nums 2) (if (member 2 nums) #t #f)))
(newline)
(display (if (string? nums) (string-contains nums 4) (if (member 4 nums) #t #f)))
(newline)
