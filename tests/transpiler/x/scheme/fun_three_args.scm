;; Generated on 2025-07-21 17:26 +0700
(import (srfi 1) (srfi 69) (chibi string))
(define (sum3 a b c)
   (+ (+ a b)
     c)
  )
(display (sum3 1 2 3)
  )
(newline)
