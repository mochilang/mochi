;; Generated on 2025-07-21 17:26 +0700
(import (srfi 1) (srfi 69) (chibi string))
(define todo (alist->hash-table (list (cons "title" "hi")
      )
    )
  )
(display (hash-table-ref todo "title")
  )
(newline)
