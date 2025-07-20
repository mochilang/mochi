;; Generated on 2025-07-20 14:31 +0700
(import (srfi 1) (srfi 69) (chibi string))
(display (hash-table-size (alist->hash-table (list (cons "a" 1)
         (cons "b" 2)
        )
      )
    )
  )
(newline)
