;; Generated on 2025-07-21 17:26 +0700
(import (srfi 1) (srfi 69) (chibi string))
(display (hash-table-size (alist->hash-table (list (cons "a" 1)
         (cons "b" 2)
        )
      )
    )
  )
(newline)
