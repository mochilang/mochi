;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(define items
  (list
    (alist->hash-table
      (list
        (cons "cat" "a")
        (cons "val" 3)))
    (alist->hash-table
      (list
        (cons "cat" "a")
        (cons "val" 1)))
    (alist->hash-table
      (list
        (cons "cat" "b")
        (cons "val" 5)))
    (alist->hash-table
      (list
        (cons "cat" "b")
        (cons "val" 2)))))
(define grouped
  (let
    ((res29
        (list)))
    (begin
      (for-each
        (lambda
          (i)
          (set! res29
            (append res29
              (list
                (alist->hash-table
                  (list
                    (cons "cat"
                      (hash-table-ref g "key"))
                    (cons "total"
                      (apply +
                        (let
                          ((res28
                              (list)))
                          (begin
                            (for-each
                              (lambda
                                (x)
                                (set! res28
                                  (append res28
                                    (list
                                      (hash-table-ref x "val"))))) "g") res28)))))))))) items) res29)))
(display grouped)
(newline)
