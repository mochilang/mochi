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
        (cons "val" 10)
        (cons "flag" "true")))
    (alist->hash-table
      (list
        (cons "cat" "a")
        (cons "val" 5)
        (cons "flag" "false")))
    (alist->hash-table
      (list
        (cons "cat" "b")
        (cons "val" 20)
        (cons "flag" "true")))))
(define result
  (let
    ((res14
        (list)))
    (begin
      (for-each
        (lambda
          (i)
          (set! res14
            (append res14
              (list
                (alist->hash-table
                  (list
                    (cons "cat"
                      (hash-table-ref g "key"))
                    (cons "share"
                      (quotient
                        (apply +
                          (let
                            ((res12
                                (list)))
                            (begin
                              (for-each
                                (lambda
                                  (x)
                                  (set! res12
                                    (append res12
                                      (list
                                        (if
                                          (hash-table-ref x "flag")
                                          (hash-table-ref x "val") 0))))) "g") res12)))
                        (apply +
                          (let
                            ((res13
                                (list)))
                            (begin
                              (for-each
                                (lambda
                                  (x)
                                  (set! res13
                                    (append res13
                                      (list
                                        (hash-table-ref x "val"))))) "g") res13))))))))))) items) res14)))
(display result)
(newline)
