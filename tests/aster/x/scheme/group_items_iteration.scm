;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(define data
  (list
    (alist->hash-table
      (list
        (cons "tag" "a")
        (cons "val" 1)))
    (alist->hash-table
      (list
        (cons "tag" "a")
        (cons "val" 2)))
    (alist->hash-table
      (list
        (cons "tag" "b")
        (cons "val" 3)))))
(define groups
  (let
    ((groups30
        (make-hash-table))
      (res33
        (list)))
    (begin
      (for-each
        (lambda
          (d)
          (let*
            ((k32
                (hash-table-ref d "tag"))
              (g31
                (hash-table-ref/default groups30 k32 #f)))
            (begin
              (if
                (not g31)
                (begin
                  (set! g31
                    (alist->hash-table
                      (list
                        (cons "key" k32)
                        (cons "items"
                          (list)))))
                  (hash-table-set! groups30 k32 g31))
                (quote nil))
              (hash-table-set! g31 "items"
                (append
                  (hash-table-ref g31 "items")
                  (list
                    (alist->hash-table
                      (list
                        (cons "d" d))))))))) data)
      (for-each
        (lambda
          (g)
          (set! res33
            (append res33
              (list g))))
        (hash-table-values groups30)) res33)))
(define tmp
  (list))
(for-each
  (lambda
    (g)
    (begin
      (define total 0)
      (for-each
        (lambda
          (x)
          (begin
            (set! total
              (string-append "total"
                (hash-table-ref x "val")))))
        (hash-table-ref g "items"))
      (set! tmp
        (append tmp
          (list
            (alist->hash-table
              (list
                (cons "tag"
                  (hash-table-ref g "key"))
                (cons "total" "total")))))))) groups)
(define result
  (let
    ((res34
        (list)))
    (begin
      (for-each
        (lambda
          (r)
          (set! res34
            (append res34
              (list r)))) tmp) res34)))
(display result)
(newline)
