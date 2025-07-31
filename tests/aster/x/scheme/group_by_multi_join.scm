;; Generated on 2025-07-21 19:10 +0700
(import
  (srfi 69)
  (scheme sort)
  (chibi string))
(define
  (to-str x)
  (cond
    ((hash-table? x)
      (let*
        ((pairs
            (hash-table->alist x))
          (pairs
            (list-sort
              (lambda
                (a b)
                (string<?
                  (car a)
                  (car b))) pairs)))
        (string-append "{"
          (string-join
            (map
              (lambda
                (kv)
                (string-append "'"
                  (car kv) "': "
                  (to-str
                    (cdr kv)))) pairs) ", ") "}")))
    ((list? x)
      (string-append "["
        (string-join
          (map to-str x) ", ") "]"))
    ((string? x)
      (string-append "\"" x "\""))
    (else
      (number->string x))))
(define nations
  (list
    (alist->hash-table
      (list
        (cons "id" 1)
        (cons "name" "A")))
    (alist->hash-table
      (list
        (cons "id" 2)
        (cons "name" "B")))))
(define suppliers
  (list
    (alist->hash-table
      (list
        (cons "id" 1)
        (cons "nation" 1)))
    (alist->hash-table
      (list
        (cons "id" 2)
        (cons "nation" 2)))))
(define partsupp
  (list
    (alist->hash-table
      (list
        (cons "part" 100)
        (cons "supplier" 1)
        (cons "cost" 10.0)
        (cons "qty" 2)))
    (alist->hash-table
      (list
        (cons "part" 100)
        (cons "supplier" 2)
        (cons "cost" 20.0)
        (cons "qty" 1)))
    (alist->hash-table
      (list
        (cons "part" 200)
        (cons "supplier" 1)
        (cons "cost" 5.0)
        (cons "qty" 3)))))
(define filtered
  (let
    ((res1
        (list)))
    (begin
      (for-each
        (lambda
          (ps)
          (for-each
            (lambda
              (s)
              (for-each
                (lambda
                  (n)
                  (if
                    (and
                      (and
                        (string=?
                          (hash-table-ref n "name") "A")
                        (=
                          (hash-table-ref s "id")
                          (hash-table-ref ps "supplier")))
                      (=
                        (hash-table-ref n "id")
                        (hash-table-ref s "nation")))
                    (set! res1
                      (append res1
                        (list
                          (alist->hash-table
                            (list
                              (cons "part"
                                (hash-table-ref ps "part"))
                              (cons "value"
                                (*
                                  (hash-table-ref ps "cost")
                                  (hash-table-ref ps "qty"))))))))
                    (quote nil))) nations)) suppliers)) partsupp) res1)))
(define grouped
  (let
    ((groups3
        (make-hash-table))
      (res6
        (list)))
    (begin
      (for-each
        (lambda
          (x)
          (let*
            ((k5
                (hash-table-ref x "part"))
              (g4
                (hash-table-ref/default groups3 k5 #f)))
            (begin
              (if
                (not g4)
                (begin
                  (set! g4
                    (alist->hash-table
                      (list
                        (cons "key" k5)
                        (cons "items"
                          (list)))))
                  (hash-table-set! groups3 k5 g4))
                (quote nil))
              (hash-table-set! g4 "items"
                (append
                  (hash-table-ref g4 "items")
                  (list x)))))) filtered)
      (for-each
        (lambda
          (g)
          (set! res6
            (append res6
              (list
                (alist->hash-table
                  (list
                    (cons "part"
                      (hash-table-ref g "key"))
                    (cons "total"
                      (apply +
                        (let
                          ((res2
                              (list)))
                          (begin
                            (for-each
                              (lambda
                                (r)
                                (set! res2
                                  (append res2
                                    (list
                                      (hash-table-ref r "value")))))
                              (hash-table-ref g "items")) res2))))))))))
        (hash-table-values groups3)) res6)))
(display
  (to-str grouped))
(newline)
