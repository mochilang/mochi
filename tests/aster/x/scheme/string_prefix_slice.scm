;; Generated on 2025-07-22 06:19 +0700
(begin
  (current-error-port
    (open-output-string))
  (import
    (scheme base)
    (srfi 69)
    (scheme sort)
    (chibi string)))
(define
  (to-str x)
  (cond
    ((and
        (list? x)
        (pair? x)
        (pair?
          (car x))
        (string?
          (car
            (car x))))
      (string-append "{"
        (string-join
          (map
            (lambda
              (kv)
              (string-append "\""
                (car kv) "\": "
                (to-str
                  (cdr kv)))) x) ", ") "}"))
    ((hash-table? x)
      (let
        ((pairs
            (hash-table->alist x)))
        (string-append "{"
          (string-join
            (map
              (lambda
                (kv)
                (string-append "\""
                  (car kv) "\": "
                  (to-str
                    (cdr kv)))) pairs) ", ") "}")))
    ((list? x)
      (string-append "["
        (string-join
          (map to-str x) ", ") "]"))
    ((string? x)
      (string-append "\"" x "\""))
    ((boolean? x)
      (if x "true" "false"))
    (else
      (number->string x))))
(define prefix "fore")
(define s1 "forest")
(display
  (if
    (string=?
      (substring s1 0
        (string-length prefix)) prefix) 1 0))
(newline)
(define s2 "desert")
(display
  (if
    (string=?
      (substring s2 0
        (string-length prefix)) prefix) 1 0))
(newline)
