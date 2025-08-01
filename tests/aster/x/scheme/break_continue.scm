;; Generated on 2025-07-25 08:58 +0700
(import
  (only
    (scheme base) call/cc when list-ref list-set! list))
(import
  (scheme time))
(import
  (chibi string))
(import
  (only
    (scheme char) string-upcase string-downcase))
(define
  (to-str x)
  (cond
    ((pair? x)
      (string-append "["
        (string-join
          (map to-str x) ", ") "]"))
    ((string? x) x)
    ((boolean? x)
      (if x "1" "0"))
    (else
      (number->string x))))
(define
  (upper s)
  (string-upcase s))
(define
  (lower s)
  (string-downcase s))
(define
  (fmod a b)
  (- a
    (*
      (floor
        (/ a b)) b)))
(define numbers
  (list 1 2 3 4 5 6 7 8 9))
(call/cc
  (lambda
    (break11)
    (letrec
      ((loop10
          (lambda
            (xs)
            (if
              (null? xs)
              (quote nil)
              (begin
                (let
                  ((n
                      (car xs)))
                  (begin
                    (if
                      (equal?
                        (modulo n 2) 0)
                      (begin
                        (loop10
                          (cdr xs)))
                      (quote nil))
                    (if
                      (> n 7)
                      (begin
                        (break11
                          (quote nil)))
                      (quote nil))
                    (display
                      (to-str "odd number:"))
                    (display " ")
                    (display
                      (to-str n))
                    (newline)))
                (loop10
                  (cdr xs)))))))
      (loop10 numbers))))
