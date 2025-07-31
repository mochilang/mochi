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
(import
  (chibi time)
  (srfi 98))
(define _now_seeded #f)
(define _now_seed 0)
(define
  (now)
  (when
    (not _now_seeded)
    (let
      ((s
          (get-environment-variable "MOCHI_NOW_SEED")))
      (when
        (and s
          (string->number s))
        (set! _now_seed
          (string->number s))
        (set! _now_seeded #t))))
  (if _now_seeded
    (begin
      (set! _now_seed
        (modulo
          (+
            (* _now_seed 1664525) 1013904223) 2147483647)) _now_seed)
    (*
      (current-seconds) 1000000000)))
(import
  (chibi json))
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
(let
  ((start6
      (now)))
  (begin
    (let
      ((n 1000))
      (begin
        (let
          ((s 0))
          (begin
            (call/cc
              (lambda
                (break5)
                (letrec
                  ((loop4
                      (lambda
                        (i)
                        (if
                          (< i n)
                          (begin
                            (begin
                              (set! s
                                (+ s i)))
                            (loop4
                              (+ i 1)))
                          (quote nil)))))
                  (loop4 1))))))))
    (let
      ((end7
          (now)))
      (let
        ((dur8
            (quotient
              (- end7 start6) 1000)))
        (begin
          (display
            (string-append "{\n  \"duration_us\": "
              (number->string dur8) ",\n  \"memory_bytes\": 0,\n  \"name\": \"simple\"\n}"))
          (newline))))))
