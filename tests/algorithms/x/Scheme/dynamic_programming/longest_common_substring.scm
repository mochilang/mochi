;; Generated on 2025-08-07 08:40 +0700
(import (scheme base))
(import (scheme time))
(import (chibi string))
(import (only (scheme char) string-upcase string-downcase))
(import (srfi 69))
(import (srfi 1))
(define _list list)
(import (chibi time))
(define (_mem) (* 1024 (resource-usage-max-rss (get-resource-usage resource-usage/self))))
(import (chibi json))
(define (to-str x)
  (cond ((pair? x)
         (string-append "[" (string-join (map to-str x) ", ") "]"))
        ((hash-table? x)
         (let* ((ks (hash-table-keys x))
                (pairs (map (lambda (k)
                              (string-append (to-str k) ": " (to-str (hash-table-ref x k))))
                            ks)))
           (string-append "{" (string-join pairs ", ") "}")))
        ((null? x) "[]")
        ((string? x) (let ((out (open-output-string))) (json-write x out) (get-output-string out)))
        ((boolean? x) (if x "true" "false"))
        (else (number->string x))))
(define (to-str-space x)
  (cond ((pair? x)
         (string-append "[" (string-join (map to-str-space x) " ") "]"))
        ((string? x) x)
        (else (to-str x))))
(define (upper s) (string-upcase s))
(define (lower s) (string-downcase s))
(define (fmod a b) (- a (* (floor (/ a b)) b)))
(define (_mod a b) (if (and (integer? a) (integer? b)) (modulo a b) (fmod a b)))
(define (_div a b) (if (and (integer? a) (integer? b)) (quotient a b) (/ a b)))
(define (_gt a b) (cond ((and (number? a) (number? b)) (> a b)) ((and (string? a) (string? b)) (string>? a b)) (else (> a b))))
(define (_lt a b) (cond ((and (number? a) (number? b)) (< a b)) ((and (string? a) (string? b)) (string<? a b)) (else (< a b))))
(define (_ge a b) (cond ((and (number? a) (number? b)) (>= a b)) ((and (string? a) (string? b)) (string>=? a b)) (else (>= a b))))
(define (_le a b) (cond ((and (number? a) (number? b)) (<= a b)) ((and (string? a) (string? b)) (string<=? a b)) (else (<= a b))))
(define (_add a b)
  (cond ((and (number? a) (number? b)) (+ a b))
        ((string? a) (string-append a (to-str b)))
        ((string? b) (string-append (to-str a) b))
        ((and (list? a) (list? b)) (append a b))
        (else (+ a b))))
(define (indexOf s sub) (let ((cur (string-contains s sub)))   (if cur (string-cursor->index s cur) -1)))
(define (_display . args) (apply display args))
(define (panic msg) (error msg))
(define (padStart s width pad)
  (let loop ((out s))
    (if (< (string-length out) width)
        (loop (string-append pad out))
        out)))
(define (_substring s start end)
  (let* ((len (string-length s))
         (s0 (max 0 (min len start)))
         (e0 (max s0 (min len end))))
    (substring s s0 e0)))
(define (_repeat s n)
  (let loop ((i 0) (out ""))
    (if (< i n)
        (loop (+ i 1) (string-append out s))
        out)))
(define (slice seq start end)
  (let* ((len (if (string? seq) (string-length seq) (length seq)))
         (s (if (< start 0) (+ len start) start))
         (e (if (< end 0) (+ len end) end)))
    (set! s (max 0 (min len s)))
    (set! e (max 0 (min len e)))
    (when (< e s) (set! e s))
    (if (string? seq)
        (_substring seq s e)
        (take (drop seq s) (- e s)))))
(define (_parseIntStr s base)
  (let* ((b (if (number? base) base 10))
         (n (string->number (if (list? s) (list->string s) s) b)))
    (if n (inexact->exact (truncate n)) 0)))
(define (_split s sep)
  (let* ((str (if (string? s) s (list->string s)))
         (del (cond ((char? sep) sep)
                     ((string? sep) (if (= (string-length sep) 1)
                                       (string-ref sep 0)
                                       sep))
                     (else sep))))
    (cond
     ((and (string? del) (string=? del ""))
      (map string (string->list str)))
     ((char? del)
      (string-split str del))
     (else
        (let loop ((r str) (acc '()))
          (let ((cur (string-contains r del)))
            (if cur
                (let ((idx (string-cursor->index r cur)))
                  (loop (_substring r (+ idx (string-length del)) (string-length r))
                        (cons (_substring r 0 idx) acc)))
                (reverse (cons r acc)))))))))
(define (_len x)
  (cond ((string? x) (string-length x))
        ((hash-table? x) (hash-table-size x))
        (else (length x))))
(
  let (
    (
      start10 (
        current-jiffy
      )
    )
     (
      jps13 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        longest_common_substring text1 text2
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                or (
                  equal? (
                    _len text1
                  )
                   0
                )
                 (
                  equal? (
                    _len text2
                  )
                   0
                )
              )
               (
                begin (
                  ret1 ""
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              let (
                (
                  m (
                    _len text1
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      n (
                        _len text2
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          dp (
                            _list
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              i 0
                            )
                          )
                           (
                            begin (
                              call/cc (
                                lambda (
                                  break3
                                )
                                 (
                                  letrec (
                                    (
                                      loop2 (
                                        lambda (
                                          
                                        )
                                         (
                                          if (
                                            < i (
                                              + m 1
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  row (
                                                    _list
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      j 0
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      call/cc (
                                                        lambda (
                                                          break5
                                                        )
                                                         (
                                                          letrec (
                                                            (
                                                              loop4 (
                                                                lambda (
                                                                  
                                                                )
                                                                 (
                                                                  if (
                                                                    < j (
                                                                      + n 1
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      set! row (
                                                                        append row (
                                                                          _list 0
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      set! j (
                                                                        + j 1
                                                                      )
                                                                    )
                                                                     (
                                                                      loop4
                                                                    )
                                                                  )
                                                                   (
                                                                    quote (
                                                                      
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            loop4
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      set! dp (
                                                        append dp (
                                                          _list row
                                                        )
                                                      )
                                                    )
                                                     (
                                                      set! i (
                                                        + i 1
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              loop2
                                            )
                                          )
                                           (
                                            quote (
                                              
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop2
                                  )
                                )
                              )
                            )
                             (
                              let (
                                (
                                  end_pos 0
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      max_len 0
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          ii 1
                                        )
                                      )
                                       (
                                        begin (
                                          call/cc (
                                            lambda (
                                              break7
                                            )
                                             (
                                              letrec (
                                                (
                                                  loop6 (
                                                    lambda (
                                                      
                                                    )
                                                     (
                                                      if (
                                                        <= ii m
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              jj 1
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              call/cc (
                                                                lambda (
                                                                  break9
                                                                )
                                                                 (
                                                                  letrec (
                                                                    (
                                                                      loop8 (
                                                                        lambda (
                                                                          
                                                                        )
                                                                         (
                                                                          if (
                                                                            <= jj n
                                                                          )
                                                                           (
                                                                            begin (
                                                                              if (
                                                                                string=? (
                                                                                  _substring text1 (
                                                                                    - ii 1
                                                                                  )
                                                                                   ii
                                                                                )
                                                                                 (
                                                                                  _substring text2 (
                                                                                    - jj 1
                                                                                  )
                                                                                   jj
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  list-set! (
                                                                                    list-ref dp ii
                                                                                  )
                                                                                   jj (
                                                                                    + 1 (
                                                                                      cond (
                                                                                        (
                                                                                          string? (
                                                                                            list-ref dp (
                                                                                              - ii 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          _substring (
                                                                                            list-ref dp (
                                                                                              - ii 1
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            - jj 1
                                                                                          )
                                                                                           (
                                                                                            + (
                                                                                              - jj 1
                                                                                            )
                                                                                             1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? (
                                                                                            list-ref dp (
                                                                                              - ii 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref (
                                                                                            list-ref dp (
                                                                                              - ii 1
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            - jj 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref (
                                                                                            list-ref dp (
                                                                                              - ii 1
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            - jj 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  if (
                                                                                    > (
                                                                                      cond (
                                                                                        (
                                                                                          string? (
                                                                                            list-ref dp ii
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          _substring (
                                                                                            list-ref dp ii
                                                                                          )
                                                                                           jj (
                                                                                            + jj 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? (
                                                                                            list-ref dp ii
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref (
                                                                                            list-ref dp ii
                                                                                          )
                                                                                           jj
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref (
                                                                                            list-ref dp ii
                                                                                          )
                                                                                           jj
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     max_len
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      set! max_len (
                                                                                        cond (
                                                                                          (
                                                                                            string? (
                                                                                              list-ref dp ii
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            _substring (
                                                                                              list-ref dp ii
                                                                                            )
                                                                                             jj (
                                                                                              + jj 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? (
                                                                                              list-ref dp ii
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref (
                                                                                              list-ref dp ii
                                                                                            )
                                                                                             jj
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref (
                                                                                              list-ref dp ii
                                                                                            )
                                                                                             jj
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      set! end_pos ii
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    quote (
                                                                                      
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                quote (
                                                                                  
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              set! jj (
                                                                                + jj 1
                                                                              )
                                                                            )
                                                                             (
                                                                              loop8
                                                                            )
                                                                          )
                                                                           (
                                                                            quote (
                                                                              
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    loop8
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              set! ii (
                                                                + ii 1
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          loop6
                                                        )
                                                      )
                                                       (
                                                        quote (
                                                          
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                loop6
                                              )
                                            )
                                          )
                                        )
                                         (
                                          ret1 (
                                            _substring text1 (
                                              - end_pos max_len
                                            )
                                             end_pos
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
     (
      _display (
        if (
          string? (
            longest_common_substring "abcdef" "xabded"
          )
        )
         (
          longest_common_substring "abcdef" "xabded"
        )
         (
          to-str (
            longest_common_substring "abcdef" "xabded"
          )
        )
      )
    )
     (
      newline
    )
     (
      _display (
        if (
          string? "\n"
        )
         "\n" (
          to-str "\n"
        )
      )
    )
     (
      newline
    )
     (
      _display (
        if (
          string? (
            longest_common_substring "zxabcdezy" "yzabcdezx"
          )
        )
         (
          longest_common_substring "zxabcdezy" "yzabcdezx"
        )
         (
          to-str (
            longest_common_substring "zxabcdezy" "yzabcdezx"
          )
        )
      )
    )
     (
      newline
    )
     (
      let (
        (
          end11 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur12 (
              quotient (
                * (
                  - end11 start10
                )
                 1000000
              )
               jps13
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur12
              )
               ",\n  \"memory_bytes\": " (
                number->string (
                  _mem
                )
              )
               ",\n  \"name\": \"main\"\n}"
            )
          )
           (
            newline
          )
        )
      )
    )
  )
)
