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
        ceil_index v left right key
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                l left
              )
            )
             (
              begin (
                let (
                  (
                    r right
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
                                  > (
                                    - r l
                                  )
                                   1
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        middle (
                                          _div (
                                            + l r
                                          )
                                           2
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          >= (
                                            list-ref v middle
                                          )
                                           key
                                        )
                                         (
                                          begin (
                                            set! r middle
                                          )
                                        )
                                         (
                                          begin (
                                            set! l middle
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
                    ret1 r
                  )
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        longest_increasing_subsequence_length v
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                equal? (
                  _len v
                )
                 0
              )
               (
                begin (
                  ret4 0
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
                  tail (
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
                          break6
                        )
                         (
                          letrec (
                            (
                              loop5 (
                                lambda (
                                  
                                )
                                 (
                                  if (
                                    < i (
                                      _len v
                                    )
                                  )
                                   (
                                    begin (
                                      set! tail (
                                        append tail (
                                          _list 0
                                        )
                                      )
                                    )
                                     (
                                      set! i (
                                        + i 1
                                      )
                                    )
                                     (
                                      loop5
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
                            loop5
                          )
                        )
                      )
                    )
                     (
                      let (
                        (
                          length 1
                        )
                      )
                       (
                        begin (
                          list-set! tail 0 (
                            list-ref v 0
                          )
                        )
                         (
                          let (
                            (
                              j 1
                            )
                          )
                           (
                            begin (
                              call/cc (
                                lambda (
                                  break8
                                )
                                 (
                                  letrec (
                                    (
                                      loop7 (
                                        lambda (
                                          
                                        )
                                         (
                                          if (
                                            < j (
                                              _len v
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                < (
                                                  list-ref v j
                                                )
                                                 (
                                                  list-ref tail 0
                                                )
                                              )
                                               (
                                                begin (
                                                  list-set! tail 0 (
                                                    list-ref v j
                                                  )
                                                )
                                              )
                                               (
                                                if (
                                                  > (
                                                    list-ref v j
                                                  )
                                                   (
                                                    list-ref tail (
                                                      - length 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    list-set! tail length (
                                                      list-ref v j
                                                    )
                                                  )
                                                   (
                                                    set! length (
                                                      + length 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        idx (
                                                          ceil_index tail (
                                                            - 1
                                                          )
                                                           (
                                                            - length 1
                                                          )
                                                           (
                                                            list-ref v j
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        list-set! tail idx (
                                                          list-ref v j
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              set! j (
                                                + j 1
                                              )
                                            )
                                             (
                                              loop7
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
                                    loop7
                                  )
                                )
                              )
                            )
                             (
                              ret4 length
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
      define (
        main
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                example1 (
                  _list 2 5 3 7 11 8 10 13 6
                )
              )
            )
             (
              begin (
                let (
                  (
                    example2 (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        example3 (
                          _list 0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            example4 (
                              _list 5 4 3 2 1
                            )
                          )
                        )
                         (
                          begin (
                            _display (
                              if (
                                string? (
                                  longest_increasing_subsequence_length example1
                                )
                              )
                               (
                                longest_increasing_subsequence_length example1
                              )
                               (
                                to-str (
                                  longest_increasing_subsequence_length example1
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
                                string? (
                                  longest_increasing_subsequence_length example2
                                )
                              )
                               (
                                longest_increasing_subsequence_length example2
                              )
                               (
                                to-str (
                                  longest_increasing_subsequence_length example2
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
                                string? (
                                  longest_increasing_subsequence_length example3
                                )
                              )
                               (
                                longest_increasing_subsequence_length example3
                              )
                               (
                                to-str (
                                  longest_increasing_subsequence_length example3
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
                                string? (
                                  longest_increasing_subsequence_length example4
                                )
                              )
                               (
                                longest_increasing_subsequence_length example4
                              )
                               (
                                to-str (
                                  longest_increasing_subsequence_length example4
                                )
                              )
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
              )
            )
          )
        )
      )
    )
     (
      main
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
