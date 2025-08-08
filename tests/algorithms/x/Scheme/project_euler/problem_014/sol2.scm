;; Generated on 2025-08-07 16:45 +0700
(import (scheme base))
(import (scheme time))
(import (chibi string))
(import (only (scheme char) string-upcase string-downcase))
(import (srfi 69))
(import (srfi 1))
(define _list list)
(import (chibi io))
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
        ((number? x)
         (if (integer? x)
             (number->string (inexact->exact x))
             (number->string x)))
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
(define (_div a b) (if (and (integer? a) (integer? b) (exact? a) (exact? b)) (quotient a b) (/ a b)))
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
(define (list-ref-safe lst idx) (if (and (integer? idx) (>= idx 0) (< idx (length lst))) (list-ref lst idx) '()))
(define (_input)
  (let ((l (read-line)))
    (if (eof-object? l) "" l)))
(
  let (
    (
      start11 (
        current-jiffy
      )
    )
     (
      jps14 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          collatz_cache (
            alist->hash-table (
              _list (
                cons 1 1
              )
            )
          )
        )
      )
       (
        begin (
          define (
            collatz_length n
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                let (
                  (
                    num n
                  )
                )
                 (
                  begin (
                    let (
                      (
                        sequence (
                          _list
                        )
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
                                      not (
                                        cond (
                                          (
                                            string? collatz_cache
                                          )
                                           (
                                            if (
                                              string-contains collatz_cache num
                                            )
                                             #t #f
                                          )
                                        )
                                         (
                                          (
                                            hash-table? collatz_cache
                                          )
                                           (
                                            if (
                                              hash-table-exists? collatz_cache num
                                            )
                                             #t #f
                                          )
                                        )
                                         (
                                          else (
                                            if (
                                              member num collatz_cache
                                            )
                                             #t #f
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! sequence (
                                          append sequence (
                                            _list num
                                          )
                                        )
                                      )
                                       (
                                        if (
                                          equal? (
                                            _mod num 2
                                          )
                                           0
                                        )
                                         (
                                          begin (
                                            set! num (
                                              let (
                                                (
                                                  v4 (
                                                    _div num 2
                                                  )
                                                )
                                              )
                                               (
                                                cond (
                                                  (
                                                    string? v4
                                                  )
                                                   (
                                                    inexact->exact (
                                                      floor (
                                                        string->number v4
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    boolean? v4
                                                  )
                                                   (
                                                    if v4 1 0
                                                  )
                                                )
                                                 (
                                                  else (
                                                    inexact->exact (
                                                      floor v4
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! num (
                                              + (
                                                * 3 num
                                              )
                                               1
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop2
                                      )
                                    )
                                     '(
                                      
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
                            length (
                              hash-table-ref/default collatz_cache num '(
                                
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                i (
                                  - (
                                    _len sequence
                                  )
                                   1
                                )
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
                                              >= i 0
                                            )
                                             (
                                              begin (
                                                set! length (
                                                  + length 1
                                                )
                                              )
                                               (
                                                hash-table-set! collatz_cache (
                                                  list-ref-safe sequence i
                                                )
                                                 length
                                              )
                                               (
                                                set! i (
                                                  - i 1
                                                )
                                              )
                                               (
                                                loop5
                                              )
                                            )
                                             '(
                                              
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
                                ret1 length
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
            solution limit
          )
           (
            call/cc (
              lambda (
                ret7
              )
               (
                let (
                  (
                    max_len 0
                  )
                )
                 (
                  begin (
                    let (
                      (
                        max_start 1
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            i 1
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
                                          < i limit
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                length (
                                                  collatz_length i
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  _gt length max_len
                                                )
                                                 (
                                                  begin (
                                                    set! max_len length
                                                  )
                                                   (
                                                    set! max_start i
                                                  )
                                                )
                                                 '(
                                                  
                                                )
                                              )
                                               (
                                                set! i (
                                                  + i 1
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop8
                                          )
                                        )
                                         '(
                                          
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
                            ret7 max_start
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
          let (
            (
              input_str (
                _input
              )
            )
          )
           (
            begin (
              let (
                (
                  limit (
                    let (
                      (
                        v10 input_str
                      )
                    )
                     (
                      cond (
                        (
                          string? v10
                        )
                         (
                          exact (
                            floor (
                              string->number v10
                            )
                          )
                        )
                      )
                       (
                        (
                          boolean? v10
                        )
                         (
                          if v10 1 0
                        )
                      )
                       (
                        else (
                          exact (
                            floor v10
                          )
                        )
                      )
                    )
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        solution limit
                      )
                    )
                     (
                      solution limit
                    )
                     (
                      to-str (
                        solution limit
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
     (
      let (
        (
          end12 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur13 (
              quotient (
                * (
                  - end12 start11
                )
                 1000000
              )
               jps14
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur13
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
