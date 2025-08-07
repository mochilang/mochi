;; Generated on 2025-08-07 14:57 +0700
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
(
  let (
    (
      start12 (
        current-jiffy
      )
    )
     (
      jps15 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        rank_of_matrix matrix
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                rows (
                  _len matrix
                )
              )
            )
             (
              begin (
                if (
                  equal? rows 0
                )
                 (
                  begin (
                    ret1 0
                  )
                )
                 '(
                  
                )
              )
               (
                let (
                  (
                    columns (
                      if (
                        > (
                          _len (
                            list-ref-safe matrix 0
                          )
                        )
                         0
                      )
                       (
                        _len (
                          list-ref-safe matrix 0
                        )
                      )
                       0
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        rank (
                          if (
                            < rows columns
                          )
                           rows columns
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            row 0
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
                                          < row rank
                                        )
                                         (
                                          begin (
                                            if (
                                              not (
                                                equal? (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref-safe matrix row
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref-safe matrix row
                                                      )
                                                       row (
                                                        + row 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref-safe matrix row
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref-safe matrix row
                                                      )
                                                       row
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref-safe (
                                                        list-ref-safe matrix row
                                                      )
                                                       row
                                                    )
                                                  )
                                                )
                                                 0.0
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    col (
                                                      + row 1
                                                    )
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
                                                                  < col rows
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        mult (
                                                                          _div (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref-safe matrix col
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref-safe matrix col
                                                                                )
                                                                                 row (
                                                                                  + row 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref-safe matrix col
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref-safe matrix col
                                                                                )
                                                                                 row
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe (
                                                                                  list-ref-safe matrix col
                                                                                )
                                                                                 row
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref-safe matrix row
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref-safe matrix row
                                                                                )
                                                                                 row (
                                                                                  + row 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref-safe matrix row
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref-safe matrix row
                                                                                )
                                                                                 row
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe (
                                                                                  list-ref-safe matrix row
                                                                                )
                                                                                 row
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            i row
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
                                                                                          < i columns
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            list-set! (
                                                                                              list-ref-safe matrix col
                                                                                            )
                                                                                             i (
                                                                                              - (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? (
                                                                                                      list-ref-safe matrix col
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    _substring (
                                                                                                      list-ref-safe matrix col
                                                                                                    )
                                                                                                     i (
                                                                                                      + i 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  (
                                                                                                    hash-table? (
                                                                                                      list-ref-safe matrix col
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    hash-table-ref (
                                                                                                      list-ref-safe matrix col
                                                                                                    )
                                                                                                     i
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    list-ref-safe (
                                                                                                      list-ref-safe matrix col
                                                                                                    )
                                                                                                     i
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                * mult (
                                                                                                  cond (
                                                                                                    (
                                                                                                      string? (
                                                                                                        list-ref-safe matrix row
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      _substring (
                                                                                                        list-ref-safe matrix row
                                                                                                      )
                                                                                                       i (
                                                                                                        + i 1
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    (
                                                                                                      hash-table? (
                                                                                                        list-ref-safe matrix row
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      hash-table-ref (
                                                                                                        list-ref-safe matrix row
                                                                                                      )
                                                                                                       i
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    else (
                                                                                                      list-ref-safe (
                                                                                                        list-ref-safe matrix row
                                                                                                      )
                                                                                                       i
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! i (
                                                                                              + i 1
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            loop6
                                                                                          )
                                                                                        )
                                                                                         '(
                                                                                          
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
                                                                            set! col (
                                                                              + col 1
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
                                                                 '(
                                                                  
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
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    reduce #t
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        i (
                                                          + row 1
                                                        )
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
                                                                      < i rows
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          not (
                                                                            equal? (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref-safe matrix i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref-safe matrix i
                                                                                  )
                                                                                   row (
                                                                                    + row 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref-safe matrix i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref-safe matrix i
                                                                                  )
                                                                                   row
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref-safe (
                                                                                    list-ref-safe matrix i
                                                                                  )
                                                                                   row
                                                                                )
                                                                              )
                                                                            )
                                                                             0.0
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                temp (
                                                                                  list-ref-safe matrix row
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                list-set! matrix row (
                                                                                  list-ref-safe matrix i
                                                                                )
                                                                              )
                                                                               (
                                                                                list-set! matrix i temp
                                                                              )
                                                                               (
                                                                                set! reduce #f
                                                                              )
                                                                               (
                                                                                break9 '(
                                                                                  
                                                                                )
                                                                              )
                                                                            )
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
                                                        if reduce (
                                                          begin (
                                                            set! rank (
                                                              - rank 1
                                                            )
                                                          )
                                                           (
                                                            let (
                                                              (
                                                                j 0
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                call/cc (
                                                                  lambda (
                                                                    break11
                                                                  )
                                                                   (
                                                                    letrec (
                                                                      (
                                                                        loop10 (
                                                                          lambda (
                                                                            
                                                                          )
                                                                           (
                                                                            if (
                                                                              < j rows
                                                                            )
                                                                             (
                                                                              begin (
                                                                                list-set! (
                                                                                  list-ref-safe matrix j
                                                                                )
                                                                                 row (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref-safe matrix j
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref-safe matrix j
                                                                                      )
                                                                                       rank (
                                                                                        + rank 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref-safe matrix j
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref-safe matrix j
                                                                                      )
                                                                                       rank
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref-safe (
                                                                                        list-ref-safe matrix j
                                                                                      )
                                                                                       rank
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
                                                                                loop10
                                                                              )
                                                                            )
                                                                             '(
                                                                              
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      loop10
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         '(
                                                          
                                                        )
                                                      )
                                                       (
                                                        set! row (
                                                          - row 1
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! row (
                                              + row 1
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
                            ret1 rank
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
      let (
        (
          end13 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur14 (
              quotient (
                * (
                  - end13 start12
                )
                 1000000
              )
               jps15
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur14
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
