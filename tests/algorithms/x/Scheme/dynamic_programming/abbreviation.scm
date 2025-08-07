;; Generated on 2025-08-07 08:20 +0700
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
      start18 (
        current-jiffy
      )
    )
     (
      jps21 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        index_of s ch
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
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
                                _len s
                              )
                            )
                             (
                              begin (
                                if (
                                  string=? (
                                    _substring s i (
                                      + i 1
                                    )
                                  )
                                   ch
                                )
                                 (
                                  begin (
                                    ret1 i
                                  )
                                )
                                 (
                                  quote (
                                    
                                  )
                                )
                              )
                               (
                                set! i (
                                  + i 1
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
                ret1 (
                  - 1
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        ord ch
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              )
            )
             (
              begin (
                let (
                  (
                    lower "abcdefghijklmnopqrstuvwxyz"
                  )
                )
                 (
                  begin (
                    let (
                      (
                        idx (
                          index_of upper ch
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          _ge idx 0
                        )
                         (
                          begin (
                            ret4 (
                              _add 65 idx
                            )
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        set! idx (
                          index_of lower ch
                        )
                      )
                       (
                        if (
                          _ge idx 0
                        )
                         (
                          begin (
                            ret4 (
                              _add 97 idx
                            )
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        ret4 0
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
        chr n
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              )
            )
             (
              begin (
                let (
                  (
                    lower "abcdefghijklmnopqrstuvwxyz"
                  )
                )
                 (
                  begin (
                    if (
                      and (
                        >= n 65
                      )
                       (
                        < n 91
                      )
                    )
                     (
                      begin (
                        ret5 (
                          _substring upper (
                            - n 65
                          )
                           (
                            - n 64
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
                    if (
                      and (
                        >= n 97
                      )
                       (
                        < n 123
                      )
                    )
                     (
                      begin (
                        ret5 (
                          _substring lower (
                            - n 97
                          )
                           (
                            - n 96
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
                    ret5 "?"
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
        to_upper_char c
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                code (
                  ord c
                )
              )
            )
             (
              begin (
                if (
                  and (
                    _ge code 97
                  )
                   (
                    _le code 122
                  )
                )
                 (
                  begin (
                    ret6 (
                      chr (
                        - code 32
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
                ret6 c
              )
            )
          )
        )
      )
    )
     (
      define (
        is_lower c
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                code (
                  ord c
                )
              )
            )
             (
              begin (
                ret7 (
                  and (
                    _ge code 97
                  )
                   (
                    _le code 122
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
        abbr a b
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                n (
                  _len a
                )
              )
            )
             (
              begin (
                let (
                  (
                    m (
                      _len b
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
                                break10
                              )
                               (
                                letrec (
                                  (
                                    loop9 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          <= i n
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
                                                        break12
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop11 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  <= j m
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! row (
                                                                      append row (
                                                                        _list #f
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! j (
                                                                      + j 1
                                                                    )
                                                                  )
                                                                   (
                                                                    loop11
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
                                                          loop11
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
                                            loop9
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
                                  loop9
                                )
                              )
                            )
                          )
                           (
                            list-set! (
                              list-ref dp 0
                            )
                             0 #t
                          )
                           (
                            set! i 0
                          )
                           (
                            call/cc (
                              lambda (
                                break14
                              )
                               (
                                letrec (
                                  (
                                    loop13 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i n
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
                                                    break16
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop15 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              <= j m
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  cond (
                                                                    (
                                                                      string? (
                                                                        list-ref dp i
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref dp i
                                                                      )
                                                                       j (
                                                                        + j 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        list-ref dp i
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref dp i
                                                                      )
                                                                       j
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref (
                                                                        list-ref dp i
                                                                      )
                                                                       j
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      and (
                                                                        < j m
                                                                      )
                                                                       (
                                                                        string=? (
                                                                          to_upper_char (
                                                                            _substring a i (
                                                                              + i 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring b j (
                                                                            + j 1
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        list-set! (
                                                                          list-ref dp (
                                                                            + i 1
                                                                          )
                                                                        )
                                                                         (
                                                                          + j 1
                                                                        )
                                                                         #t
                                                                      )
                                                                    )
                                                                     (
                                                                      quote (
                                                                        
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    if (
                                                                      is_lower (
                                                                        _substring a i (
                                                                          + i 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        list-set! (
                                                                          list-ref dp (
                                                                            + i 1
                                                                          )
                                                                        )
                                                                         j #t
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
                                                                set! j (
                                                                  + j 1
                                                                )
                                                              )
                                                               (
                                                                loop15
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
                                                      loop15
                                                    )
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
                                           (
                                            loop13
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
                                  loop13
                                )
                              )
                            )
                          )
                           (
                            ret8 (
                              cond (
                                (
                                  string? (
                                    list-ref dp n
                                  )
                                )
                                 (
                                  _substring (
                                    list-ref dp n
                                  )
                                   m (
                                    + m 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? (
                                    list-ref dp n
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    list-ref dp n
                                  )
                                   m
                                )
                              )
                               (
                                else (
                                  list-ref (
                                    list-ref dp n
                                  )
                                   m
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
      define (
        print_bool b
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            if b (
              begin (
                _display (
                  if (
                    string? (
                      if #t #t #f
                    )
                  )
                   (
                    if #t #t #f
                  )
                   (
                    to-str (
                      if #t #t #f
                    )
                  )
                )
              )
               (
                newline
              )
            )
             (
              begin (
                _display (
                  if (
                    string? (
                      if #f #t #f
                    )
                  )
                   (
                    if #f #t #f
                  )
                   (
                    to-str (
                      if #f #t #f
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
     (
      print_bool (
        abbr "daBcd" "ABC"
      )
    )
     (
      print_bool (
        abbr "dBcd" "ABC"
      )
    )
     (
      let (
        (
          end19 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur20 (
              quotient (
                * (
                  - end19 start18
                )
                 1000000
              )
               jps21
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur20
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
