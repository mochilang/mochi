;; Generated on 2025-08-07 13:41 +0700
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
(define (_div a b) (/ a b))
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
        parse_decimal s
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                value 0
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
                                    _len s
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        c (
                                          _substring s i (
                                            + i 1
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          or (
                                            string<? c "0"
                                          )
                                           (
                                            string>? c "9"
                                          )
                                        )
                                         (
                                          begin (
                                            panic "invalid literal"
                                          )
                                        )
                                         (
                                          quote (
                                            
                                          )
                                        )
                                      )
                                       (
                                        set! value (
                                          + (
                                            * value 10
                                          )
                                           (
                                            let (
                                              (
                                                v4 c
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
                                        set! i (
                                          + i 1
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
                    ret1 value
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
        zeller_day date_input
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                days (
                  alist->hash-table (
                    _list (
                      cons 0 "Sunday"
                    )
                     (
                      cons 1 "Monday"
                    )
                     (
                      cons 2 "Tuesday"
                    )
                     (
                      cons 3 "Wednesday"
                    )
                     (
                      cons 4 "Thursday"
                    )
                     (
                      cons 5 "Friday"
                    )
                     (
                      cons 6 "Saturday"
                    )
                  )
                )
              )
            )
             (
              begin (
                if (
                  not (
                    equal? (
                      _len date_input
                    )
                     10
                  )
                )
                 (
                  begin (
                    panic "Must be 10 characters long"
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
                      parse_decimal (
                        _substring date_input 0 2
                      )
                    )
                  )
                )
                 (
                  begin (
                    if (
                      or (
                        _le m 0
                      )
                       (
                        _ge m 13
                      )
                    )
                     (
                      begin (
                        panic "Month must be between 1 - 12"
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
                        sep1 (
                          _substring date_input 2 (
                            + 2 1
                          )
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          and (
                            not (
                              string=? sep1 "-"
                            )
                          )
                           (
                            not (
                              string=? sep1 "/"
                            )
                          )
                        )
                         (
                          begin (
                            panic "Date separator must be '-' or '/'"
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
                            d (
                              parse_decimal (
                                _substring date_input 3 5
                              )
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              or (
                                _le d 0
                              )
                               (
                                _ge d 32
                              )
                            )
                             (
                              begin (
                                panic "Date must be between 1 - 31"
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
                                sep2 (
                                  _substring date_input 5 (
                                    + 5 1
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                if (
                                  and (
                                    not (
                                      string=? sep2 "-"
                                    )
                                  )
                                   (
                                    not (
                                      string=? sep2 "/"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    panic "Date separator must be '-' or '/'"
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
                                    y (
                                      parse_decimal (
                                        _substring date_input 6 10
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      or (
                                        _le y 45
                                      )
                                       (
                                        _ge y 8500
                                      )
                                    )
                                     (
                                      begin (
                                        panic "Year out of range. There has to be some sort of limit...right?"
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
                                        year y
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            month m
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              _le month 2
                                            )
                                             (
                                              begin (
                                                set! year (
                                                  - year 1
                                                )
                                              )
                                               (
                                                set! month (
                                                  _add month 12
                                                )
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
                                                c (
                                                  _div year 100
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    k (
                                                      _mod year 100
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        t (
                                                          let (
                                                            (
                                                              v6 (
                                                                - (
                                                                  * 2.6 (
                                                                    + 0.0 month
                                                                  )
                                                                )
                                                                 5.39
                                                              )
                                                            )
                                                          )
                                                           (
                                                            cond (
                                                              (
                                                                string? v6
                                                              )
                                                               (
                                                                exact (
                                                                  floor (
                                                                    string->number v6
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                boolean? v6
                                                              )
                                                               (
                                                                if v6 1 0
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                exact (
                                                                  floor v6
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
                                                            u (
                                                              _div c 4
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                v (
                                                                  _div k 4
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    x (
                                                                      _add d k
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        z (
                                                                          _add (
                                                                            _add (
                                                                              _add t u
                                                                            )
                                                                             v
                                                                          )
                                                                           x
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            w (
                                                                              - z (
                                                                                * 2 c
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                f (
                                                                                  _mod w 7
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                if (
                                                                                  _lt f 0
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! f (
                                                                                      _add f 7
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  quote (
                                                                                    
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                ret5 (
                                                                                  hash-table-ref/default days f (
                                                                                    quote (
                                                                                      
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
    )
     (
      define (
        zeller date_input
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                day (
                  zeller_day date_input
                )
              )
            )
             (
              begin (
                ret7 (
                  string-append (
                    string-append (
                      string-append (
                        string-append "Your date " date_input
                      )
                       ", is a "
                    )
                     day
                  )
                   "!"
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        test_zeller
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                inputs (
                  _list "01-31-2010" "02-01-2010" "11-26-2024" "07-04-1776"
                )
              )
            )
             (
              begin (
                let (
                  (
                    expected (
                      _list "Sunday" "Monday" "Tuesday" "Thursday"
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
                                      < i (
                                        _len inputs
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            res (
                                              zeller_day (
                                                list-ref inputs i
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              not (
                                                string=? res (
                                                  list-ref expected i
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                panic "zeller test failed"
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
            ret11
          )
           (
            begin (
              test_zeller
            )
             (
              _display (
                if (
                  string? (
                    zeller "01-31-2010"
                  )
                )
                 (
                  zeller "01-31-2010"
                )
                 (
                  to-str (
                    zeller "01-31-2010"
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
     (
      main
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
