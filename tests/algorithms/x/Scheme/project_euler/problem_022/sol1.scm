;; Generated on 2025-08-09 10:22 +0700
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
(define (_floor x) (cond ((string? x) (let ((n (string->number x))) (if n (floor n) 0))) ((boolean? x) (if x 1 0)) (else (floor x))))
(define (fmod a b) (- a (* (_floor (/ a b)) b)))
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
      start8 (
        current-jiffy
      )
    )
     (
      jps11 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        parse_names line
      )
       (
        let (
          (
            names (
              _list
            )
          )
        )
         (
          begin (
            let (
              (
                current ""
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
                    letrec (
                      (
                        loop1 (
                          lambda (
                            
                          )
                           (
                            if (
                              < i (
                                _len line
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    ch (
                                      _substring line i (
                                        + i 1
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? ch ","
                                    )
                                     (
                                      begin (
                                        set! names (
                                          append names (
                                            _list current
                                          )
                                        )
                                      )
                                       (
                                        set! current ""
                                      )
                                    )
                                     (
                                      if (
                                        not (
                                          equal? ch "\""
                                        )
                                      )
                                       (
                                        begin (
                                          set! current (
                                            string-append current ch
                                          )
                                        )
                                      )
                                       '(
                                        
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
                                loop1
                              )
                            )
                             '(
                              
                            )
                          )
                        )
                      )
                    )
                     (
                      loop1
                    )
                  )
                   (
                    set! names (
                      append names (
                        _list current
                      )
                    )
                  )
                   names
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        insertion_sort arr
      )
       (
        let (
          (
            a arr
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
                letrec (
                  (
                    loop2 (
                      lambda (
                        
                      )
                       (
                        if (
                          < i (
                            _len a
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                key (
                                  list-ref-safe a i
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    j (
                                      - i 1
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    letrec (
                                      (
                                        loop3 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              and (
                                                >= j 0
                                              )
                                               (
                                                string>? (
                                                  list-ref-safe a j
                                                )
                                                 key
                                              )
                                            )
                                             (
                                              begin (
                                                list-set! a (
                                                  + j 1
                                                )
                                                 (
                                                  list-ref-safe a j
                                                )
                                              )
                                               (
                                                set! j (
                                                  - j 1
                                                )
                                              )
                                               (
                                                loop3
                                              )
                                            )
                                             '(
                                              
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      loop3
                                    )
                                  )
                                   (
                                    list-set! a (
                                      + j 1
                                    )
                                     key
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
               a
            )
          )
        )
      )
    )
     (
      define (
        letter_value ch
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              )
            )
             (
              begin (
                let (
                  (
                    idx 0
                  )
                )
                 (
                  begin (
                    letrec (
                      (
                        loop5 (
                          lambda (
                            
                          )
                           (
                            if (
                              < idx (
                                _len alphabet
                              )
                            )
                             (
                              begin (
                                if (
                                  equal? (
                                    _substring alphabet idx (
                                      + idx 1
                                    )
                                  )
                                   ch
                                )
                                 (
                                  begin (
                                    ret4 (
                                      + idx 1
                                    )
                                  )
                                )
                                 '(
                                  
                                )
                              )
                               (
                                set! idx (
                                  + idx 1
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
     (
      define (
        name_score name
      )
       (
        let (
          (
            score 0
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
                letrec (
                  (
                    loop6 (
                      lambda (
                        
                      )
                       (
                        if (
                          < i (
                            _len name
                          )
                        )
                         (
                          begin (
                            set! score (
                              _add score (
                                letter_value (
                                  _substring name i (
                                    + i 1
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
               score
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
        let (
          (
            line (
              _input
            )
          )
        )
         (
          begin (
            let (
              (
                names (
                  insertion_sort (
                    parse_names line
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    total 0
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
                        letrec (
                          (
                            loop7 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len names
                                  )
                                )
                                 (
                                  begin (
                                    set! total (
                                      _add total (
                                        * (
                                          + i 1
                                        )
                                         (
                                          name_score (
                                            cond (
                                              (
                                                string? names
                                              )
                                               (
                                                _substring names i (
                                                  + i 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? names
                                              )
                                               (
                                                hash-table-ref names i
                                              )
                                            )
                                             (
                                              else (
                                                list-ref-safe names i
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
                                   (
                                    loop7
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop7
                        )
                      )
                       (
                        _display (
                          if (
                            string? (
                              to-str-space total
                            )
                          )
                           (
                            to-str-space total
                          )
                           (
                            to-str (
                              to-str-space total
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
     (
      main
    )
     (
      let (
        (
          end9 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur10 (
              quotient (
                * (
                  - end9 start8
                )
                 1000000
              )
               jps11
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur10
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
