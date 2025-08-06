;; Generated on 2025-08-06 21:38 +0700
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
      start9 (
        current-jiffy
      )
    )
     (
      jps12 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        nor_gate input_1 input_2
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                and (
                  equal? input_1 0
                )
                 (
                  equal? input_2 0
                )
              )
               (
                begin (
                  ret1 1
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret1 0
            )
          )
        )
      )
    )
     (
      define (
        center s width
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                total (
                  - width (
                    _len s
                  )
                )
              )
            )
             (
              begin (
                if (
                  <= total 0
                )
                 (
                  begin (
                    ret2 s
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
                    left (
                      quotient total 2
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        right (
                          - total left
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            res s
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
                                    break4
                                  )
                                   (
                                    letrec (
                                      (
                                        loop3 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i left
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  string-append " " res
                                                )
                                              )
                                               (
                                                set! i (
                                                  + i 1
                                                )
                                              )
                                               (
                                                loop3
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
                                      loop3
                                    )
                                  )
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
                                                  < j right
                                                )
                                                 (
                                                  begin (
                                                    set! res (
                                                      string-append res " "
                                                    )
                                                  )
                                                   (
                                                    set! j (
                                                      + j 1
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
                                    ret2 res
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
        make_table_row i j
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                output (
                  nor_gate i j
                )
              )
            )
             (
              begin (
                ret7 (
                  string-append (
                    string-append (
                      string-append (
                        string-append (
                          string-append (
                            string-append "| " (
                              center (
                                to-str-space i
                              )
                               8
                            )
                          )
                           " | "
                        )
                         (
                          center (
                            to-str-space j
                          )
                           8
                        )
                      )
                       " | "
                    )
                     (
                      center (
                        to-str-space output
                      )
                       8
                    )
                  )
                   " |"
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        truth_table
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            ret8 (
              string-append (
                string-append (
                  string-append (
                    string-append (
                      string-append (
                        string-append (
                          string-append "Truth Table of NOR Gate:\n| Input 1 | Input 2 | Output  |\n" (
                            make_table_row 0 0
                          )
                        )
                         "\n"
                      )
                       (
                        make_table_row 0 1
                      )
                    )
                     "\n"
                  )
                   (
                    make_table_row 1 0
                  )
                )
                 "\n"
              )
               (
                make_table_row 1 1
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
            nor_gate 0 0
          )
        )
         (
          nor_gate 0 0
        )
         (
          to-str (
            nor_gate 0 0
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
            nor_gate 0 1
          )
        )
         (
          nor_gate 0 1
        )
         (
          to-str (
            nor_gate 0 1
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
            nor_gate 1 0
          )
        )
         (
          nor_gate 1 0
        )
         (
          to-str (
            nor_gate 1 0
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
            nor_gate 1 1
          )
        )
         (
          nor_gate 1 1
        )
         (
          to-str (
            nor_gate 1 1
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
            truth_table
          )
        )
         (
          truth_table
        )
         (
          to-str (
            truth_table
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
          end10 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur11 (
              quotient (
                * (
                  - end10 start9
                )
                 1000000
              )
               jps12
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur11
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
