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
      define (
        repeat_char ch times
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                res ""
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
                                  < i times
                                )
                                 (
                                  begin (
                                    set! res (
                                      string-append res ch
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
                    ret1 res
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
        to_binary n
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                equal? n 0
              )
               (
                begin (
                  ret4 "0"
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
                  res ""
                )
              )
               (
                begin (
                  let (
                    (
                      v n
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
                                    > v 0
                                  )
                                   (
                                    begin (
                                      set! res (
                                        string-append (
                                          to-str-space (
                                            modulo v 2
                                          )
                                        )
                                         res
                                      )
                                    )
                                     (
                                      set! v (
                                        quotient v 2
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
                      ret4 res
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
        pow2 exp
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                res 1
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
                                  < i exp
                                )
                                 (
                                  begin (
                                    set! res (
                                      * res 2
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
                    ret7 res
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
        twos_complement number
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            begin (
              if (
                > number 0
              )
               (
                begin (
                  panic "input must be a negative integer"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                equal? number 0
              )
               (
                begin (
                  ret10 "0b0"
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
                  abs_number (
                    if (
                      < number 0
                    )
                     (
                      - number
                    )
                     number
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      binary_number_length (
                        _len (
                          to_binary abs_number
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          complement_value (
                            - (
                              pow2 binary_number_length
                            )
                             abs_number
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              complement_binary (
                                to_binary complement_value
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  padding (
                                    repeat_char "0" (
                                      - binary_number_length (
                                        _len complement_binary
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      twos_complement_number (
                                        string-append (
                                          string-append "1" padding
                                        )
                                         complement_binary
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      ret10 (
                                        string-append "0b" twos_complement_number
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
            twos_complement 0
          )
        )
         (
          twos_complement 0
        )
         (
          to-str (
            twos_complement 0
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
            twos_complement (
              - 1
            )
          )
        )
         (
          twos_complement (
            - 1
          )
        )
         (
          to-str (
            twos_complement (
              - 1
            )
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
            twos_complement (
              - 5
            )
          )
        )
         (
          twos_complement (
            - 5
          )
        )
         (
          to-str (
            twos_complement (
              - 5
            )
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
            twos_complement (
              - 17
            )
          )
        )
         (
          twos_complement (
            - 17
          )
        )
         (
          to-str (
            twos_complement (
              - 17
            )
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
            twos_complement (
              - 207
            )
          )
        )
         (
          twos_complement (
            - 207
          )
        )
         (
          to-str (
            twos_complement (
              - 207
            )
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
