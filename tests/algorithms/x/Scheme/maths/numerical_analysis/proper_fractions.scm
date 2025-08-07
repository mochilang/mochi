;; Generated on 2025-08-07 10:06 +0700
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
        gcd a b
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                x a
              )
            )
             (
              begin (
                let (
                  (
                    y b
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
                                    equal? y 0
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        t (
                                          _mod x y
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! x y
                                      )
                                       (
                                        set! y t
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
                    if (
                      < x 0
                    )
                     (
                      begin (
                        ret1 (
                          - x
                        )
                      )
                    )
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
                    ret1 x
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
        proper_fractions den
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                < den 0
              )
               (
                begin (
                  panic "The Denominator Cannot be less than 0"
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
                  res (
                    _list
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      n 1
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
                                    < n den
                                  )
                                   (
                                    begin (
                                      if (
                                        equal? (
                                          gcd n den
                                        )
                                         1
                                      )
                                       (
                                        begin (
                                          set! res (
                                            append res (
                                              _list (
                                                string-append (
                                                  string-append (
                                                    to-str-space n
                                                  )
                                                   "/"
                                                )
                                                 (
                                                  to-str-space den
                                                )
                                              )
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
                                      set! n (
                                        + n 1
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
        test_proper_fractions
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                a (
                  proper_fractions 10
                )
              )
            )
             (
              begin (
                if (
                  not (
                    equal? a (
                      _list "1/10" "3/10" "7/10" "9/10"
                    )
                  )
                )
                 (
                  begin (
                    panic "test 10 failed"
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
                    b (
                      proper_fractions 5
                    )
                  )
                )
                 (
                  begin (
                    if (
                      not (
                        equal? b (
                          _list "1/5" "2/5" "3/5" "4/5"
                        )
                      )
                    )
                     (
                      begin (
                        panic "test 5 failed"
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
                          proper_fractions 0
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          not (
                            equal? c (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            panic "test 0 failed"
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
            ret8
          )
           (
            begin (
              test_proper_fractions
            )
             (
              _display (
                if (
                  string? (
                    to-str-space (
                      proper_fractions 10
                    )
                  )
                )
                 (
                  to-str-space (
                    proper_fractions 10
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      proper_fractions 10
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
                    to-str-space (
                      proper_fractions 5
                    )
                  )
                )
                 (
                  to-str-space (
                    proper_fractions 5
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      proper_fractions 5
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
                    to-str-space (
                      proper_fractions 0
                    )
                  )
                )
                 (
                  to-str-space (
                    proper_fractions 0
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      proper_fractions 0
                    )
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
