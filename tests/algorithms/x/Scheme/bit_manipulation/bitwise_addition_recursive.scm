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
        bitwise_xor a b
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                result 0
              )
            )
             (
              begin (
                let (
                  (
                    bit 1
                  )
                )
                 (
                  begin (
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
                                          or (
                                            > x 0
                                          )
                                           (
                                            > y 0
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                ax (
                                                  modulo x 2
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    by (
                                                      modulo y 2
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      equal? (
                                                        modulo (
                                                          + ax by
                                                        )
                                                         2
                                                      )
                                                       1
                                                    )
                                                     (
                                                      begin (
                                                        set! result (
                                                          + result bit
                                                        )
                                                      )
                                                    )
                                                     (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! x (
                                                      quotient x 2
                                                    )
                                                  )
                                                   (
                                                    set! y (
                                                      quotient y 2
                                                    )
                                                  )
                                                   (
                                                    set! bit (
                                                      * bit 2
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
                            ret1 result
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
        bitwise_and a b
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                result 0
              )
            )
             (
              begin (
                let (
                  (
                    bit 1
                  )
                )
                 (
                  begin (
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
                                          and (
                                            > x 0
                                          )
                                           (
                                            > y 0
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              and (
                                                equal? (
                                                  modulo x 2
                                                )
                                                 1
                                              )
                                               (
                                                equal? (
                                                  modulo y 2
                                                )
                                                 1
                                              )
                                            )
                                             (
                                              begin (
                                                set! result (
                                                  + result bit
                                                )
                                              )
                                            )
                                             (
                                              quote (
                                                
                                              )
                                            )
                                          )
                                           (
                                            set! x (
                                              quotient x 2
                                            )
                                          )
                                           (
                                            set! y (
                                              quotient y 2
                                            )
                                          )
                                           (
                                            set! bit (
                                              * bit 2
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
                            ret4 result
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
        bitwise_addition_recursive number other_number
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              if (
                or (
                  < number 0
                )
                 (
                  < other_number 0
                )
              )
               (
                begin (
                  panic "Both arguments MUST be non-negative!"
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
                  bitwise_sum (
                    bitwise_xor number other_number
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      carry (
                        bitwise_and number other_number
                      )
                    )
                  )
                   (
                    begin (
                      if (
                        equal? carry 0
                      )
                       (
                        begin (
                          ret7 bitwise_sum
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret7 (
                        bitwise_addition_recursive bitwise_sum (
                          * carry 2
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
            to-str-space (
              bitwise_addition_recursive 4 5
            )
          )
        )
         (
          to-str-space (
            bitwise_addition_recursive 4 5
          )
        )
         (
          to-str (
            to-str-space (
              bitwise_addition_recursive 4 5
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
              bitwise_addition_recursive 8 9
            )
          )
        )
         (
          to-str-space (
            bitwise_addition_recursive 8 9
          )
        )
         (
          to-str (
            to-str-space (
              bitwise_addition_recursive 8 9
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
              bitwise_addition_recursive 0 4
            )
          )
        )
         (
          to-str-space (
            bitwise_addition_recursive 0 4
          )
        )
         (
          to-str (
            to-str-space (
              bitwise_addition_recursive 0 4
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
