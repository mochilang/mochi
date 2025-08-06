;; Generated on 2025-08-06 23:57 +0700
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
        to_binary n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                equal? n 0
              )
               (
                begin (
                  ret1 "0"
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
                  num n
                )
              )
               (
                begin (
                  let (
                    (
                      res ""
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
                                    > num 0
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          bit (
                                            _mod num 2
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          set! res (
                                            string-append (
                                              to-str-space bit
                                            )
                                             res
                                          )
                                        )
                                         (
                                          set! num (
                                            _div num 2
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
                      ret1 res
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
        contains_key_int m key
      )
       (
        call/cc (
          lambda (
            ret4
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
                          xs
                        )
                         (
                          if (
                            null? xs
                          )
                           (
                            quote (
                              
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  k (
                                    car xs
                                  )
                                )
                              )
                               (
                                begin (
                                  if (
                                    string=? k key
                                  )
                                   (
                                    begin (
                                      ret4 #t
                                    )
                                  )
                                   (
                                    quote (
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop5 (
                                cdr xs
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                   (
                    loop5 (
                      hash-table-keys m
                    )
                  )
                )
              )
            )
             (
              ret4 #f
            )
          )
        )
      )
    )
     (
      define (
        lzw_compress bits
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                dict (
                  alist->hash-table (
                    _list (
                      cons "0" 0
                    )
                     (
                      cons "1" 1
                    )
                  )
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
                        result ""
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            index 2
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
                                              < i (
                                                _len bits
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    ch (
                                                      _substring bits i (
                                                        + i 1
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        candidate (
                                                          string-append current ch
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          contains_key_int dict candidate
                                                        )
                                                         (
                                                          begin (
                                                            set! current candidate
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! result (
                                                              string-append result (
                                                                to_binary (
                                                                  hash-table-ref/default dict current (
                                                                    quote (
                                                                      
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            hash-table-set! dict candidate index
                                                          )
                                                           (
                                                            set! index (
                                                              + index 1
                                                            )
                                                          )
                                                           (
                                                            set! current ch
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
                                if (
                                  not (
                                    string=? current ""
                                  )
                                )
                                 (
                                  begin (
                                    set! result (
                                      string-append result (
                                        to_binary (
                                          hash-table-ref/default dict current (
                                            quote (
                                              
                                            )
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
                                ret7 result
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
      let (
        (
          data "01001100100111"
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                lzw_compress data
              )
            )
             (
              lzw_compress data
            )
             (
              to-str (
                lzw_compress data
              )
            )
          )
        )
         (
          newline
        )
      )
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
