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
      start15 (
        current-jiffy
      )
    )
     (
      jps18 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        list_contains xs v
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
                                _len xs
                              )
                            )
                             (
                              begin (
                                if (
                                  string=? (
                                    list-ref xs i
                                  )
                                   v
                                )
                                 (
                                  begin (
                                    ret1 #t
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
                ret1 #f
              )
            )
          )
        )
      )
    )
     (
      define (
        is_power_of_two n
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                < n 1
              )
               (
                begin (
                  ret4 #f
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
                  x n
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
                                > x 1
                              )
                               (
                                begin (
                                  if (
                                    not (
                                      equal? (
                                        _mod x 2
                                      )
                                       0
                                    )
                                  )
                                   (
                                    begin (
                                      ret4 #f
                                    )
                                  )
                                   (
                                    quote (
                                      
                                    )
                                  )
                                )
                                 (
                                  set! x (
                                    _div x 2
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
                  ret4 #t
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        bin_string n
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              if (
                equal? n 0
              )
               (
                begin (
                  ret7 "0"
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
                      x n
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
                                    > x 0
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          bit (
                                            _mod x 2
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
                                          set! x (
                                            _div x 2
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
                      ret7 res
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
        decompress_data data_bits
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                lexicon (
                  alist->hash-table (
                    _list (
                      cons "0" "0"
                    )
                     (
                      cons "1" "1"
                    )
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    keys (
                      _list "0" "1"
                    )
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
                            curr_string ""
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
                                                  < i (
                                                    _len data_bits
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! curr_string (
                                                      string-append curr_string (
                                                        _substring data_bits i (
                                                          + i 1
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    if (
                                                      not (
                                                        list_contains keys curr_string
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! i (
                                                          + i 1
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
                                                   (
                                                    let (
                                                      (
                                                        last_match_id (
                                                          hash-table-ref/default lexicon curr_string (
                                                            quote (
                                                              
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! result (
                                                          string-append result last_match_id
                                                        )
                                                      )
                                                       (
                                                        hash-table-set! lexicon curr_string (
                                                          string-append last_match_id "0"
                                                        )
                                                      )
                                                       (
                                                        if (
                                                          is_power_of_two index
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                new_lex (
                                                                  alist->hash-table (
                                                                    _list
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    new_keys (
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
                                                                                      < j (
                                                                                        _len keys
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            curr_key (
                                                                                              list-ref keys j
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            hash-table-set! new_lex (
                                                                                              string-append "0" curr_key
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref/default lexicon curr_key (
                                                                                                quote (
                                                                                                  
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! new_keys (
                                                                                              append new_keys (
                                                                                                _list (
                                                                                                  string-append "0" curr_key
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! j (
                                                                                              + j 1
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
                                                                        set! lexicon new_lex
                                                                      )
                                                                       (
                                                                        set! keys new_keys
                                                                      )
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
                                                        let (
                                                          (
                                                            new_key (
                                                              bin_string index
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            hash-table-set! lexicon new_key (
                                                              string-append last_match_id "1"
                                                            )
                                                          )
                                                           (
                                                            set! keys (
                                                              append keys (
                                                                _list new_key
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! index (
                                                              + index 1
                                                            )
                                                          )
                                                           (
                                                            set! curr_string ""
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
                                    ret10 result
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
      let (
        (
          sample "1011001"
        )
      )
       (
        begin (
          let (
            (
              decompressed (
                decompress_data sample
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? decompressed
                )
                 decompressed (
                  to-str decompressed
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
      let (
        (
          end16 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur17 (
              quotient (
                * (
                  - end16 start15
                )
                 1000000
              )
               jps18
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur17
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
