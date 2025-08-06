;; Generated on 2025-08-06 22:04 +0700
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
      start14 (
        current-jiffy
      )
    )
     (
      jps17 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          default_alphabet "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
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
            encrypt input_string key alphabet
          )
           (
            call/cc (
              lambda (
                ret4
              )
               (
                let (
                  (
                    result ""
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
                        let (
                          (
                            n (
                              _len alphabet
                            )
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
                                          < i (
                                            _len input_string
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                ch (
                                                  _substring input_string i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    idx (
                                                      index_of alphabet ch
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      _lt idx 0
                                                    )
                                                     (
                                                      begin (
                                                        set! result (
                                                          string-append result ch
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            new_key (
                                                              fmod (
                                                                _add idx key
                                                              )
                                                               n
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              _lt new_key 0
                                                            )
                                                             (
                                                              begin (
                                                                set! new_key (
                                                                  _add new_key n
                                                                )
                                                              )
                                                            )
                                                             (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! result (
                                                              string-append result (
                                                                _substring alphabet new_key (
                                                                  _add new_key 1
                                                                )
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
         (
          define (
            decrypt input_string key alphabet
          )
           (
            call/cc (
              lambda (
                ret7
              )
               (
                let (
                  (
                    result ""
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
                        let (
                          (
                            n (
                              _len alphabet
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
                                          < i (
                                            _len input_string
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                ch (
                                                  _substring input_string i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    idx (
                                                      index_of alphabet ch
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      _lt idx 0
                                                    )
                                                     (
                                                      begin (
                                                        set! result (
                                                          string-append result ch
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            new_key (
                                                              fmod (
                                                                - idx key
                                                              )
                                                               n
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              _lt new_key 0
                                                            )
                                                             (
                                                              begin (
                                                                set! new_key (
                                                                  _add new_key n
                                                                )
                                                              )
                                                            )
                                                             (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! result (
                                                              string-append result (
                                                                _substring alphabet new_key (
                                                                  _add new_key 1
                                                                )
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
         (
          define (
            brute_force input_string alphabet
          )
           (
            call/cc (
              lambda (
                ret10
              )
               (
                let (
                  (
                    results (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        key 1
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            n (
                              _len alphabet
                            )
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
                                          <= key n
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                message (
                                                  decrypt input_string key alphabet
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! results (
                                                  append results (
                                                    _list message
                                                  )
                                                )
                                              )
                                               (
                                                set! key (
                                                  + key 1
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
                            ret10 results
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
                ret13
              )
               (
                let (
                  (
                    alpha default_alphabet
                  )
                )
                 (
                  begin (
                    let (
                      (
                        enc (
                          encrypt "The quick brown fox jumps over the lazy dog" 8 alpha
                        )
                      )
                    )
                     (
                      begin (
                        _display (
                          if (
                            string? enc
                          )
                           enc (
                            to-str enc
                          )
                        )
                      )
                       (
                        newline
                      )
                       (
                        let (
                          (
                            dec (
                              decrypt enc 8 alpha
                            )
                          )
                        )
                         (
                          begin (
                            _display (
                              if (
                                string? dec
                              )
                               dec (
                                to-str dec
                              )
                            )
                          )
                           (
                            newline
                          )
                           (
                            let (
                              (
                                brute (
                                  brute_force "jFyuMy xIH'N vLONy zILwy Gy!" alpha
                                )
                              )
                            )
                             (
                              begin (
                                _display (
                                  if (
                                    string? (
                                      cond (
                                        (
                                          string? brute
                                        )
                                         (
                                          _substring brute 19 (
                                            + 19 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? brute
                                        )
                                         (
                                          hash-table-ref brute 19
                                        )
                                      )
                                       (
                                        else (
                                          list-ref brute 19
                                        )
                                      )
                                    )
                                  )
                                   (
                                    cond (
                                      (
                                        string? brute
                                      )
                                       (
                                        _substring brute 19 (
                                          + 19 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? brute
                                      )
                                       (
                                        hash-table-ref brute 19
                                      )
                                    )
                                     (
                                      else (
                                        list-ref brute 19
                                      )
                                    )
                                  )
                                   (
                                    to-str (
                                      cond (
                                        (
                                          string? brute
                                        )
                                         (
                                          _substring brute 19 (
                                            + 19 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? brute
                                        )
                                         (
                                          hash-table-ref brute 19
                                        )
                                      )
                                       (
                                        else (
                                          list-ref brute 19
                                        )
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
      )
    )
     (
      let (
        (
          end15 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur16 (
              quotient (
                * (
                  - end15 start14
                )
                 1000000
              )
               jps17
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur16
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
