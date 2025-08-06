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
      let (
        (
          ASCII_UPPERCASE "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        )
      )
       (
        begin (
          let (
            (
              ASCII_LOWERCASE "abcdefghijklmnopqrstuvwxyz"
            )
          )
           (
            begin (
              let (
                (
                  NEG_ONE (
                    - 0 1
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
                            ret1 NEG_ONE
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    to_uppercase s
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
                                                _len s
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    ch (
                                                      _substring s i (
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
                                                          index_of ASCII_LOWERCASE ch
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          equal? idx NEG_ONE
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
                                                            set! result (
                                                              string-append result (
                                                                _substring ASCII_UPPERCASE idx (
                                                                  _add idx 1
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
                 (
                  define (
                    gronsfeld text key
                  )
                   (
                    call/cc (
                      lambda (
                        ret7
                      )
                       (
                        let (
                          (
                            ascii_len (
                              _len ASCII_UPPERCASE
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                key_len (
                                  _len key
                                )
                              )
                            )
                             (
                              begin (
                                if (
                                  equal? key_len 0
                                )
                                 (
                                  begin (
                                    panic "integer modulo by zero"
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
                                    upper_text (
                                      to_uppercase text
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        encrypted ""
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
                                                            _len upper_text
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                ch (
                                                                  _substring upper_text i (
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
                                                                      index_of ASCII_UPPERCASE ch
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      equal? idx NEG_ONE
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! encrypted (
                                                                          string-append encrypted ch
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            key_idx (
                                                                              modulo i key_len
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                shift (
                                                                                  let (
                                                                                    (
                                                                                      v10 (
                                                                                        _substring key key_idx (
                                                                                          + key_idx 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    cond (
                                                                                      (
                                                                                        string? v10
                                                                                      )
                                                                                       (
                                                                                        exact (
                                                                                          floor (
                                                                                            string->number v10
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        boolean? v10
                                                                                      )
                                                                                       (
                                                                                        if v10 1 0
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        exact (
                                                                                          floor v10
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
                                                                                    new_position (
                                                                                      fmod (
                                                                                        _add idx shift
                                                                                      )
                                                                                       ascii_len
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! encrypted (
                                                                                      string-append encrypted (
                                                                                        _substring ASCII_UPPERCASE new_position (
                                                                                          _add new_position 1
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
                                            ret7 encrypted
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
                        gronsfeld "hello" "412"
                      )
                    )
                     (
                      gronsfeld "hello" "412"
                    )
                     (
                      to-str (
                        gronsfeld "hello" "412"
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
                        gronsfeld "hello" "123"
                      )
                    )
                     (
                      gronsfeld "hello" "123"
                    )
                     (
                      to-str (
                        gronsfeld "hello" "123"
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
                        gronsfeld "" "123"
                      )
                    )
                     (
                      gronsfeld "" "123"
                    )
                     (
                      to-str (
                        gronsfeld "" "123"
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
                        gronsfeld "yes, ¥€$ - _!@#%?" "0"
                      )
                    )
                     (
                      gronsfeld "yes, ¥€$ - _!@#%?" "0"
                    )
                     (
                      to-str (
                        gronsfeld "yes, ¥€$ - _!@#%?" "0"
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
                        gronsfeld "yes, ¥€$ - _!@#%?" "01"
                      )
                    )
                     (
                      gronsfeld "yes, ¥€$ - _!@#%?" "01"
                    )
                     (
                      to-str (
                        gronsfeld "yes, ¥€$ - _!@#%?" "01"
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
                        gronsfeld "yes, ¥€$ - _!@#%?" "012"
                      )
                    )
                     (
                      gronsfeld "yes, ¥€$ - _!@#%?" "012"
                    )
                     (
                      to-str (
                        gronsfeld "yes, ¥€$ - _!@#%?" "012"
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
