;; Generated on 2025-08-06 23:15 +0700
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
        xor a b
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                res 0
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
                                                abit (
                                                  _mod x 2
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    bbit (
                                                      _mod y 2
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      not (
                                                        equal? abit bbit
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! res (
                                                          + res bit
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
                                                      _div x 2
                                                    )
                                                  )
                                                   (
                                                    set! y (
                                                      _div y 2
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
        )
      )
    )
     (
      let (
        (
          ascii " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
        )
      )
       (
        begin (
          define (
            ord ch
          )
           (
            call/cc (
              lambda (
                ret4
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
                                    _len ascii
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      string=? (
                                        _substring ascii i (
                                          + i 1
                                        )
                                      )
                                       ch
                                    )
                                     (
                                      begin (
                                        ret4 (
                                          + 32 i
                                        )
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
                    ret4 0
                  )
                )
              )
            )
          )
        )
         (
          define (
            chr n
          )
           (
            call/cc (
              lambda (
                ret7
              )
               (
                begin (
                  if (
                    and (
                      >= n 32
                    )
                     (
                      < n 127
                    )
                  )
                   (
                    begin (
                      ret7 (
                        _substring ascii (
                          - n 32
                        )
                         (
                          - n 31
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
                  ret7 ""
                )
              )
            )
          )
        )
         (
          define (
            normalize_key key
          )
           (
            call/cc (
              lambda (
                ret8
              )
               (
                let (
                  (
                    k key
                  )
                )
                 (
                  begin (
                    if (
                      equal? k 0
                    )
                     (
                      begin (
                        set! k 1
                      )
                    )
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
                    set! k (
                      _mod k 256
                    )
                  )
                   (
                    if (
                      < k 0
                    )
                     (
                      begin (
                        set! k (
                          + k 256
                        )
                      )
                    )
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
                    ret8 k
                  )
                )
              )
            )
          )
        )
         (
          define (
            encrypt content key
          )
           (
            call/cc (
              lambda (
                ret9
              )
               (
                let (
                  (
                    k (
                      normalize_key key
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        result (
                          _list
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
                                break11
                              )
                               (
                                letrec (
                                  (
                                    loop10 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i (
                                            _len content
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                c (
                                                  ord (
                                                    _substring content i (
                                                      + i 1
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    e (
                                                      xor c k
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! result (
                                                      append result (
                                                        _list (
                                                          chr e
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
                                            loop10
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
                                  loop10
                                )
                              )
                            )
                          )
                           (
                            ret9 result
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
            encrypt_string content key
          )
           (
            call/cc (
              lambda (
                ret12
              )
               (
                let (
                  (
                    chars (
                      encrypt content key
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        out ""
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
                                            ch (
                                              car xs
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! out (
                                              string-append out ch
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop13 (
                                          cdr xs
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop13 chars
                            )
                          )
                        )
                      )
                       (
                        ret12 out
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
              sample "hallo welt"
            )
          )
           (
            begin (
              let (
                (
                  enc (
                    encrypt_string sample 1
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      dec (
                        encrypt_string enc 1
                      )
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            to-str-space (
                              encrypt sample 1
                            )
                          )
                        )
                         (
                          to-str-space (
                            encrypt sample 1
                          )
                        )
                         (
                          to-str (
                            to-str-space (
                              encrypt sample 1
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
