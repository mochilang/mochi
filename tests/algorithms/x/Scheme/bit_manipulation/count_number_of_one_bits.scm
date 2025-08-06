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
      start18 (
        current-jiffy
      )
    )
     (
      jps21 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        bit_and a b
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                ua a
              )
            )
             (
              begin (
                let (
                  (
                    ub b
                  )
                )
                 (
                  begin (
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
                                            > ua 0
                                          )
                                           (
                                            > ub 0
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              and (
                                                equal? (
                                                  modulo ua 2
                                                )
                                                 1
                                              )
                                               (
                                                equal? (
                                                  modulo ub 2
                                                )
                                                 1
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
                                            set! ua (
                                              let (
                                                (
                                                  v4 (
                                                    quotient ua 2
                                                  )
                                                )
                                              )
                                               (
                                                cond (
                                                  (
                                                    string? v4
                                                  )
                                                   (
                                                    inexact->exact (
                                                      floor (
                                                        string->number v4
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    boolean? v4
                                                  )
                                                   (
                                                    if v4 1 0
                                                  )
                                                )
                                                 (
                                                  else (
                                                    inexact->exact (
                                                      floor v4
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! ub (
                                              let (
                                                (
                                                  v5 (
                                                    quotient ub 2
                                                  )
                                                )
                                              )
                                               (
                                                cond (
                                                  (
                                                    string? v5
                                                  )
                                                   (
                                                    inexact->exact (
                                                      floor (
                                                        string->number v5
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    boolean? v5
                                                  )
                                                   (
                                                    if v5 1 0
                                                  )
                                                )
                                                 (
                                                  else (
                                                    inexact->exact (
                                                      floor v5
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! bit (
                                              * bit 2
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
      define (
        count_bits_kernighan n
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            begin (
              if (
                < n 0
              )
               (
                begin (
                  panic "the value of input must not be negative"
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
                      result 0
                    )
                  )
                   (
                    begin (
                      call/cc (
                        lambda (
                          break8
                        )
                         (
                          letrec (
                            (
                              loop7 (
                                lambda (
                                  
                                )
                                 (
                                  if (
                                    not (
                                      equal? num 0
                                    )
                                  )
                                   (
                                    begin (
                                      set! num (
                                        bit_and num (
                                          - num 1
                                        )
                                      )
                                    )
                                     (
                                      set! result (
                                        + result 1
                                      )
                                    )
                                     (
                                      loop7
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
                            loop7
                          )
                        )
                      )
                    )
                     (
                      ret6 result
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
        count_bits_modulo n
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            begin (
              if (
                < n 0
              )
               (
                begin (
                  panic "the value of input must not be negative"
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
                      result 0
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
                                    not (
                                      equal? num 0
                                    )
                                  )
                                   (
                                    begin (
                                      if (
                                        equal? (
                                          modulo num 2
                                        )
                                         1
                                      )
                                       (
                                        begin (
                                          set! result (
                                            + result 1
                                          )
                                        )
                                      )
                                       (
                                        quote (
                                          
                                        )
                                      )
                                    )
                                     (
                                      set! num (
                                        let (
                                          (
                                            v12 (
                                              quotient num 2
                                            )
                                          )
                                        )
                                         (
                                          cond (
                                            (
                                              string? v12
                                            )
                                             (
                                              inexact->exact (
                                                floor (
                                                  string->number v12
                                                )
                                              )
                                            )
                                          )
                                           (
                                            (
                                              boolean? v12
                                            )
                                             (
                                              if v12 1 0
                                            )
                                          )
                                           (
                                            else (
                                              inexact->exact (
                                                floor v12
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
                numbers (
                  _list 25 37 21 58 0 256
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
                        break15
                      )
                       (
                        letrec (
                          (
                            loop14 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len numbers
                                  )
                                )
                                 (
                                  begin (
                                    _display (
                                      if (
                                        string? (
                                          to-str-space (
                                            count_bits_kernighan (
                                              list-ref numbers i
                                            )
                                          )
                                        )
                                      )
                                       (
                                        to-str-space (
                                          count_bits_kernighan (
                                            list-ref numbers i
                                          )
                                        )
                                      )
                                       (
                                        to-str (
                                          to-str-space (
                                            count_bits_kernighan (
                                              list-ref numbers i
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    newline
                                  )
                                   (
                                    set! i (
                                      + i 1
                                    )
                                  )
                                   (
                                    loop14
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
                          loop14
                        )
                      )
                    )
                  )
                   (
                    set! i 0
                  )
                   (
                    call/cc (
                      lambda (
                        break17
                      )
                       (
                        letrec (
                          (
                            loop16 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len numbers
                                  )
                                )
                                 (
                                  begin (
                                    _display (
                                      if (
                                        string? (
                                          to-str-space (
                                            count_bits_modulo (
                                              list-ref numbers i
                                            )
                                          )
                                        )
                                      )
                                       (
                                        to-str-space (
                                          count_bits_modulo (
                                            list-ref numbers i
                                          )
                                        )
                                      )
                                       (
                                        to-str (
                                          to-str-space (
                                            count_bits_modulo (
                                              list-ref numbers i
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    newline
                                  )
                                   (
                                    set! i (
                                      + i 1
                                    )
                                  )
                                   (
                                    loop16
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
                          loop16
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
     (
      let (
        (
          end19 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur20 (
              quotient (
                * (
                  - end19 start18
                )
                 1000000
              )
               jps21
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur20
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
