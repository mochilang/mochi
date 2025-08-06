;; Generated on 2025-08-06 18:11 +0700
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
                                            modulo num 2
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
                                            quotient num 2
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
        zfill s width
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                res s
              )
            )
             (
              begin (
                let (
                  (
                    pad (
                      - width (
                        _len s
                      )
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
                                  > pad 0
                                )
                                 (
                                  begin (
                                    set! res (
                                      string-append "0" res
                                    )
                                  )
                                   (
                                    set! pad (
                                      - pad 1
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
     (
      define (
        binary_and a b
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
                  < a 0
                )
                 (
                  < b 0
                )
              )
               (
                begin (
                  panic "the value of both inputs must be positive"
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
                  a_bin (
                    to_binary a
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      b_bin (
                        to_binary b
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          max_len (
                            _len a_bin
                          )
                        )
                      )
                       (
                        begin (
                          if (
                            > (
                              _len b_bin
                            )
                             max_len
                          )
                           (
                            begin (
                              set! max_len (
                                _len b_bin
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
                              a_pad (
                                zfill a_bin max_len
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  b_pad (
                                    zfill b_bin max_len
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
                                      let (
                                        (
                                          res ""
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
                                                        < i max_len
                                                      )
                                                       (
                                                        begin (
                                                          if (
                                                            and (
                                                              string=? (
                                                                cond (
                                                                  (
                                                                    string? a_pad
                                                                  )
                                                                   (
                                                                    _substring a_pad i (
                                                                      + i 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? a_pad
                                                                  )
                                                                   (
                                                                    hash-table-ref a_pad i
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref a_pad i
                                                                  )
                                                                )
                                                              )
                                                               "1"
                                                            )
                                                             (
                                                              string=? (
                                                                cond (
                                                                  (
                                                                    string? b_pad
                                                                  )
                                                                   (
                                                                    _substring b_pad i (
                                                                      + i 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? b_pad
                                                                  )
                                                                   (
                                                                    hash-table-ref b_pad i
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref b_pad i
                                                                  )
                                                                )
                                                              )
                                                               "1"
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              set! res (
                                                                string-append res "1"
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              set! res (
                                                                string-append res "0"
                                                              )
                                                            )
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
                                          ret7 (
                                            string-append "0b" res
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
      )
    )
     (
      _display (
        if (
          string? (
            binary_and 25 32
          )
        )
         (
          binary_and 25 32
        )
         (
          to-str (
            binary_and 25 32
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
            binary_and 37 50
          )
        )
         (
          binary_and 37 50
        )
         (
          to-str (
            binary_and 37 50
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
            binary_and 21 30
          )
        )
         (
          binary_and 21 30
        )
         (
          to-str (
            binary_and 21 30
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
            binary_and 58 73
          )
        )
         (
          binary_and 58 73
        )
         (
          to-str (
            binary_and 58 73
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
            binary_and 0 255
          )
        )
         (
          binary_and 0 255
        )
         (
          to-str (
            binary_and 0 255
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
            binary_and 256 256
          )
        )
         (
          binary_and 256 256
        )
         (
          to-str (
            binary_and 256 256
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
