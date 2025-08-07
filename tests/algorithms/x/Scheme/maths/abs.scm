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
      start17 (
        current-jiffy
      )
    )
     (
      jps20 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        abs_val num
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                < num 0.0
              )
               (
                begin (
                  ret1 (
                    - num
                  )
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret1 num
            )
          )
        )
      )
    )
     (
      define (
        abs_min x
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                equal? (
                  _len x
                )
                 0
              )
               (
                begin (
                  panic "abs_min() arg is an empty sequence"
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
                  j (
                    list-ref x 0
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      idx 0
                    )
                  )
                   (
                    begin (
                      call/cc (
                        lambda (
                          break4
                        )
                         (
                          letrec (
                            (
                              loop3 (
                                lambda (
                                  
                                )
                                 (
                                  if (
                                    < idx (
                                      _len x
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          i (
                                            list-ref x idx
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            _lt (
                                              abs_val (
                                                exact->inexact i
                                              )
                                            )
                                             (
                                              abs_val (
                                                exact->inexact j
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! j i
                                            )
                                          )
                                           (
                                            quote (
                                              
                                            )
                                          )
                                        )
                                         (
                                          set! idx (
                                            + idx 1
                                          )
                                        )
                                      )
                                    )
                                     (
                                      loop3
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
                            loop3
                          )
                        )
                      )
                    )
                     (
                      ret2 j
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
        abs_max x
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              if (
                equal? (
                  _len x
                )
                 0
              )
               (
                begin (
                  panic "abs_max() arg is an empty sequence"
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
                  j (
                    list-ref x 0
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      idx 0
                    )
                  )
                   (
                    begin (
                      call/cc (
                        lambda (
                          break7
                        )
                         (
                          letrec (
                            (
                              loop6 (
                                lambda (
                                  
                                )
                                 (
                                  if (
                                    < idx (
                                      _len x
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          i (
                                            list-ref x idx
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            _gt (
                                              abs_val (
                                                exact->inexact i
                                              )
                                            )
                                             (
                                              abs_val (
                                                exact->inexact j
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! j i
                                            )
                                          )
                                           (
                                            quote (
                                              
                                            )
                                          )
                                        )
                                         (
                                          set! idx (
                                            + idx 1
                                          )
                                        )
                                      )
                                    )
                                     (
                                      loop6
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
                            loop6
                          )
                        )
                      )
                    )
                     (
                      ret5 j
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
        abs_max_sort x
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            begin (
              if (
                equal? (
                  _len x
                )
                 0
              )
               (
                begin (
                  panic "abs_max_sort() arg is an empty sequence"
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
                  arr (
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
                          break10
                        )
                         (
                          letrec (
                            (
                              loop9 (
                                lambda (
                                  
                                )
                                 (
                                  if (
                                    < i (
                                      _len x
                                    )
                                  )
                                   (
                                    begin (
                                      set! arr (
                                        append arr (
                                          _list (
                                            list-ref x i
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
                                      loop9
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
                            loop9
                          )
                        )
                      )
                    )
                     (
                      let (
                        (
                          n (
                            _len arr
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              a 0
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
                                            < a n
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  b 0
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
                                                                < b (
                                                                  - (
                                                                    - n a
                                                                  )
                                                                   1
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  if (
                                                                    _gt (
                                                                      abs_val (
                                                                        exact->inexact (
                                                                          list-ref arr b
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      abs_val (
                                                                        exact->inexact (
                                                                          list-ref arr (
                                                                            + b 1
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      let (
                                                                        (
                                                                          temp (
                                                                            list-ref arr b
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          list-set! arr b (
                                                                            list-ref arr (
                                                                              + b 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          list-set! arr (
                                                                            + b 1
                                                                          )
                                                                           temp
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
                                                                  set! b (
                                                                    + b 1
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
                                                  set! a (
                                                    + a 1
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
                              ret8 (
                                list-ref arr (
                                  - n 1
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
      define (
        test_abs_val
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            begin (
              if (
                not (
                  equal? (
                    abs_val 0.0
                  )
                   0.0
                )
              )
               (
                begin (
                  panic "abs_val(0) failed"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                not (
                  equal? (
                    abs_val 34.0
                  )
                   34.0
                )
              )
               (
                begin (
                  panic "abs_val(34) failed"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                not (
                  equal? (
                    abs_val (
                      - 100000000000.0
                    )
                  )
                   100000000000.0
                )
              )
               (
                begin (
                  panic "abs_val large failed"
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
                  a (
                    _list (
                      - 3
                    )
                     (
                      - 1
                    )
                     2 (
                      - 11
                    )
                  )
                )
              )
               (
                begin (
                  if (
                    not (
                      equal? (
                        abs_max a
                      )
                       (
                        - 11
                      )
                    )
                  )
                   (
                    begin (
                      panic "abs_max failed"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    not (
                      equal? (
                        abs_max_sort a
                      )
                       (
                        - 11
                      )
                    )
                  )
                   (
                    begin (
                      panic "abs_max_sort failed"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    not (
                      equal? (
                        abs_min a
                      )
                       (
                        - 1
                      )
                    )
                  )
                   (
                    begin (
                      panic "abs_min failed"
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
     (
      define (
        main
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            begin (
              test_abs_val
            )
             (
              _display (
                if (
                  string? (
                    abs_val (
                      - 34.0
                    )
                  )
                )
                 (
                  abs_val (
                    - 34.0
                  )
                )
                 (
                  to-str (
                    abs_val (
                      - 34.0
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
          end18 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur19 (
              quotient (
                * (
                  - end18 start17
                )
                 1000000
              )
               jps20
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur19
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
