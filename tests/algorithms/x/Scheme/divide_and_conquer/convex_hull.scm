;; Generated on 2025-08-07 08:20 +0700
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
        cross o a b
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              - (
                * (
                  - (
                    hash-table-ref a "x"
                  )
                   (
                    hash-table-ref o "x"
                  )
                )
                 (
                  - (
                    hash-table-ref b "y"
                  )
                   (
                    hash-table-ref o "y"
                  )
                )
              )
               (
                * (
                  - (
                    hash-table-ref a "y"
                  )
                   (
                    hash-table-ref o "y"
                  )
                )
                 (
                  - (
                    hash-table-ref b "x"
                  )
                   (
                    hash-table-ref o "x"
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
        sortPoints ps
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                arr ps
              )
            )
             (
              begin (
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
                        i 0
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
                                      < i n
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
                                                          < j (
                                                            - n 1
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                p (
                                                                  list-ref arr j
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    q (
                                                                      list-ref arr (
                                                                        + j 1
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      or (
                                                                        > (
                                                                          hash-table-ref p "x"
                                                                        )
                                                                         (
                                                                          hash-table-ref q "x"
                                                                        )
                                                                      )
                                                                       (
                                                                        and (
                                                                          equal? (
                                                                            hash-table-ref p "x"
                                                                          )
                                                                           (
                                                                            hash-table-ref q "x"
                                                                          )
                                                                        )
                                                                         (
                                                                          > (
                                                                            hash-table-ref p "y"
                                                                          )
                                                                           (
                                                                            hash-table-ref q "y"
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        list-set! arr j q
                                                                      )
                                                                       (
                                                                        list-set! arr (
                                                                          + j 1
                                                                        )
                                                                         p
                                                                      )
                                                                    )
                                                                     (
                                                                      quote (
                                                                        
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
                                            set! i (
                                              + i 1
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
                        ret2 arr
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
        convex_hull ps
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              set! ps (
                sortPoints ps
              )
            )
             (
              let (
                (
                  lower (
                    _list
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
                                      p (
                                        car xs
                                      )
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
                                                    and (
                                                      >= (
                                                        _len lower
                                                      )
                                                       2
                                                    )
                                                     (
                                                      _le (
                                                        cross (
                                                          list-ref lower (
                                                            - (
                                                              _len lower
                                                            )
                                                             2
                                                          )
                                                        )
                                                         (
                                                          list-ref lower (
                                                            - (
                                                              _len lower
                                                            )
                                                             1
                                                          )
                                                        )
                                                         p
                                                      )
                                                       0
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! lower (
                                                        take (
                                                          drop lower 0
                                                        )
                                                         (
                                                          - (
                                                            - (
                                                              _len lower
                                                            )
                                                             1
                                                          )
                                                           0
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
                                      set! lower (
                                        append lower (
                                          _list p
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  loop8 (
                                    cdr xs
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                       (
                        loop8 ps
                      )
                    )
                  )
                )
                 (
                  let (
                    (
                      upper (
                        _list
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          i (
                            - (
                              _len ps
                            )
                             1
                          )
                        )
                      )
                       (
                        begin (
                          call/cc (
                            lambda (
                              break13
                            )
                             (
                              letrec (
                                (
                                  loop12 (
                                    lambda (
                                      
                                    )
                                     (
                                      if (
                                        >= i 0
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              p (
                                                list-ref ps i
                                              )
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
                                                            and (
                                                              >= (
                                                                _len upper
                                                              )
                                                               2
                                                            )
                                                             (
                                                              _le (
                                                                cross (
                                                                  list-ref upper (
                                                                    - (
                                                                      _len upper
                                                                    )
                                                                     2
                                                                  )
                                                                )
                                                                 (
                                                                  list-ref upper (
                                                                    - (
                                                                      _len upper
                                                                    )
                                                                     1
                                                                  )
                                                                )
                                                                 p
                                                              )
                                                               0
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              set! upper (
                                                                take (
                                                                  drop upper 0
                                                                )
                                                                 (
                                                                  - (
                                                                    - (
                                                                      _len upper
                                                                    )
                                                                     1
                                                                  )
                                                                   0
                                                                )
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
                                              set! upper (
                                                append upper (
                                                  _list p
                                                )
                                              )
                                            )
                                             (
                                              set! i (
                                                - i 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          loop12
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
                                loop12
                              )
                            )
                          )
                        )
                         (
                          let (
                            (
                              hull (
                                take (
                                  drop lower 0
                                )
                                 (
                                  - (
                                    - (
                                      _len lower
                                    )
                                     1
                                  )
                                   0
                                )
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
                                                < j (
                                                  - (
                                                    _len upper
                                                  )
                                                   1
                                                )
                                              )
                                               (
                                                begin (
                                                  set! hull (
                                                    append hull (
                                                      _list (
                                                        list-ref upper j
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  set! j (
                                                    + j 1
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
                                 (
                                  ret7 hull
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
