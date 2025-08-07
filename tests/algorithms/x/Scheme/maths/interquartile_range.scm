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
        bubble_sort nums
      )
       (
        call/cc (
          lambda (
            ret1
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
                                    _len nums
                                  )
                                )
                                 (
                                  begin (
                                    set! arr (
                                      append arr (
                                        _list (
                                          list-ref nums i
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
                                break5
                              )
                               (
                                letrec (
                                  (
                                    loop4 (
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
                                                                  > (
                                                                    list-ref arr b
                                                                  )
                                                                   (
                                                                    list-ref arr (
                                                                      + b 1
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
                                                set! a (
                                                  + a 1
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop4
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
                                  loop4
                                )
                              )
                            )
                          )
                           (
                            ret1 arr
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
        find_median nums
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                length (
                  _len nums
                )
              )
            )
             (
              begin (
                let (
                  (
                    div (
                      _div length 2
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        mod (
                          _mod length 2
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          not (
                            equal? mod 0
                          )
                        )
                         (
                          begin (
                            ret8 (
                              list-ref nums div
                            )
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        ret8 (
                          _div (
                            + (
                              list-ref nums div
                            )
                             (
                              list-ref nums (
                                - div 1
                              )
                            )
                          )
                           2.0
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
        interquartile_range nums
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            begin (
              if (
                equal? (
                  _len nums
                )
                 0
              )
               (
                begin (
                  panic "The list is empty. Provide a non-empty list."
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
                  sorted (
                    bubble_sort nums
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      length (
                        _len sorted
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          div (
                            _div length 2
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              mod (
                                _mod length 2
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  lower (
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
                                                    < i div
                                                  )
                                                   (
                                                    begin (
                                                      set! lower (
                                                        append lower (
                                                          _list (
                                                            cond (
                                                              (
                                                                string? sorted
                                                              )
                                                               (
                                                                _substring sorted i (
                                                                  + i 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? sorted
                                                              )
                                                               (
                                                                hash-table-ref sorted i
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref sorted i
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
                                              j (
                                                + div mod
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
                                                            < j length
                                                          )
                                                           (
                                                            begin (
                                                              set! upper (
                                                                append upper (
                                                                  _list (
                                                                    cond (
                                                                      (
                                                                        string? sorted
                                                                      )
                                                                       (
                                                                        _substring sorted j (
                                                                          + j 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? sorted
                                                                      )
                                                                       (
                                                                        hash-table-ref sorted j
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref sorted j
                                                                      )
                                                                    )
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
                                                  q1 (
                                                    find_median lower
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      q3 (
                                                        find_median upper
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      ret9 (
                                                        - q3 q1
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
              )
            )
          )
        )
      )
    )
     (
      define (
        absf x
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            begin (
              if (
                < x 0.0
              )
               (
                begin (
                  ret14 (
                    - x
                  )
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret14 x
            )
          )
        )
      )
    )
     (
      define (
        float_equal a b
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            let (
              (
                diff (
                  absf (
                    - a b
                  )
                )
              )
            )
             (
              begin (
                ret15 (
                  _lt diff 1e-07
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        test_interquartile_range
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            begin (
              if (
                not (
                  float_equal (
                    interquartile_range (
                      _list 4.0 1.0 2.0 3.0 2.0
                    )
                  )
                   2.0
                )
              )
               (
                begin (
                  panic "interquartile_range case1 failed"
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
                  float_equal (
                    interquartile_range (
                      _list (
                        - 2.0
                      )
                       (
                        - 7.0
                      )
                       (
                        - 10.0
                      )
                       9.0 8.0 4.0 (
                        - 67.0
                      )
                       45.0
                    )
                  )
                   17.0
                )
              )
               (
                begin (
                  panic "interquartile_range case2 failed"
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
                  float_equal (
                    interquartile_range (
                      _list (
                        - 2.1
                      )
                       (
                        - 7.1
                      )
                       (
                        - 10.1
                      )
                       9.1 8.1 4.1 (
                        - 67.1
                      )
                       45.1
                    )
                  )
                   17.2
                )
              )
               (
                begin (
                  panic "interquartile_range case3 failed"
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
                  float_equal (
                    interquartile_range (
                      _list 0.0 0.0 0.0 0.0 0.0
                    )
                  )
                   0.0
                )
              )
               (
                begin (
                  panic "interquartile_range case4 failed"
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
     (
      define (
        main
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            begin (
              test_interquartile_range
            )
             (
              _display (
                if (
                  string? (
                    to-str-space (
                      interquartile_range (
                        _list 4.0 1.0 2.0 3.0 2.0
                      )
                    )
                  )
                )
                 (
                  to-str-space (
                    interquartile_range (
                      _list 4.0 1.0 2.0 3.0 2.0
                    )
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      interquartile_range (
                        _list 4.0 1.0 2.0 3.0 2.0
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
