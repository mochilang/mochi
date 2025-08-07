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
      start20 (
        current-jiffy
      )
    )
     (
      jps23 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        ucal u p
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                temp u
              )
            )
             (
              begin (
                let (
                  (
                    i 1
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
                                  < i p
                                )
                                 (
                                  begin (
                                    set! temp (
                                      * temp (
                                        - u (
                                          + 0.0 i
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
                    ret1 temp
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
        factorial n
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                result 1.0
              )
            )
             (
              begin (
                let (
                  (
                    i 2
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
                                  <= i n
                                )
                                 (
                                  begin (
                                    set! result (
                                      * result (
                                        + 0.0 i
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
        newton_forward_interpolation x y0 value
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                n (
                  _len x
                )
              )
            )
             (
              begin (
                let (
                  (
                    y (
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
                                      < i n
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            row (
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
                                                              < j n
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list 0.0
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! j (
                                                                  + j 1
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
                                                set! y (
                                                  append y (
                                                    _list row
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
                        set! i 0
                      )
                       (
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
                                      < i n
                                    )
                                     (
                                      begin (
                                        list-set! (
                                          list-ref y i
                                        )
                                         0 (
                                          list-ref y0 i
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
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
                            i1 1
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
                                          < i1 n
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                j1 0
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
                                                              < j1 (
                                                                - n i1
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                list-set! (
                                                                  list-ref y j1
                                                                )
                                                                 i1 (
                                                                  - (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref y (
                                                                            + j1 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref y (
                                                                            + j1 1
                                                                          )
                                                                        )
                                                                         (
                                                                          - i1 1
                                                                        )
                                                                         (
                                                                          + (
                                                                            - i1 1
                                                                          )
                                                                           1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref y (
                                                                            + j1 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref y (
                                                                            + j1 1
                                                                          )
                                                                        )
                                                                         (
                                                                          - i1 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          list-ref y (
                                                                            + j1 1
                                                                          )
                                                                        )
                                                                         (
                                                                          - i1 1
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref y j1
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref y j1
                                                                        )
                                                                         (
                                                                          - i1 1
                                                                        )
                                                                         (
                                                                          + (
                                                                            - i1 1
                                                                          )
                                                                           1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref y j1
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref y j1
                                                                        )
                                                                         (
                                                                          - i1 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          list-ref y j1
                                                                        )
                                                                         (
                                                                          - i1 1
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! j1 (
                                                                  + j1 1
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
                                                set! i1 (
                                                  + i1 1
                                                )
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
                            let (
                              (
                                u (
                                  _div (
                                    - value (
                                      list-ref x 0
                                    )
                                  )
                                   (
                                    - (
                                      list-ref x 1
                                    )
                                     (
                                      list-ref x 0
                                    )
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    sum (
                                      cond (
                                        (
                                          string? (
                                            list-ref y 0
                                          )
                                        )
                                         (
                                          _substring (
                                            list-ref y 0
                                          )
                                           0 (
                                            + 0 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? (
                                            list-ref y 0
                                          )
                                        )
                                         (
                                          hash-table-ref (
                                            list-ref y 0
                                          )
                                           0
                                        )
                                      )
                                       (
                                        else (
                                          list-ref (
                                            list-ref y 0
                                          )
                                           0
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        k 1
                                      )
                                    )
                                     (
                                      begin (
                                        call/cc (
                                          lambda (
                                            break19
                                          )
                                           (
                                            letrec (
                                              (
                                                loop18 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < k n
                                                    )
                                                     (
                                                      begin (
                                                        set! sum (
                                                          _add sum (
                                                            _div (
                                                              * (
                                                                ucal u k
                                                              )
                                                               (
                                                                cond (
                                                                  (
                                                                    string? (
                                                                      list-ref y 0
                                                                    )
                                                                  )
                                                                   (
                                                                    _substring (
                                                                      list-ref y 0
                                                                    )
                                                                     k (
                                                                      + k 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? (
                                                                      list-ref y 0
                                                                    )
                                                                  )
                                                                   (
                                                                    hash-table-ref (
                                                                      list-ref y 0
                                                                    )
                                                                     k
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref (
                                                                      list-ref y 0
                                                                    )
                                                                     k
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              factorial k
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! k (
                                                          + k 1
                                                        )
                                                      )
                                                       (
                                                        loop18
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
                                              loop18
                                            )
                                          )
                                        )
                                      )
                                       (
                                        ret7 sum
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
      let (
        (
          x_points (
            _list 0.0 1.0 2.0 3.0
          )
        )
      )
       (
        begin (
          let (
            (
              y_points (
                _list 0.0 1.0 8.0 27.0
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    to-str-space (
                      newton_forward_interpolation x_points y_points 1.5
                    )
                  )
                )
                 (
                  to-str-space (
                    newton_forward_interpolation x_points y_points 1.5
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      newton_forward_interpolation x_points y_points 1.5
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
      let (
        (
          end21 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur22 (
              quotient (
                * (
                  - end21 start20
                )
                 1000000
              )
               jps23
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur22
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
