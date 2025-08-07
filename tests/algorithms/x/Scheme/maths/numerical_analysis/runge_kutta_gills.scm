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
      start12 (
        current-jiffy
      )
    )
     (
      jps15 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        sqrt x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                guess (
                  if (
                    > x 1.0
                  )
                   (
                    _div x 2.0
                  )
                   1.0
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
                                  < i 20
                                )
                                 (
                                  begin (
                                    set! guess (
                                      * 0.5 (
                                        _add guess (
                                          _div x guess
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
                    ret1 guess
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
        runge_kutta_gills func x_initial y_initial step_size x_final
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                >= x_initial x_final
              )
               (
                begin (
                  panic "The final value of x must be greater than initial value of x."
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                <= step_size 0.0
              )
               (
                begin (
                  panic "Step size must be positive."
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
                  n (
                    let (
                      (
                        v5 (
                          _div (
                            - x_final x_initial
                          )
                           step_size
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
                                        <= i n
                                      )
                                       (
                                        begin (
                                          set! y (
                                            append y (
                                              _list 0.0
                                            )
                                          )
                                        )
                                         (
                                          set! i (
                                            + i 1
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
                          list-set! y 0 y_initial
                        )
                         (
                          let (
                            (
                              xi x_initial
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
                                  let (
                                    (
                                      root2 (
                                        sqrt 2.0
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
                                                    < idx n
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          k1 (
                                                            * step_size (
                                                              func xi (
                                                                list-ref y idx
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              k2 (
                                                                * step_size (
                                                                  func (
                                                                    _add xi (
                                                                      _div step_size 2.0
                                                                    )
                                                                  )
                                                                   (
                                                                    _add (
                                                                      list-ref y idx
                                                                    )
                                                                     (
                                                                      _div k1 2.0
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
                                                                  k3 (
                                                                    * step_size (
                                                                      func (
                                                                        _add xi (
                                                                          _div step_size 2.0
                                                                        )
                                                                      )
                                                                       (
                                                                        _add (
                                                                          _add (
                                                                            list-ref y idx
                                                                          )
                                                                           (
                                                                            * (
                                                                              _add (
                                                                                - 0.5
                                                                              )
                                                                               (
                                                                                _div 1.0 root2
                                                                              )
                                                                            )
                                                                             k1
                                                                          )
                                                                        )
                                                                         (
                                                                          * (
                                                                            - 1.0 (
                                                                              _div 1.0 root2
                                                                            )
                                                                          )
                                                                           k2
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
                                                                      k4 (
                                                                        * step_size (
                                                                          func (
                                                                            + xi step_size
                                                                          )
                                                                           (
                                                                            _add (
                                                                              - (
                                                                                list-ref y idx
                                                                              )
                                                                               (
                                                                                * (
                                                                                  _div 1.0 root2
                                                                                )
                                                                                 k2
                                                                              )
                                                                            )
                                                                             (
                                                                              * (
                                                                                _add 1.0 (
                                                                                  _div 1.0 root2
                                                                                )
                                                                              )
                                                                               k3
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      list-set! y (
                                                                        + idx 1
                                                                      )
                                                                       (
                                                                        _add (
                                                                          list-ref y idx
                                                                        )
                                                                         (
                                                                          _div (
                                                                            _add (
                                                                              _add (
                                                                                _add k1 (
                                                                                  * (
                                                                                    - 2.0 root2
                                                                                  )
                                                                                   k2
                                                                                )
                                                                              )
                                                                               (
                                                                                * (
                                                                                  _add 2.0 root2
                                                                                )
                                                                                 k3
                                                                              )
                                                                            )
                                                                             k4
                                                                          )
                                                                           6.0
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      set! xi (
                                                                        + xi step_size
                                                                      )
                                                                    )
                                                                     (
                                                                      set! idx (
                                                                        + idx 1
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
                                      ret4 y
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
        f1 x y
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            ret10 (
              _div (
                - x y
              )
               2.0
            )
          )
        )
      )
    )
     (
      let (
        (
          y1 (
            runge_kutta_gills f1 0.0 3.0 0.2 5.0
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                to-str-space (
                  cond (
                    (
                      string? y1
                    )
                     (
                      _substring y1 (
                        - (
                          _len y1
                        )
                         1
                      )
                       (
                        + (
                          - (
                            _len y1
                          )
                           1
                        )
                         1
                      )
                    )
                  )
                   (
                    (
                      hash-table? y1
                    )
                     (
                      hash-table-ref y1 (
                        - (
                          _len y1
                        )
                         1
                      )
                    )
                  )
                   (
                    else (
                      list-ref y1 (
                        - (
                          _len y1
                        )
                         1
                      )
                    )
                  )
                )
              )
            )
             (
              to-str-space (
                cond (
                  (
                    string? y1
                  )
                   (
                    _substring y1 (
                      - (
                        _len y1
                      )
                       1
                    )
                     (
                      + (
                        - (
                          _len y1
                        )
                         1
                      )
                       1
                    )
                  )
                )
                 (
                  (
                    hash-table? y1
                  )
                   (
                    hash-table-ref y1 (
                      - (
                        _len y1
                      )
                       1
                    )
                  )
                )
                 (
                  else (
                    list-ref y1 (
                      - (
                        _len y1
                      )
                       1
                    )
                  )
                )
              )
            )
             (
              to-str (
                to-str-space (
                  cond (
                    (
                      string? y1
                    )
                     (
                      _substring y1 (
                        - (
                          _len y1
                        )
                         1
                      )
                       (
                        + (
                          - (
                            _len y1
                          )
                           1
                        )
                         1
                      )
                    )
                  )
                   (
                    (
                      hash-table? y1
                    )
                     (
                      hash-table-ref y1 (
                        - (
                          _len y1
                        )
                         1
                      )
                    )
                  )
                   (
                    else (
                      list-ref y1 (
                        - (
                          _len y1
                        )
                         1
                      )
                    )
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
          define (
            f2 x y
          )
           (
            call/cc (
              lambda (
                ret11
              )
               (
                ret11 x
              )
            )
          )
        )
         (
          let (
            (
              y2 (
                runge_kutta_gills f2 (
                  - 1.0
                )
                 0.0 0.2 0.0
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    to-str-space y2
                  )
                )
                 (
                  to-str-space y2
                )
                 (
                  to-str (
                    to-str-space y2
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
          end13 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur14 (
              quotient (
                * (
                  - end13 start12
                )
                 1000000
              )
               jps15
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur14
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
