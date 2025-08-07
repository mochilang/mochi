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
        ceil_float x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                i (
                  let (
                    (
                      v2 x
                    )
                  )
                   (
                    cond (
                      (
                        string? v2
                      )
                       (
                        inexact->exact (
                          floor (
                            string->number v2
                          )
                        )
                      )
                    )
                     (
                      (
                        boolean? v2
                      )
                       (
                        if v2 1 0
                      )
                    )
                     (
                      else (
                        inexact->exact (
                          floor v2
                        )
                      )
                    )
                  )
                )
              )
            )
             (
              begin (
                if (
                  > x (
                    + 0.0 i
                  )
                )
                 (
                  begin (
                    ret1 (
                      + i 1
                    )
                  )
                )
                 (
                  quote (
                    
                  )
                )
              )
               (
                ret1 i
              )
            )
          )
        )
      )
    )
     (
      define (
        exp_approx x
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            let (
              (
                term 1.0
              )
            )
             (
              begin (
                let (
                  (
                    sum 1.0
                  )
                )
                 (
                  begin (
                    let (
                      (
                        n 1
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
                                      < n 20
                                    )
                                     (
                                      begin (
                                        set! term (
                                          _div (
                                            * term x
                                          )
                                           (
                                            + 0.0 n
                                          )
                                        )
                                      )
                                       (
                                        set! sum (
                                          + sum term
                                        )
                                      )
                                       (
                                        set! n (
                                          + n 1
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
                        ret3 sum
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
        euler_modified ode_func y0 x0 step x_end
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                n (
                  ceil_float (
                    _div (
                      - x_end x0
                    )
                     step
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    y (
                      _list y0
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        x x0
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            k 0
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
                                          _lt k n
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                y_predict (
                                                  _add (
                                                    list-ref y k
                                                  )
                                                   (
                                                    * step (
                                                      ode_func x (
                                                        list-ref y k
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
                                                    slope1 (
                                                      ode_func x (
                                                        list-ref y k
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        slope2 (
                                                          ode_func (
                                                            + x step
                                                          )
                                                           y_predict
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            y_next (
                                                              _add (
                                                                list-ref y k
                                                              )
                                                               (
                                                                * (
                                                                  _div step 2.0
                                                                )
                                                                 (
                                                                  + slope1 slope2
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! y (
                                                              append y (
                                                                _list y_next
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! x (
                                                              + x step
                                                            )
                                                          )
                                                           (
                                                            set! k (
                                                              + k 1
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
                            ret6 y
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
            ret9
          )
           (
            ret9 (
              * (
                * (
                  * (
                    - 2.0
                  )
                   x
                )
                 y
              )
               y
            )
          )
        )
      )
    )
     (
      define (
        f2 x y
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            ret10 (
              _add (
                * (
                  - 2.0
                )
                 y
              )
               (
                * (
                  * (
                    * x x
                  )
                   x
                )
                 (
                  exp_approx (
                    * (
                      - 2.0
                    )
                     x
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
            ret11
          )
           (
            let (
              (
                y1 (
                  euler_modified f1 1.0 0.0 0.2 1.0
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
                   (
                    to-str (
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
               (
                newline
              )
               (
                let (
                  (
                    y2 (
                      euler_modified f2 1.0 0.0 0.1 0.3
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
                              string? y2
                            )
                             (
                              _substring y2 (
                                - (
                                  _len y2
                                )
                                 1
                              )
                               (
                                + (
                                  - (
                                    _len y2
                                  )
                                   1
                                )
                                 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? y2
                            )
                             (
                              hash-table-ref y2 (
                                - (
                                  _len y2
                                )
                                 1
                              )
                            )
                          )
                           (
                            else (
                              list-ref y2 (
                                - (
                                  _len y2
                                )
                                 1
                              )
                            )
                          )
                        )
                      )
                       (
                        cond (
                          (
                            string? y2
                          )
                           (
                            _substring y2 (
                              - (
                                _len y2
                              )
                               1
                            )
                             (
                              + (
                                - (
                                  _len y2
                                )
                                 1
                              )
                               1
                            )
                          )
                        )
                         (
                          (
                            hash-table? y2
                          )
                           (
                            hash-table-ref y2 (
                              - (
                                _len y2
                              )
                               1
                            )
                          )
                        )
                         (
                          else (
                            list-ref y2 (
                              - (
                                _len y2
                              )
                               1
                            )
                          )
                        )
                      )
                       (
                        to-str (
                          cond (
                            (
                              string? y2
                            )
                             (
                              _substring y2 (
                                - (
                                  _len y2
                                )
                                 1
                              )
                               (
                                + (
                                  - (
                                    _len y2
                                  )
                                   1
                                )
                                 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? y2
                            )
                             (
                              hash-table-ref y2 (
                                - (
                                  _len y2
                                )
                                 1
                              )
                            )
                          )
                           (
                            else (
                              list-ref y2 (
                                - (
                                  _len y2
                                )
                                 1
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
