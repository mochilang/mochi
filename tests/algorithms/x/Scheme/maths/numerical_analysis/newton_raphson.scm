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
      start8 (
        current-jiffy
      )
    )
     (
      jps11 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        abs_float x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            if (
              < x 0.0
            )
             (
              begin (
                ret1 (
                  - x
                )
              )
            )
             (
              begin (
                ret1 x
              )
            )
          )
        )
      )
    )
     (
      define (
        fail msg
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              _display (
                if (
                  string? (
                    string-append "error: " msg
                  )
                )
                 (
                  string-append "error: " msg
                )
                 (
                  to-str (
                    string-append "error: " msg
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
      define (
        calc_derivative f x delta_x
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            ret3 (
              _div (
                - (
                  f (
                    _add x (
                      _div delta_x 2.0
                    )
                  )
                )
                 (
                  f (
                    - x (
                      _div delta_x 2.0
                    )
                  )
                )
              )
               delta_x
            )
          )
        )
      )
    )
     (
      define (
        newton_raphson f x0 max_iter step max_error log_steps
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                a x0
              )
            )
             (
              begin (
                let (
                  (
                    steps (
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
                                      < i max_iter
                                    )
                                     (
                                      begin (
                                        if log_steps (
                                          begin (
                                            set! steps (
                                              append steps (
                                                _list a
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
                                        let (
                                          (
                                            err (
                                              abs_float (
                                                f a
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              < err max_error
                                            )
                                             (
                                              begin (
                                                ret4 (
                                                  alist->hash-table (
                                                    _list (
                                                      cons "root" a
                                                    )
                                                     (
                                                      cons "error" err
                                                    )
                                                     (
                                                      cons "steps" steps
                                                    )
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
                                            let (
                                              (
                                                der (
                                                  calc_derivative f a step
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  equal? der 0.0
                                                )
                                                 (
                                                  begin (
                                                    fail "No converging solution found, zero derivative"
                                                  )
                                                   (
                                                    ret4 (
                                                      alist->hash-table (
                                                        _list (
                                                          cons "root" a
                                                        )
                                                         (
                                                          cons "error" err
                                                        )
                                                         (
                                                          cons "steps" steps
                                                        )
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
                                                set! a (
                                                  - a (
                                                    _div (
                                                      f a
                                                    )
                                                     der
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
                        fail "No converging solution found, iteration limit reached"
                      )
                       (
                        ret4 (
                          alist->hash-table (
                            _list (
                              cons "root" a
                            )
                             (
                              cons "error" (
                                abs_float (
                                  f a
                                )
                              )
                            )
                             (
                              cons "steps" steps
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
        poly x
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            ret7 (
              _add (
                - (
                  * x x
                )
                 (
                  * 5.0 x
                )
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
          result (
            newton_raphson poly 0.4 20 1e-06 1e-06 #f
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                string-append (
                  string-append (
                    string-append "root = " (
                      to-str-space (
                        hash-table-ref result "root"
                      )
                    )
                  )
                   ", error = "
                )
                 (
                  to-str-space (
                    hash-table-ref result "error"
                  )
                )
              )
            )
             (
              string-append (
                string-append (
                  string-append "root = " (
                    to-str-space (
                      hash-table-ref result "root"
                    )
                  )
                )
                 ", error = "
              )
               (
                to-str-space (
                  hash-table-ref result "error"
                )
              )
            )
             (
              to-str (
                string-append (
                  string-append (
                    string-append "root = " (
                      to-str-space (
                        hash-table-ref result "root"
                      )
                    )
                  )
                   ", error = "
                )
                 (
                  to-str-space (
                    hash-table-ref result "error"
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
     (
      let (
        (
          end9 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur10 (
              quotient (
                * (
                  - end9 start8
                )
                 1000000
              )
               jps11
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur10
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
