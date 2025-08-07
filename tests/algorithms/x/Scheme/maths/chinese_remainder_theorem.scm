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
      start5 (
        current-jiffy
      )
    )
     (
      jps8 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        extended_euclid a b
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                equal? b 0
              )
               (
                begin (
                  ret1 (
                    alist->hash-table (
                      _list (
                        cons "x" 1
                      )
                       (
                        cons "y" 0
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
                  res (
                    extended_euclid b (
                      _mod a b
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      k (
                        _div a b
                      )
                    )
                  )
                   (
                    begin (
                      ret1 (
                        alist->hash-table (
                          _list (
                            cons "x" (
                              hash-table-ref res "y"
                            )
                          )
                           (
                            cons "y" (
                              - (
                                hash-table-ref res "x"
                              )
                               (
                                * k (
                                  hash-table-ref res "y"
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
        chinese_remainder_theorem n1 r1 n2 r2
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                res (
                  extended_euclid n1 n2
                )
              )
            )
             (
              begin (
                let (
                  (
                    x (
                      hash-table-ref res "x"
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        y (
                          hash-table-ref res "y"
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            m (
                              * n1 n2
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                n (
                                  _add (
                                    * (
                                      * r2 x
                                    )
                                     n1
                                  )
                                   (
                                    * (
                                      * r1 y
                                    )
                                     n2
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                ret2 (
                                  _mod (
                                    _add (
                                      _mod n m
                                    )
                                     m
                                  )
                                   m
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
        invert_modulo a n
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            let (
              (
                res (
                  extended_euclid a n
                )
              )
            )
             (
              begin (
                let (
                  (
                    b (
                      hash-table-ref res "x"
                    )
                  )
                )
                 (
                  begin (
                    if (
                      _lt b 0
                    )
                     (
                      begin (
                        set! b (
                          _mod (
                            _add (
                              _mod b n
                            )
                             n
                          )
                           n
                        )
                      )
                    )
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
                    ret3 b
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
        chinese_remainder_theorem2 n1 r1 n2 r2
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                x (
                  invert_modulo n1 n2
                )
              )
            )
             (
              begin (
                let (
                  (
                    y (
                      invert_modulo n2 n1
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        m (
                          * n1 n2
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            n (
                              _add (
                                * (
                                  * r2 x
                                )
                                 n1
                              )
                               (
                                * (
                                  * r1 y
                                )
                                 n2
                              )
                            )
                          )
                        )
                         (
                          begin (
                            ret4 (
                              _mod (
                                _add (
                                  _mod n m
                                )
                                 m
                              )
                               m
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
          e1 (
            extended_euclid 10 6
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
                    to-str-space (
                      hash-table-ref e1 "x"
                    )
                  )
                   ","
                )
                 (
                  to-str-space (
                    hash-table-ref e1 "y"
                  )
                )
              )
            )
             (
              string-append (
                string-append (
                  to-str-space (
                    hash-table-ref e1 "x"
                  )
                )
                 ","
              )
               (
                to-str-space (
                  hash-table-ref e1 "y"
                )
              )
            )
             (
              to-str (
                string-append (
                  string-append (
                    to-str-space (
                      hash-table-ref e1 "x"
                    )
                  )
                   ","
                )
                 (
                  to-str-space (
                    hash-table-ref e1 "y"
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
              e2 (
                extended_euclid 7 5
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
                        to-str-space (
                          hash-table-ref e2 "x"
                        )
                      )
                       ","
                    )
                     (
                      to-str-space (
                        hash-table-ref e2 "y"
                      )
                    )
                  )
                )
                 (
                  string-append (
                    string-append (
                      to-str-space (
                        hash-table-ref e2 "x"
                      )
                    )
                     ","
                  )
                   (
                    to-str-space (
                      hash-table-ref e2 "y"
                    )
                  )
                )
                 (
                  to-str (
                    string-append (
                      string-append (
                        to-str-space (
                          hash-table-ref e2 "x"
                        )
                      )
                       ","
                    )
                     (
                      to-str-space (
                        hash-table-ref e2 "y"
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
              _display (
                if (
                  string? (
                    to-str-space (
                      chinese_remainder_theorem 5 1 7 3
                    )
                  )
                )
                 (
                  to-str-space (
                    chinese_remainder_theorem 5 1 7 3
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      chinese_remainder_theorem 5 1 7 3
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
                  string? (
                    to-str-space (
                      chinese_remainder_theorem 6 1 4 3
                    )
                  )
                )
                 (
                  to-str-space (
                    chinese_remainder_theorem 6 1 4 3
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      chinese_remainder_theorem 6 1 4 3
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
                  string? (
                    to-str-space (
                      invert_modulo 2 5
                    )
                  )
                )
                 (
                  to-str-space (
                    invert_modulo 2 5
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      invert_modulo 2 5
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
                  string? (
                    to-str-space (
                      invert_modulo 8 7
                    )
                  )
                )
                 (
                  to-str-space (
                    invert_modulo 8 7
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      invert_modulo 8 7
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
                  string? (
                    to-str-space (
                      chinese_remainder_theorem2 5 1 7 3
                    )
                  )
                )
                 (
                  to-str-space (
                    chinese_remainder_theorem2 5 1 7 3
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      chinese_remainder_theorem2 5 1 7 3
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
                  string? (
                    to-str-space (
                      chinese_remainder_theorem2 6 1 4 3
                    )
                  )
                )
                 (
                  to-str-space (
                    chinese_remainder_theorem2 6 1 4 3
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      chinese_remainder_theorem2 6 1 4 3
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
          end6 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur7 (
              quotient (
                * (
                  - end6 start5
                )
                 1000000
              )
               jps8
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur7
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
