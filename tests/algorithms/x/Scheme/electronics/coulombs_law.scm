;; Generated on 2025-08-07 08:40 +0700
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
      start9 (
        current-jiffy
      )
    )
     (
      jps12 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          COULOMBS_CONSTANT 8988000000.0
        )
      )
       (
        begin (
          define (
            abs x
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                begin (
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
                    quote (
                      
                    )
                  )
                )
                 (
                  ret1 x
                )
              )
            )
          )
        )
         (
          define (
            sqrtApprox x
          )
           (
            call/cc (
              lambda (
                ret2
              )
               (
                begin (
                  if (
                    <= x 0.0
                  )
                   (
                    begin (
                      ret2 0.0
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
                      guess x
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
                                        < i 20
                                      )
                                       (
                                        begin (
                                          set! guess (
                                            _div (
                                              _add guess (
                                                _div x guess
                                              )
                                            )
                                             2.0
                                          )
                                        )
                                         (
                                          set! i (
                                            + i 1
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
                          ret2 guess
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
            coulombs_law force charge1 charge2 distance
          )
           (
            call/cc (
              lambda (
                ret5
              )
               (
                let (
                  (
                    charge_product (
                      abs (
                        * charge1 charge2
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        zero_count 0
                      )
                    )
                     (
                      begin (
                        if (
                          equal? force 0.0
                        )
                         (
                          begin (
                            set! zero_count (
                              + zero_count 1
                            )
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        if (
                          equal? charge1 0.0
                        )
                         (
                          begin (
                            set! zero_count (
                              + zero_count 1
                            )
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        if (
                          equal? charge2 0.0
                        )
                         (
                          begin (
                            set! zero_count (
                              + zero_count 1
                            )
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        if (
                          equal? distance 0.0
                        )
                         (
                          begin (
                            set! zero_count (
                              + zero_count 1
                            )
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
                            equal? zero_count 1
                          )
                        )
                         (
                          begin (
                            panic "One and only one argument must be 0"
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        if (
                          < distance 0.0
                        )
                         (
                          begin (
                            panic "Distance cannot be negative"
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        if (
                          equal? force 0.0
                        )
                         (
                          begin (
                            let (
                              (
                                f (
                                  _div (
                                    * COULOMBS_CONSTANT charge_product
                                  )
                                   (
                                    * distance distance
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                ret5 (
                                  alist->hash-table (
                                    _list (
                                      cons "force" f
                                    )
                                  )
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
                        if (
                          equal? charge1 0.0
                        )
                         (
                          begin (
                            let (
                              (
                                c1 (
                                  _div (
                                    * (
                                      abs force
                                    )
                                     (
                                      * distance distance
                                    )
                                  )
                                   (
                                    * COULOMBS_CONSTANT charge2
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                ret5 (
                                  alist->hash-table (
                                    _list (
                                      cons "charge1" c1
                                    )
                                  )
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
                        if (
                          equal? charge2 0.0
                        )
                         (
                          begin (
                            let (
                              (
                                c2 (
                                  _div (
                                    * (
                                      abs force
                                    )
                                     (
                                      * distance distance
                                    )
                                  )
                                   (
                                    * COULOMBS_CONSTANT charge1
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                ret5 (
                                  alist->hash-table (
                                    _list (
                                      cons "charge2" c2
                                    )
                                  )
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
                            d (
                              sqrtApprox (
                                _div (
                                  * COULOMBS_CONSTANT charge_product
                                )
                                 (
                                  abs force
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            ret5 (
                              alist->hash-table (
                                _list (
                                  cons "distance" d
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
            print_map m
          )
           (
            call/cc (
              lambda (
                ret6
              )
               (
                call/cc (
                  lambda (
                    break8
                  )
                   (
                    letrec (
                      (
                        loop7 (
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
                                    k (
                                      car xs
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
                                              string-append (
                                                string-append "{\"" k
                                              )
                                               "\": "
                                            )
                                             (
                                              to-str-space (
                                                hash-table-ref/default m k (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           "}"
                                        )
                                      )
                                       (
                                        string-append (
                                          string-append (
                                            string-append (
                                              string-append "{\"" k
                                            )
                                             "\": "
                                          )
                                           (
                                            to-str-space (
                                              hash-table-ref/default m k (
                                                quote (
                                                  
                                                )
                                              )
                                            )
                                          )
                                        )
                                         "}"
                                      )
                                       (
                                        to-str (
                                          string-append (
                                            string-append (
                                              string-append (
                                                string-append "{\"" k
                                              )
                                               "\": "
                                            )
                                             (
                                              to-str-space (
                                                hash-table-ref/default m k (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           "}"
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
                                loop7 (
                                  cdr xs
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      loop7 (
                        hash-table-keys m
                      )
                    )
                  )
                )
              )
            )
          )
        )
         (
          print_map (
            coulombs_law 0.0 3.0 5.0 2000.0
          )
        )
         (
          print_map (
            coulombs_law 10.0 3.0 5.0 0.0
          )
        )
         (
          print_map (
            coulombs_law 10.0 0.0 5.0 2000.0
          )
        )
      )
    )
     (
      let (
        (
          end10 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur11 (
              quotient (
                * (
                  - end10 start9
                )
                 1000000
              )
               jps12
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur11
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
