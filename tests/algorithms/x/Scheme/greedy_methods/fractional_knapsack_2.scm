;; Generated on 2025-08-07 08:56 +0700
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
      start15 (
        current-jiffy
      )
    )
     (
      jps18 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        sort_by_ratio index ratio
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
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
                              < i (
                                _len index
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    key (
                                      list-ref index i
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        key_ratio (
                                          list-ref ratio key
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            j (
                                              - i 1
                                            )
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
                                                          and (
                                                            >= j 0
                                                          )
                                                           (
                                                            < (
                                                              list-ref ratio (
                                                                list-ref index j
                                                              )
                                                            )
                                                             key_ratio
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            list-set! index (
                                                              + j 1
                                                            )
                                                             (
                                                              list-ref index j
                                                            )
                                                          )
                                                           (
                                                            set! j (
                                                              - j 1
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
                                            list-set! index (
                                              + j 1
                                            )
                                             key
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
                ret1 index
              )
            )
          )
        )
      )
    )
     (
      define (
        fractional_knapsack value weight capacity
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
                  _len value
                )
              )
            )
             (
              begin (
                let (
                  (
                    index (
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
                                      < i n
                                    )
                                     (
                                      begin (
                                        set! index (
                                          append index (
                                            _list i
                                          )
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
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
                        let (
                          (
                            ratio (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            set! i 0
                          )
                           (
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
                                          < i n
                                        )
                                         (
                                          begin (
                                            set! ratio (
                                              append ratio (
                                                _list (
                                                  _div (
                                                    list-ref value i
                                                  )
                                                   (
                                                    list-ref weight i
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
                            set! index (
                              sort_by_ratio index ratio
                            )
                          )
                           (
                            let (
                              (
                                fractions (
                                  _list
                                )
                              )
                            )
                             (
                              begin (
                                set! i 0
                              )
                               (
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
                                              < i n
                                            )
                                             (
                                              begin (
                                                set! fractions (
                                                  append fractions (
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
                                let (
                                  (
                                    max_value 0.0
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
                                                      < idx (
                                                        _len index
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            item (
                                                              list-ref index idx
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              <= (
                                                                list-ref weight item
                                                              )
                                                               capacity
                                                            )
                                                             (
                                                              begin (
                                                                list-set! fractions item 1.0
                                                              )
                                                               (
                                                                set! max_value (
                                                                  + max_value (
                                                                    list-ref value item
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! capacity (
                                                                  - capacity (
                                                                    list-ref weight item
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                list-set! fractions item (
                                                                  _div capacity (
                                                                    list-ref weight item
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! max_value (
                                                                  _add max_value (
                                                                    _div (
                                                                      * (
                                                                        list-ref value item
                                                                      )
                                                                       capacity
                                                                    )
                                                                     (
                                                                      list-ref weight item
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                break14 (
                                                                  quote (
                                                                    
                                                                  )
                                                                )
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
                                        ret6 (
                                          alist->hash-table (
                                            _list (
                                              cons "max_value" max_value
                                            )
                                             (
                                              cons "fractions" fractions
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
      let (
        (
          v (
            _list 1.0 3.0 5.0 7.0 9.0
          )
        )
      )
       (
        begin (
          let (
            (
              w (
                _list 0.9 0.7 0.5 0.3 0.1
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    fractional_knapsack v w 5.0
                  )
                )
                 (
                  fractional_knapsack v w 5.0
                )
                 (
                  to-str (
                    fractional_knapsack v w 5.0
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
                    fractional_knapsack (
                      _list 1.0 3.0 5.0 7.0
                    )
                     (
                      _list 0.9 0.7 0.5 0.3
                    )
                     30.0
                  )
                )
                 (
                  fractional_knapsack (
                    _list 1.0 3.0 5.0 7.0
                  )
                   (
                    _list 0.9 0.7 0.5 0.3
                  )
                   30.0
                )
                 (
                  to-str (
                    fractional_knapsack (
                      _list 1.0 3.0 5.0 7.0
                    )
                     (
                      _list 0.9 0.7 0.5 0.3
                    )
                     30.0
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
                    fractional_knapsack (
                      _list
                    )
                     (
                      _list
                    )
                     30.0
                  )
                )
                 (
                  fractional_knapsack (
                    _list
                  )
                   (
                    _list
                  )
                   30.0
                )
                 (
                  to-str (
                    fractional_knapsack (
                      _list
                    )
                     (
                      _list
                    )
                     30.0
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
          end16 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur17 (
              quotient (
                * (
                  - end16 start15
                )
                 1000000
              )
               jps18
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur17
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
