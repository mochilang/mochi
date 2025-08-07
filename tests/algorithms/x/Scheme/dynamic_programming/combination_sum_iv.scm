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
        make_list len value
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
                                  < i len
                                )
                                 (
                                  begin (
                                    set! arr (
                                      append arr (
                                        _list value
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
                    ret1 arr
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
        count_recursive array target
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                < target 0
              )
               (
                begin (
                  ret4 0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                equal? target 0
              )
               (
                begin (
                  ret4 1
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
                  total 0
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
                                    < i (
                                      _len array
                                    )
                                  )
                                   (
                                    begin (
                                      set! total (
                                        _add total (
                                          count_recursive array (
                                            - target (
                                              list-ref array i
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
                      ret4 total
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
        combination_sum_iv array target
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            ret7 (
              count_recursive array target
            )
          )
        )
      )
    )
     (
      define (
        count_dp array target dp
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            begin (
              if (
                < target 0
              )
               (
                begin (
                  ret8 0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                equal? target 0
              )
               (
                begin (
                  ret8 1
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                > (
                  list-ref dp target
                )
                 (
                  - 0 1
                )
              )
               (
                begin (
                  ret8 (
                    list-ref dp target
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
                  total 0
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
                                      _len array
                                    )
                                  )
                                   (
                                    begin (
                                      set! total (
                                        _add total (
                                          count_dp array (
                                            - target (
                                              list-ref array i
                                            )
                                          )
                                           dp
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
                      list-set! dp target total
                    )
                     (
                      ret8 total
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
        combination_sum_iv_dp_array array target
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            let (
              (
                dp (
                  make_list (
                    + target 1
                  )
                   (
                    - 1
                  )
                )
              )
            )
             (
              begin (
                ret11 (
                  count_dp array target dp
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        combination_sum_iv_bottom_up n array target
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                dp (
                  make_list (
                    + target 1
                  )
                   0
                )
              )
            )
             (
              begin (
                list-set! dp 0 1
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
                                  <= i target
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
                                            break16
                                          )
                                           (
                                            letrec (
                                              (
                                                loop15 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < j n
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          >= (
                                                            - i (
                                                              list-ref array j
                                                            )
                                                          )
                                                           0
                                                        )
                                                         (
                                                          begin (
                                                            list-set! dp i (
                                                              _add (
                                                                cond (
                                                                  (
                                                                    string? dp
                                                                  )
                                                                   (
                                                                    _substring dp i (
                                                                      + i 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? dp
                                                                  )
                                                                   (
                                                                    hash-table-ref dp i
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref dp i
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                cond (
                                                                  (
                                                                    string? dp
                                                                  )
                                                                   (
                                                                    _substring dp (
                                                                      - i (
                                                                        list-ref array j
                                                                      )
                                                                    )
                                                                     (
                                                                      + (
                                                                        - i (
                                                                          list-ref array j
                                                                        )
                                                                      )
                                                                       1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? dp
                                                                  )
                                                                   (
                                                                    hash-table-ref dp (
                                                                      - i (
                                                                        list-ref array j
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref dp (
                                                                      - i (
                                                                        list-ref array j
                                                                      )
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
                                                        set! j (
                                                          + j 1
                                                        )
                                                      )
                                                       (
                                                        loop15
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
                                              loop15
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
                    ret12 (
                      cond (
                        (
                          string? dp
                        )
                         (
                          _substring dp target (
                            + target 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? dp
                        )
                         (
                          hash-table-ref dp target
                        )
                      )
                       (
                        else (
                          list-ref dp target
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
      _display (
        if (
          string? (
            to-str-space (
              combination_sum_iv (
                _list 1 2 5
              )
               5
            )
          )
        )
         (
          to-str-space (
            combination_sum_iv (
              _list 1 2 5
            )
             5
          )
        )
         (
          to-str (
            to-str-space (
              combination_sum_iv (
                _list 1 2 5
              )
               5
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
              combination_sum_iv_dp_array (
                _list 1 2 5
              )
               5
            )
          )
        )
         (
          to-str-space (
            combination_sum_iv_dp_array (
              _list 1 2 5
            )
             5
          )
        )
         (
          to-str (
            to-str-space (
              combination_sum_iv_dp_array (
                _list 1 2 5
              )
               5
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
              combination_sum_iv_bottom_up 3 (
                _list 1 2 5
              )
               5
            )
          )
        )
         (
          to-str-space (
            combination_sum_iv_bottom_up 3 (
              _list 1 2 5
            )
             5
          )
        )
         (
          to-str (
            to-str-space (
              combination_sum_iv_bottom_up 3 (
                _list 1 2 5
              )
               5
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
