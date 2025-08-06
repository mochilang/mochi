;; Generated on 2025-08-06 23:57 +0700
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
      start7 (
        current-jiffy
      )
    )
     (
      jps10 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        partition arr low high
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                pivot (
                  list-ref arr high
                )
              )
            )
             (
              begin (
                let (
                  (
                    i (
                      - low 1
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        j low
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
                                      < j high
                                    )
                                     (
                                      begin (
                                        if (
                                          >= (
                                            list-ref arr j
                                          )
                                           pivot
                                        )
                                         (
                                          begin (
                                            set! i (
                                              + i 1
                                            )
                                          )
                                           (
                                            let (
                                              (
                                                tmp (
                                                  list-ref arr i
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                list-set! arr i (
                                                  list-ref arr j
                                                )
                                              )
                                               (
                                                list-set! arr j tmp
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
                            k (
                              + i 1
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                tmp (
                                  list-ref arr k
                                )
                              )
                            )
                             (
                              begin (
                                list-set! arr k (
                                  list-ref arr high
                                )
                              )
                               (
                                list-set! arr high tmp
                              )
                               (
                                ret1 k
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
        kth_largest_element arr position
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                equal? (
                  _len arr
                )
                 0
              )
               (
                begin (
                  ret4 (
                    - 1
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
                or (
                  < position 1
                )
                 (
                  > position (
                    _len arr
                  )
                )
              )
               (
                begin (
                  ret4 (
                    - 1
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
                  low 0
                )
              )
               (
                begin (
                  let (
                    (
                      high (
                        - (
                          _len arr
                        )
                         1
                      )
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
                                    <= low high
                                  )
                                   (
                                    begin (
                                      if (
                                        or (
                                          > low (
                                            - (
                                              _len arr
                                            )
                                             1
                                          )
                                        )
                                         (
                                          < high 0
                                        )
                                      )
                                       (
                                        begin (
                                          ret4 (
                                            - 1
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
                                          pivot_index (
                                            partition arr low high
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            equal? pivot_index (
                                              - position 1
                                            )
                                          )
                                           (
                                            begin (
                                              ret4 (
                                                list-ref arr pivot_index
                                              )
                                            )
                                          )
                                           (
                                            if (
                                              _gt pivot_index (
                                                - position 1
                                              )
                                            )
                                             (
                                              begin (
                                                set! high (
                                                  - pivot_index 1
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! low (
                                                  _add pivot_index 1
                                                )
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
                      ret4 (
                        - 1
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
          arr1 (
            _list 3 1 4 1 5 9 2 6 5 3 5
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                kth_largest_element arr1 3
              )
            )
             (
              kth_largest_element arr1 3
            )
             (
              to-str (
                kth_largest_element arr1 3
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
              string? "\n"
            )
             "\n" (
              to-str "\n"
            )
          )
        )
         (
          newline
        )
         (
          let (
            (
              arr2 (
                _list 2 5 6 1 9 3 8 4 7 3 5
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    kth_largest_element arr2 1
                  )
                )
                 (
                  kth_largest_element arr2 1
                )
                 (
                  to-str (
                    kth_largest_element arr2 1
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
          end8 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur9 (
              quotient (
                * (
                  - end8 start7
                )
                 1000000
              )
               jps10
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur9
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
