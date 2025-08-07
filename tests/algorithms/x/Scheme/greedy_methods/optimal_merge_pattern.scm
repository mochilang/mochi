;; Generated on 2025-08-07 14:57 +0700
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
(define (_div a b) (if (and (integer? a) (integer? b) (exact? a) (exact? b)) (quotient a b) (/ a b)))
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
(define (list-ref-safe lst idx) (if (and (integer? idx) (>= idx 0) (< idx (length lst))) (list-ref lst idx) '()))
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
        index_of_min xs
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                min_idx 0
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
                                  < i (
                                    _len xs
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      < (
                                        list-ref-safe xs i
                                      )
                                       (
                                        list-ref-safe xs min_idx
                                      )
                                    )
                                     (
                                      begin (
                                        set! min_idx i
                                      )
                                    )
                                     (
                                      quote (
                                        
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
                    ret1 min_idx
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
        remove_at xs idx
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                res (
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
                                  < i (
                                    _len xs
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      not (
                                        equal? i idx
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              list-ref-safe xs i
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
                    ret4 res
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
        optimal_merge_pattern files
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                arr files
              )
            )
             (
              begin (
                let (
                  (
                    optimal_merge_cost 0
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
                                  > (
                                    _len arr
                                  )
                                   1
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        temp 0
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
                                                          < k 2
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                min_idx (
                                                                  index_of_min arr
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! temp (
                                                                  + temp (
                                                                    list-ref-safe arr min_idx
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! arr (
                                                                  remove_at arr min_idx
                                                                )
                                                              )
                                                               (
                                                                set! k (
                                                                  + k 1
                                                                )
                                                              )
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
                                            set! arr (
                                              append arr (
                                                _list temp
                                              )
                                            )
                                          )
                                           (
                                            set! optimal_merge_cost (
                                              + optimal_merge_cost temp
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
                    ret7 optimal_merge_cost
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
            optimal_merge_pattern (
              _list 2 3 4
            )
          )
        )
         (
          optimal_merge_pattern (
            _list 2 3 4
          )
        )
         (
          to-str (
            optimal_merge_pattern (
              _list 2 3 4
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
            optimal_merge_pattern (
              _list 5 10 20 30 30
            )
          )
        )
         (
          optimal_merge_pattern (
            _list 5 10 20 30 30
          )
        )
         (
          to-str (
            optimal_merge_pattern (
              _list 5 10 20 30 30
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
            optimal_merge_pattern (
              _list 8 8 8 8 8
            )
          )
        )
         (
          optimal_merge_pattern (
            _list 8 8 8 8 8
          )
        )
         (
          to-str (
            optimal_merge_pattern (
              _list 8 8 8 8 8
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
