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
      start18 (
        current-jiffy
      )
    )
     (
      jps21 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        longest_distance graph
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                n (
                  _len graph
                )
              )
            )
             (
              begin (
                let (
                  (
                    indegree (
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
                                      < i n
                                    )
                                     (
                                      begin (
                                        set! indegree (
                                          append indegree (
                                            _list 0
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
                        let (
                          (
                            long_dist (
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
                                              < j n
                                            )
                                             (
                                              begin (
                                                set! long_dist (
                                                  append long_dist (
                                                    _list 1
                                                  )
                                                )
                                              )
                                               (
                                                set! j (
                                                  + j 1
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
                                let (
                                  (
                                    u 0
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
                                                  < u n
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
                                                                        v (
                                                                          car xs
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        list-set! indegree v (
                                                                          + (
                                                                            list-ref indegree v
                                                                          )
                                                                           1
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    loop8 (
                                                                      cdr xs
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          loop8 (
                                                            list-ref graph u
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! u (
                                                      + u 1
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
                                    let (
                                      (
                                        queue (
                                          _list
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            head 0
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
                                                              < k n
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  equal? (
                                                                    list-ref indegree k
                                                                  )
                                                                   0
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! queue (
                                                                      append queue (
                                                                        _list k
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
                                                                set! k (
                                                                  + k 1
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
                                                              < head (
                                                                _len queue
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    vertex (
                                                                      list-ref queue head
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! head (
                                                                      + head 1
                                                                    )
                                                                  )
                                                                   (
                                                                    call/cc (
                                                                      lambda (
                                                                        break15
                                                                      )
                                                                       (
                                                                        letrec (
                                                                          (
                                                                            loop14 (
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
                                                                                        x (
                                                                                          car xs
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        list-set! indegree x (
                                                                                          - (
                                                                                            list-ref indegree x
                                                                                          )
                                                                                           1
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        let (
                                                                                          (
                                                                                            new_dist (
                                                                                              + (
                                                                                                list-ref long_dist vertex
                                                                                              )
                                                                                               1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            if (
                                                                                              > new_dist (
                                                                                                list-ref long_dist x
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                list-set! long_dist x new_dist
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              quote (
                                                                                                
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            if (
                                                                                              equal? (
                                                                                                list-ref indegree x
                                                                                              )
                                                                                               0
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! queue (
                                                                                                  append queue (
                                                                                                    _list x
                                                                                                  )
                                                                                                )
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
                                                                                  )
                                                                                   (
                                                                                    loop14 (
                                                                                      cdr xs
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          loop14 (
                                                                            list-ref graph vertex
                                                                          )
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
                                                    max_len (
                                                      list-ref long_dist 0
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        m 1
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
                                                                      < m n
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          > (
                                                                            list-ref long_dist m
                                                                          )
                                                                           max_len
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! max_len (
                                                                              list-ref long_dist m
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          quote (
                                                                            
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        set! m (
                                                                          + m 1
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
                                                        ret1 max_len
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
            )
          )
        )
      )
    )
     (
      let (
        (
          graph (
            _list (
              _list 2 3 4
            )
             (
              _list 2 7
            )
             (
              _list 5
            )
             (
              _list 5 7
            )
             (
              _list 7
            )
             (
              _list 6
            )
             (
              _list 7
            )
             (
              _list
            )
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                longest_distance graph
              )
            )
             (
              longest_distance graph
            )
             (
              to-str (
                longest_distance graph
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
          end19 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur20 (
              quotient (
                * (
                  - end19 start18
                )
                 1000000
              )
               jps21
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur20
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
