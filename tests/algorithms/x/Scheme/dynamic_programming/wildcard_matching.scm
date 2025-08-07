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
        make_bool_list n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                row (
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
                                    set! row (
                                      append row (
                                        _list #f
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
                    ret1 row
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
        make_bool_matrix rows cols
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                matrix (
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
                                  < i rows
                                )
                                 (
                                  begin (
                                    set! matrix (
                                      append matrix (
                                        _list (
                                          make_bool_list cols
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
                    ret4 matrix
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
        is_match s p
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                n (
                  _len s
                )
              )
            )
             (
              begin (
                let (
                  (
                    m (
                      _len p
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        dp (
                          make_bool_matrix (
                            + n 1
                          )
                           (
                            + m 1
                          )
                        )
                      )
                    )
                     (
                      begin (
                        list-set! (
                          list-ref dp 0
                        )
                         0 #t
                      )
                       (
                        let (
                          (
                            j 1
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
                                          <= j m
                                        )
                                         (
                                          begin (
                                            if (
                                              string=? (
                                                _substring p (
                                                  - j 1
                                                )
                                                 j
                                              )
                                               "*"
                                            )
                                             (
                                              begin (
                                                list-set! (
                                                  list-ref dp 0
                                                )
                                                 j (
                                                  cond (
                                                    (
                                                      string? (
                                                        cond (
                                                          (
                                                            string? dp
                                                          )
                                                           (
                                                            _substring dp 0 (
                                                              + 0 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          (
                                                            hash-table? dp
                                                          )
                                                           (
                                                            hash-table-ref dp 0
                                                          )
                                                        )
                                                         (
                                                          else (
                                                            list-ref dp 0
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        cond (
                                                          (
                                                            string? dp
                                                          )
                                                           (
                                                            _substring dp 0 (
                                                              + 0 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          (
                                                            hash-table? dp
                                                          )
                                                           (
                                                            hash-table-ref dp 0
                                                          )
                                                        )
                                                         (
                                                          else (
                                                            list-ref dp 0
                                                          )
                                                        )
                                                      )
                                                       (
                                                        - j 1
                                                      )
                                                       (
                                                        + (
                                                          - j 1
                                                        )
                                                         1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        cond (
                                                          (
                                                            string? dp
                                                          )
                                                           (
                                                            _substring dp 0 (
                                                              + 0 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          (
                                                            hash-table? dp
                                                          )
                                                           (
                                                            hash-table-ref dp 0
                                                          )
                                                        )
                                                         (
                                                          else (
                                                            list-ref dp 0
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        cond (
                                                          (
                                                            string? dp
                                                          )
                                                           (
                                                            _substring dp 0 (
                                                              + 0 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          (
                                                            hash-table? dp
                                                          )
                                                           (
                                                            hash-table-ref dp 0
                                                          )
                                                        )
                                                         (
                                                          else (
                                                            list-ref dp 0
                                                          )
                                                        )
                                                      )
                                                       (
                                                        - j 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref (
                                                        cond (
                                                          (
                                                            string? dp
                                                          )
                                                           (
                                                            _substring dp 0 (
                                                              + 0 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          (
                                                            hash-table? dp
                                                          )
                                                           (
                                                            hash-table-ref dp 0
                                                          )
                                                        )
                                                         (
                                                          else (
                                                            list-ref dp 0
                                                          )
                                                        )
                                                      )
                                                       (
                                                        - j 1
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
                            let (
                              (
                                i 1
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
                                              <= i n
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    j2 1
                                                  )
                                                )
                                                 (
                                                  begin (
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
                                                                  <= j2 m
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        pc (
                                                                          _substring p (
                                                                            - j2 1
                                                                          )
                                                                           j2
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            sc (
                                                                              _substring s (
                                                                                - i 1
                                                                              )
                                                                               i
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            if (
                                                                              or (
                                                                                string=? pc sc
                                                                              )
                                                                               (
                                                                                string=? pc "?"
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                list-set! (
                                                                                  list-ref dp i
                                                                                )
                                                                                 j2 (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        cond (
                                                                                          (
                                                                                            string? dp
                                                                                          )
                                                                                           (
                                                                                            _substring dp (
                                                                                              - i 1
                                                                                            )
                                                                                             (
                                                                                              + (
                                                                                                - i 1
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
                                                                                              - i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref dp (
                                                                                              - i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        cond (
                                                                                          (
                                                                                            string? dp
                                                                                          )
                                                                                           (
                                                                                            _substring dp (
                                                                                              - i 1
                                                                                            )
                                                                                             (
                                                                                              + (
                                                                                                - i 1
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
                                                                                              - i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref dp (
                                                                                              - i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        - j2 1
                                                                                      )
                                                                                       (
                                                                                        + (
                                                                                          - j2 1
                                                                                        )
                                                                                         1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        cond (
                                                                                          (
                                                                                            string? dp
                                                                                          )
                                                                                           (
                                                                                            _substring dp (
                                                                                              - i 1
                                                                                            )
                                                                                             (
                                                                                              + (
                                                                                                - i 1
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
                                                                                              - i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref dp (
                                                                                              - i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        cond (
                                                                                          (
                                                                                            string? dp
                                                                                          )
                                                                                           (
                                                                                            _substring dp (
                                                                                              - i 1
                                                                                            )
                                                                                             (
                                                                                              + (
                                                                                                - i 1
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
                                                                                              - i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref dp (
                                                                                              - i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        - j2 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        cond (
                                                                                          (
                                                                                            string? dp
                                                                                          )
                                                                                           (
                                                                                            _substring dp (
                                                                                              - i 1
                                                                                            )
                                                                                             (
                                                                                              + (
                                                                                                - i 1
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
                                                                                              - i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref dp (
                                                                                              - i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        - j2 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              if (
                                                                                string=? pc "*"
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  if (
                                                                                    or (
                                                                                      cond (
                                                                                        (
                                                                                          string? (
                                                                                            cond (
                                                                                              (
                                                                                                string? dp
                                                                                              )
                                                                                               (
                                                                                                _substring dp (
                                                                                                  - i 1
                                                                                                )
                                                                                                 (
                                                                                                  + (
                                                                                                    - i 1
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
                                                                                                  - i 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref dp (
                                                                                                  - i 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          _substring (
                                                                                            cond (
                                                                                              (
                                                                                                string? dp
                                                                                              )
                                                                                               (
                                                                                                _substring dp (
                                                                                                  - i 1
                                                                                                )
                                                                                                 (
                                                                                                  + (
                                                                                                    - i 1
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
                                                                                                  - i 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref dp (
                                                                                                  - i 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           j2 (
                                                                                            + j2 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? (
                                                                                            cond (
                                                                                              (
                                                                                                string? dp
                                                                                              )
                                                                                               (
                                                                                                _substring dp (
                                                                                                  - i 1
                                                                                                )
                                                                                                 (
                                                                                                  + (
                                                                                                    - i 1
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
                                                                                                  - i 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref dp (
                                                                                                  - i 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref (
                                                                                            cond (
                                                                                              (
                                                                                                string? dp
                                                                                              )
                                                                                               (
                                                                                                _substring dp (
                                                                                                  - i 1
                                                                                                )
                                                                                                 (
                                                                                                  + (
                                                                                                    - i 1
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
                                                                                                  - i 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref dp (
                                                                                                  - i 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           j2
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref (
                                                                                            cond (
                                                                                              (
                                                                                                string? dp
                                                                                              )
                                                                                               (
                                                                                                _substring dp (
                                                                                                  - i 1
                                                                                                )
                                                                                                 (
                                                                                                  + (
                                                                                                    - i 1
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
                                                                                                  - i 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref dp (
                                                                                                  - i 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           j2
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      cond (
                                                                                        (
                                                                                          string? (
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
                                                                                        )
                                                                                         (
                                                                                          _substring (
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
                                                                                            - j2 1
                                                                                          )
                                                                                           (
                                                                                            + (
                                                                                              - j2 1
                                                                                            )
                                                                                             1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? (
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
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref (
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
                                                                                            - j2 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref (
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
                                                                                            - j2 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      list-set! (
                                                                                        list-ref dp i
                                                                                      )
                                                                                       j2 #t
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    quote (
                                                                                      
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
                                                                           (
                                                                            set! j2 (
                                                                              + j2 1
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
                                                    set! i (
                                                      + i 1
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
                                ret7 (
                                  cond (
                                    (
                                      string? (
                                        cond (
                                          (
                                            string? dp
                                          )
                                           (
                                            _substring dp n (
                                              + n 1
                                            )
                                          )
                                        )
                                         (
                                          (
                                            hash-table? dp
                                          )
                                           (
                                            hash-table-ref dp n
                                          )
                                        )
                                         (
                                          else (
                                            list-ref dp n
                                          )
                                        )
                                      )
                                    )
                                     (
                                      _substring (
                                        cond (
                                          (
                                            string? dp
                                          )
                                           (
                                            _substring dp n (
                                              + n 1
                                            )
                                          )
                                        )
                                         (
                                          (
                                            hash-table? dp
                                          )
                                           (
                                            hash-table-ref dp n
                                          )
                                        )
                                         (
                                          else (
                                            list-ref dp n
                                          )
                                        )
                                      )
                                       m (
                                        + m 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        cond (
                                          (
                                            string? dp
                                          )
                                           (
                                            _substring dp n (
                                              + n 1
                                            )
                                          )
                                        )
                                         (
                                          (
                                            hash-table? dp
                                          )
                                           (
                                            hash-table-ref dp n
                                          )
                                        )
                                         (
                                          else (
                                            list-ref dp n
                                          )
                                        )
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        cond (
                                          (
                                            string? dp
                                          )
                                           (
                                            _substring dp n (
                                              + n 1
                                            )
                                          )
                                        )
                                         (
                                          (
                                            hash-table? dp
                                          )
                                           (
                                            hash-table-ref dp n
                                          )
                                        )
                                         (
                                          else (
                                            list-ref dp n
                                          )
                                        )
                                      )
                                       m
                                    )
                                  )
                                   (
                                    else (
                                      list-ref (
                                        cond (
                                          (
                                            string? dp
                                          )
                                           (
                                            _substring dp n (
                                              + n 1
                                            )
                                          )
                                        )
                                         (
                                          (
                                            hash-table? dp
                                          )
                                           (
                                            hash-table-ref dp n
                                          )
                                        )
                                         (
                                          else (
                                            list-ref dp n
                                          )
                                        )
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
      )
    )
     (
      define (
        print_bool b
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            if b (
              begin (
                _display (
                  if (
                    string? (
                      if #t #t #f
                    )
                  )
                   (
                    if #t #t #f
                  )
                   (
                    to-str (
                      if #t #t #f
                    )
                  )
                )
              )
               (
                newline
              )
            )
             (
              begin (
                _display (
                  if (
                    string? (
                      if #f #t #f
                    )
                  )
                   (
                    if #f #t #f
                  )
                   (
                    to-str (
                      if #f #t #f
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
     (
      print_bool (
        is_match "abc" "a*c"
      )
    )
     (
      print_bool (
        is_match "abc" "a*d"
      )
    )
     (
      print_bool (
        is_match "baaabab" "*****ba*****ab"
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
