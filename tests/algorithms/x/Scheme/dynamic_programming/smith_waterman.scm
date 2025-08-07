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
        score_function source_char target_char match_score mismatch_score gap_score
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                or (
                  string=? source_char "-"
                )
                 (
                  string=? target_char "-"
                )
              )
               (
                begin (
                  ret1 gap_score
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? source_char target_char
              )
               (
                begin (
                  ret1 match_score
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret1 mismatch_score
            )
          )
        )
      )
    )
     (
      define (
        smith_waterman query subject match_score mismatch_score gap_score
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                q (
                  upper query
                )
              )
            )
             (
              begin (
                let (
                  (
                    s (
                      upper subject
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        m (
                          _len q
                        )
                      )
                    )
                     (
                      begin (
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
                                score (
                                  _list
                                )
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
                                            _
                                          )
                                           (
                                            if (
                                              < _ (
                                                + m 1
                                              )
                                            )
                                             (
                                              begin (
                                                begin (
                                                  let (
                                                    (
                                                      row (
                                                        _list
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
                                                                  _2
                                                                )
                                                                 (
                                                                  if (
                                                                    < _2 (
                                                                      + n 1
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      begin (
                                                                        set! row (
                                                                          append row (
                                                                            _list 0
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      loop5 (
                                                                        + _2 1
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
                                                           (
                                                            loop5 0
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      set! score (
                                                        append score (
                                                          _list row
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                loop3 (
                                                  + _ 1
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
                                     (
                                      loop3 0
                                    )
                                  )
                                )
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
                                            i
                                          )
                                           (
                                            if (
                                              < i (
                                                + m 1
                                              )
                                            )
                                             (
                                              begin (
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
                                                              j
                                                            )
                                                             (
                                                              if (
                                                                < j (
                                                                  + n 1
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        qc (
                                                                          _substring q (
                                                                            - i 1
                                                                          )
                                                                           i
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            sc (
                                                                              _substring s (
                                                                                - j 1
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                diag (
                                                                                  _add (
                                                                                    cond (
                                                                                      (
                                                                                        string? (
                                                                                          list-ref score (
                                                                                            - i 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        _substring (
                                                                                          list-ref score (
                                                                                            - i 1
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
                                                                                          list-ref score (
                                                                                            - i 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref (
                                                                                          list-ref score (
                                                                                            - i 1
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
                                                                                          list-ref score (
                                                                                            - i 1
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          - j 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    score_function qc sc match_score mismatch_score gap_score
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    delete (
                                                                                      + (
                                                                                        cond (
                                                                                          (
                                                                                            string? (
                                                                                              list-ref score (
                                                                                                - i 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            _substring (
                                                                                              list-ref score (
                                                                                                - i 1
                                                                                              )
                                                                                            )
                                                                                             j (
                                                                                              + j 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? (
                                                                                              list-ref score (
                                                                                                - i 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref (
                                                                                              list-ref score (
                                                                                                - i 1
                                                                                              )
                                                                                            )
                                                                                             j
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref (
                                                                                              list-ref score (
                                                                                                - i 1
                                                                                              )
                                                                                            )
                                                                                             j
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       gap_score
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        insert (
                                                                                          + (
                                                                                            cond (
                                                                                              (
                                                                                                string? (
                                                                                                  list-ref score i
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _substring (
                                                                                                  list-ref score i
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
                                                                                                  list-ref score i
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref (
                                                                                                  list-ref score i
                                                                                                )
                                                                                                 (
                                                                                                  - j 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref (
                                                                                                  list-ref score i
                                                                                                )
                                                                                                 (
                                                                                                  - j 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           gap_score
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            max_val 0
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            if (
                                                                                              _gt diag max_val
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! max_val diag
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              quote (
                                                                                                
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            if (
                                                                                              > delete max_val
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! max_val delete
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              quote (
                                                                                                
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            if (
                                                                                              > insert max_val
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! max_val insert
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              quote (
                                                                                                
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            list-set! (
                                                                                              list-ref score i
                                                                                            )
                                                                                             j max_val
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
                                                                  loop9 (
                                                                    + j 1
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
                                                       (
                                                        loop9 1
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                loop7 (
                                                  + i 1
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
                                     (
                                      loop7 1
                                    )
                                  )
                                )
                              )
                               (
                                ret2 score
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
        traceback score query subject match_score mismatch_score gap_score
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            let (
              (
                q (
                  upper query
                )
              )
            )
             (
              begin (
                let (
                  (
                    s (
                      upper subject
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        max_value 0
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            i_max 0
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                j_max 0
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
                                            i
                                          )
                                           (
                                            if (
                                              < i (
                                                _len score
                                              )
                                            )
                                             (
                                              begin (
                                                begin (
                                                  call/cc (
                                                    lambda (
                                                      break15
                                                    )
                                                     (
                                                      letrec (
                                                        (
                                                          loop14 (
                                                            lambda (
                                                              j
                                                            )
                                                             (
                                                              if (
                                                                < j (
                                                                  _len (
                                                                    list-ref score i
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  begin (
                                                                    if (
                                                                      > (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref score i
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref score i
                                                                            )
                                                                             j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref score i
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref score i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref score i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                      )
                                                                       max_value
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! max_value (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref score i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref score i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref score i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref score i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref score i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        set! i_max i
                                                                      )
                                                                       (
                                                                        set! j_max j
                                                                      )
                                                                    )
                                                                     (
                                                                      quote (
                                                                        
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  loop14 (
                                                                    + j 1
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
                                                       (
                                                        loop14 0
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                loop12 (
                                                  + i 1
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
                                     (
                                      loop12 0
                                    )
                                  )
                                )
                              )
                               (
                                let (
                                  (
                                    i i_max
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        j j_max
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            align1 ""
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                align2 ""
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    gap_penalty (
                                                      score_function "-" "-" match_score mismatch_score gap_score
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      or (
                                                        equal? i 0
                                                      )
                                                       (
                                                        equal? j 0
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        ret11 ""
                                                      )
                                                    )
                                                     (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                  )
                                                   (
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
                                                                  and (
                                                                    > i 0
                                                                  )
                                                                   (
                                                                    > j 0
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        qc (
                                                                          _substring q (
                                                                            - i 1
                                                                          )
                                                                           i
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            sc (
                                                                              _substring s (
                                                                                - j 1
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            if (
                                                                              equal? (
                                                                                cond (
                                                                                  (
                                                                                    string? (
                                                                                      list-ref score i
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref score i
                                                                                    )
                                                                                     j (
                                                                                      + j 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref score i
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref score i
                                                                                    )
                                                                                     j
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref (
                                                                                      list-ref score i
                                                                                    )
                                                                                     j
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                _add (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref score (
                                                                                          - i 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref score (
                                                                                          - i 1
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
                                                                                        list-ref score (
                                                                                          - i 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref score (
                                                                                          - i 1
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
                                                                                        list-ref score (
                                                                                          - i 1
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        - j 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  score_function qc sc match_score mismatch_score gap_score
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! align1 (
                                                                                  string-append qc align1
                                                                                )
                                                                              )
                                                                               (
                                                                                set! align2 (
                                                                                  string-append sc align2
                                                                                )
                                                                              )
                                                                               (
                                                                                set! i (
                                                                                  - i 1
                                                                                )
                                                                              )
                                                                               (
                                                                                set! j (
                                                                                  - j 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              if (
                                                                                equal? (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref score i
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref score i
                                                                                      )
                                                                                       j (
                                                                                        + j 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref score i
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref score i
                                                                                      )
                                                                                       j
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref score i
                                                                                      )
                                                                                       j
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _add (
                                                                                    cond (
                                                                                      (
                                                                                        string? (
                                                                                          list-ref score (
                                                                                            - i 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        _substring (
                                                                                          list-ref score (
                                                                                            - i 1
                                                                                          )
                                                                                        )
                                                                                         j (
                                                                                          + j 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? (
                                                                                          list-ref score (
                                                                                            - i 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref (
                                                                                          list-ref score (
                                                                                            - i 1
                                                                                          )
                                                                                        )
                                                                                         j
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        list-ref (
                                                                                          list-ref score (
                                                                                            - i 1
                                                                                          )
                                                                                        )
                                                                                         j
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   gap_penalty
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  set! align1 (
                                                                                    string-append qc align1
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  set! align2 (
                                                                                    string-append "-" align2
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  set! i (
                                                                                    - i 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  set! align1 (
                                                                                    string-append "-" align1
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  set! align2 (
                                                                                    string-append sc align2
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  set! j (
                                                                                    - j 1
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
                                                    ret11 (
                                                      string-append (
                                                        string-append align1 "\n"
                                                      )
                                                       align2
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
          query "HEAGAWGHEE"
        )
      )
       (
        begin (
          let (
            (
              subject "PAWHEAE"
            )
          )
           (
            begin (
              let (
                (
                  score (
                    smith_waterman query subject 1 (
                      - 1
                    )
                     (
                      - 2
                    )
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        traceback score query subject 1 (
                          - 1
                        )
                         (
                          - 2
                        )
                      )
                    )
                     (
                      traceback score query subject 1 (
                        - 1
                      )
                       (
                        - 2
                      )
                    )
                     (
                      to-str (
                        traceback score query subject 1 (
                          - 1
                        )
                         (
                          - 2
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
