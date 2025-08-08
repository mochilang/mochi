;; Generated on 2025-08-08 15:49 +0700
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
        ((number? x)
         (if (integer? x)
             (number->string (inexact->exact x))
             (number->string x)))
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
        index_of xs x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
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
                              < i (
                                _len xs
                              )
                            )
                             (
                              begin (
                                if (
                                  equal? (
                                    list-ref-safe xs i
                                  )
                                   x
                                )
                                 (
                                  begin (
                                    ret1 i
                                  )
                                )
                                 '(
                                  
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
                             '(
                              
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
                ret1 (
                  - 0 1
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        majority_vote votes votes_needed_to_win
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                < votes_needed_to_win 2
              )
               (
                begin (
                  ret4 (
                    _list
                  )
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  candidates (
                    _list
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      counts (
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
                                          _len votes
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              v (
                                                list-ref-safe votes i
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  idx (
                                                    index_of candidates v
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  if (
                                                    not (
                                                      equal? idx (
                                                        - 0 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      list-set! counts idx (
                                                        + (
                                                          list-ref-safe counts idx
                                                        )
                                                         1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    if (
                                                      < (
                                                        _len candidates
                                                      )
                                                       (
                                                        - votes_needed_to_win 1
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! candidates (
                                                          append candidates (
                                                            _list v
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! counts (
                                                          append counts (
                                                            _list 1
                                                          )
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
                                                                          < j (
                                                                            _len counts
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            list-set! counts j (
                                                                              - (
                                                                                list-ref-safe counts j
                                                                              )
                                                                               1
                                                                            )
                                                                          )
                                                                           (
                                                                            set! j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                           (
                                                                            loop7
                                                                          )
                                                                        )
                                                                         '(
                                                                          
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
                                                                new_candidates (
                                                                  _list
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    new_counts (
                                                                      _list
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! j 0
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
                                                                                  < j (
                                                                                    _len candidates
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    if (
                                                                                      > (
                                                                                        list-ref-safe counts j
                                                                                      )
                                                                                       0
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! new_candidates (
                                                                                          append new_candidates (
                                                                                            _list (
                                                                                              list-ref-safe candidates j
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        set! new_counts (
                                                                                          append new_counts (
                                                                                            _list (
                                                                                              list-ref-safe counts j
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     '(
                                                                                      
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! j (
                                                                                      + j 1
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    loop9
                                                                                  )
                                                                                )
                                                                                 '(
                                                                                  
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
                                                                    set! candidates new_candidates
                                                                  )
                                                                   (
                                                                    set! counts new_counts
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
                                       '(
                                        
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
                          let (
                            (
                              final_counts (
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
                                                < j (
                                                  _len candidates
                                                )
                                              )
                                               (
                                                begin (
                                                  set! final_counts (
                                                    append final_counts (
                                                      _list 0
                                                    )
                                                  )
                                                )
                                                 (
                                                  set! j (
                                                    + j 1
                                                  )
                                                )
                                                 (
                                                  loop11
                                                )
                                              )
                                               '(
                                                
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
                                  set! i 0
                                )
                                 (
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
                                                < i (
                                                  _len votes
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      v (
                                                        list-ref-safe votes i
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          idx (
                                                            index_of candidates v
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          if (
                                                            not (
                                                              equal? idx (
                                                                - 0 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              list-set! final_counts idx (
                                                                + (
                                                                  list-ref-safe final_counts idx
                                                                )
                                                                 1
                                                              )
                                                            )
                                                          )
                                                           '(
                                                            
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
                                                  loop13
                                                )
                                              )
                                               '(
                                                
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
                                  let (
                                    (
                                      result (
                                        _list
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      set! j 0
                                    )
                                     (
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
                                                    < j (
                                                      _len candidates
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      if (
                                                        > (
                                                          * (
                                                            list-ref-safe final_counts j
                                                          )
                                                           votes_needed_to_win
                                                        )
                                                         (
                                                          _len votes
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          set! result (
                                                            append result (
                                                              _list (
                                                                list-ref-safe candidates j
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       '(
                                                        
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
                                                   '(
                                                    
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
                                      ret4 result
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
        main
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            let (
              (
                votes (
                  _list 1 2 2 3 1 3 2
                )
              )
            )
             (
              begin (
                _display (
                  if (
                    string? (
                      to-str-space (
                        majority_vote votes 3
                      )
                    )
                  )
                   (
                    to-str-space (
                      majority_vote votes 3
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        majority_vote votes 3
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
                        majority_vote votes 2
                      )
                    )
                  )
                   (
                    to-str-space (
                      majority_vote votes 2
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        majority_vote votes 2
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
                        majority_vote votes 4
                      )
                    )
                  )
                   (
                    to-str-space (
                      majority_vote votes 4
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        majority_vote votes 4
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
     (
      main
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
