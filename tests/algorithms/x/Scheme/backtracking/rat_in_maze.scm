;; Generated on 2025-08-06 18:11 +0700
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
        run_maze maze i j dr dc sol
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                size (
                  _len maze
                )
              )
            )
             (
              begin (
                if (
                  and (
                    and (
                      equal? i dr
                    )
                     (
                      equal? j dc
                    )
                  )
                   (
                    equal? (
                      cond (
                        (
                          string? (
                            list-ref maze i
                          )
                        )
                         (
                          _substring (
                            list-ref maze i
                          )
                           j (
                            + j 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref maze i
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref maze i
                          )
                           j
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref maze i
                          )
                           j
                        )
                      )
                    )
                     0
                  )
                )
                 (
                  begin (
                    list-set! (
                      list-ref sol i
                    )
                     j 0
                  )
                   (
                    ret1 #t
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
                    lower_flag (
                      and (
                        >= i 0
                      )
                       (
                        >= j 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        upper_flag (
                          and (
                            < i size
                          )
                           (
                            < j size
                          )
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          and lower_flag upper_flag
                        )
                         (
                          begin (
                            let (
                              (
                                block_flag (
                                  and (
                                    equal? (
                                      cond (
                                        (
                                          string? (
                                            list-ref sol i
                                          )
                                        )
                                         (
                                          _substring (
                                            list-ref sol i
                                          )
                                           j (
                                            + j 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? (
                                            list-ref sol i
                                          )
                                        )
                                         (
                                          hash-table-ref (
                                            list-ref sol i
                                          )
                                           j
                                        )
                                      )
                                       (
                                        else (
                                          list-ref (
                                            list-ref sol i
                                          )
                                           j
                                        )
                                      )
                                    )
                                     1
                                  )
                                   (
                                    equal? (
                                      cond (
                                        (
                                          string? (
                                            list-ref maze i
                                          )
                                        )
                                         (
                                          _substring (
                                            list-ref maze i
                                          )
                                           j (
                                            + j 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? (
                                            list-ref maze i
                                          )
                                        )
                                         (
                                          hash-table-ref (
                                            list-ref maze i
                                          )
                                           j
                                        )
                                      )
                                       (
                                        else (
                                          list-ref (
                                            list-ref maze i
                                          )
                                           j
                                        )
                                      )
                                    )
                                     0
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                if block_flag (
                                  begin (
                                    list-set! (
                                      list-ref sol i
                                    )
                                     j 0
                                  )
                                   (
                                    if (
                                      or (
                                        or (
                                          or (
                                            run_maze maze (
                                              + i 1
                                            )
                                             j dr dc sol
                                          )
                                           (
                                            run_maze maze i (
                                              + j 1
                                            )
                                             dr dc sol
                                          )
                                        )
                                         (
                                          run_maze maze (
                                            - i 1
                                          )
                                           j dr dc sol
                                        )
                                      )
                                       (
                                        run_maze maze i (
                                          - j 1
                                        )
                                         dr dc sol
                                      )
                                    )
                                     (
                                      begin (
                                        ret1 #t
                                      )
                                    )
                                     (
                                      quote (
                                        
                                      )
                                    )
                                  )
                                   (
                                    list-set! (
                                      list-ref sol i
                                    )
                                     j 1
                                  )
                                   (
                                    ret1 #f
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
                          quote (
                            
                          )
                        )
                      )
                       (
                        ret1 #f
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
        solve_maze maze sr sc dr dc
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                size (
                  _len maze
                )
              )
            )
             (
              begin (
                if (
                  not (
                    and (
                      and (
                        and (
                          and (
                            and (
                              and (
                                and (
                                  <= 0 sr
                                )
                                 (
                                  < sr size
                                )
                              )
                               (
                                <= 0 sc
                              )
                            )
                             (
                              < sc size
                            )
                          )
                           (
                            <= 0 dr
                          )
                        )
                         (
                          < dr size
                        )
                      )
                       (
                        <= 0 dc
                      )
                    )
                     (
                      < dc size
                    )
                  )
                )
                 (
                  begin (
                    panic "Invalid source or destination coordinates"
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
                    sol (
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
                                      < i size
                                    )
                                     (
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
                                            let (
                                              (
                                                j 0
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
                                                              < j size
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
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
                                                set! sol (
                                                  append sol (
                                                    _list row
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
                        let (
                          (
                            solved (
                              run_maze maze sr sc dr dc sol
                            )
                          )
                        )
                         (
                          begin (
                            if solved (
                              begin (
                                ret2 sol
                              )
                            )
                             (
                              begin (
                                panic "No solution exists!"
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
          maze (
            _list (
              _list 0 1 0 1 1
            )
             (
              _list 0 0 0 0 0
            )
             (
              _list 1 0 1 0 1
            )
             (
              _list 0 0 1 0 0
            )
             (
              _list 1 0 0 1 0
            )
          )
        )
      )
       (
        begin (
          let (
            (
              n (
                - (
                  _len maze
                )
                 1
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    to-str-space (
                      solve_maze maze 0 0 n n
                    )
                  )
                )
                 (
                  to-str-space (
                    solve_maze maze 0 0 n n
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      solve_maze maze 0 0 n n
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
