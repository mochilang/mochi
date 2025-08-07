;; Generated on 2025-08-07 11:54 +0700
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
(define (_div a b) (/ a b))
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
      start8 (
        current-jiffy
      )
    )
     (
      jps11 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        depth_first_search grid row col visit
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                row_length (
                  _len grid
                )
              )
            )
             (
              begin (
                let (
                  (
                    col_length (
                      _len (
                        list-ref grid 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    if (
                      or (
                        or (
                          or (
                            < row 0
                          )
                           (
                            < col 0
                          )
                        )
                         (
                          equal? row row_length
                        )
                      )
                       (
                        equal? col col_length
                      )
                    )
                     (
                      begin (
                        ret1 0
                      )
                    )
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
                    if (
                      cond (
                        (
                          string? (
                            list-ref visit row
                          )
                        )
                         (
                          _substring (
                            list-ref visit row
                          )
                           col (
                            + col 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref visit row
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref visit row
                          )
                           col
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref visit row
                          )
                           col
                        )
                      )
                    )
                     (
                      begin (
                        ret1 0
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
                        cond (
                          (
                            string? (
                              list-ref grid row
                            )
                          )
                           (
                            _substring (
                              list-ref grid row
                            )
                             col (
                              + col 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? (
                              list-ref grid row
                            )
                          )
                           (
                            hash-table-ref (
                              list-ref grid row
                            )
                             col
                          )
                        )
                         (
                          else (
                            list-ref (
                              list-ref grid row
                            )
                             col
                          )
                        )
                      )
                       1
                    )
                     (
                      begin (
                        ret1 0
                      )
                    )
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
                    if (
                      and (
                        equal? row (
                          - row_length 1
                        )
                      )
                       (
                        equal? col (
                          - col_length 1
                        )
                      )
                    )
                     (
                      begin (
                        ret1 1
                      )
                    )
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
                    list-set! (
                      list-ref visit row
                    )
                     col #t
                  )
                   (
                    let (
                      (
                        count 0
                      )
                    )
                     (
                      begin (
                        set! count (
                          _add count (
                            depth_first_search grid (
                              + row 1
                            )
                             col visit
                          )
                        )
                      )
                       (
                        set! count (
                          _add count (
                            depth_first_search grid (
                              - row 1
                            )
                             col visit
                          )
                        )
                      )
                       (
                        set! count (
                          _add count (
                            depth_first_search grid row (
                              + col 1
                            )
                             visit
                          )
                        )
                      )
                       (
                        set! count (
                          _add count (
                            depth_first_search grid row (
                              - col 1
                            )
                             visit
                          )
                        )
                      )
                       (
                        list-set! (
                          list-ref visit row
                        )
                         col #f
                      )
                       (
                        ret1 count
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
        count_paths grid
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                rows (
                  _len grid
                )
              )
            )
             (
              begin (
                let (
                  (
                    cols (
                      _len (
                        list-ref grid 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        visit (
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
                                          < i rows
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                row_visit (
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
                                                                  < j cols
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! row_visit (
                                                                      append row_visit (
                                                                        _list #f
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
                                                    set! visit (
                                                      append visit (
                                                        _list row_visit
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
                            ret2 (
                              depth_first_search grid 0 0 visit
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
            ret7
          )
           (
            let (
              (
                grid1 (
                  _list (
                    _list 0 0 0 0
                  )
                   (
                    _list 1 1 0 0
                  )
                   (
                    _list 0 0 0 1
                  )
                   (
                    _list 0 1 0 0
                  )
                )
              )
            )
             (
              begin (
                _display (
                  if (
                    string? (
                      to-str-space (
                        count_paths grid1
                      )
                    )
                  )
                   (
                    to-str-space (
                      count_paths grid1
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        count_paths grid1
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
                    grid2 (
                      _list (
                        _list 0 0 0 0 0
                      )
                       (
                        _list 0 1 1 1 0
                      )
                       (
                        _list 0 1 1 1 0
                      )
                       (
                        _list 0 0 0 0 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    _display (
                      if (
                        string? (
                          to-str-space (
                            count_paths grid2
                          )
                        )
                      )
                       (
                        to-str-space (
                          count_paths grid2
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            count_paths grid2
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
    )
     (
      main
    )
     (
      let (
        (
          end9 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur10 (
              quotient (
                * (
                  - end9 start8
                )
                 1000000
              )
               jps11
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur10
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
