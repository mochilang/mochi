;; Generated on 2025-08-06 16:21 +0700
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
      start21 (
        current-jiffy
      )
    )
     (
      jps24 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        is_valid puzzle word row col vertical
      )
       (
        call/cc (
          lambda (
            ret1
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
                          i
                        )
                         (
                          if (
                            < i (
                              _len word
                            )
                          )
                           (
                            begin (
                              begin (
                                if vertical (
                                  begin (
                                    if (
                                      or (
                                        _ge (
                                          _add row i
                                        )
                                         (
                                          _len puzzle
                                        )
                                      )
                                       (
                                        not (
                                          string=? (
                                            cond (
                                              (
                                                string? (
                                                  list-ref puzzle (
                                                    _add row i
                                                  )
                                                )
                                              )
                                               (
                                                _substring (
                                                  list-ref puzzle (
                                                    _add row i
                                                  )
                                                )
                                                 col (
                                                  + col 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? (
                                                  list-ref puzzle (
                                                    _add row i
                                                  )
                                                )
                                              )
                                               (
                                                hash-table-ref (
                                                  list-ref puzzle (
                                                    _add row i
                                                  )
                                                )
                                                 col
                                              )
                                            )
                                             (
                                              else (
                                                list-ref (
                                                  list-ref puzzle (
                                                    _add row i
                                                  )
                                                )
                                                 col
                                              )
                                            )
                                          )
                                           ""
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        ret1 #f
                                      )
                                    )
                                     (
                                      quote (
                                        
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      or (
                                        _ge (
                                          _add col i
                                        )
                                         (
                                          _len (
                                            list-ref puzzle 0
                                          )
                                        )
                                      )
                                       (
                                        not (
                                          string=? (
                                            cond (
                                              (
                                                string? (
                                                  list-ref puzzle row
                                                )
                                              )
                                               (
                                                _substring (
                                                  list-ref puzzle row
                                                )
                                                 (
                                                  _add col i
                                                )
                                                 (
                                                  + (
                                                    _add col i
                                                  )
                                                   1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? (
                                                  list-ref puzzle row
                                                )
                                              )
                                               (
                                                hash-table-ref (
                                                  list-ref puzzle row
                                                )
                                                 (
                                                  _add col i
                                                )
                                              )
                                            )
                                             (
                                              else (
                                                list-ref (
                                                  list-ref puzzle row
                                                )
                                                 (
                                                  _add col i
                                                )
                                              )
                                            )
                                          )
                                           ""
                                        )
                                      )
                                    )
                                     (
                                      begin (
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
                              loop2 (
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
                    loop2 0
                  )
                )
              )
            )
             (
              ret1 #t
            )
          )
        )
      )
    )
     (
      define (
        place_word puzzle word row col vertical
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            call/cc (
              lambda (
                break6
              )
               (
                letrec (
                  (
                    loop5 (
                      lambda (
                        i
                      )
                       (
                        if (
                          < i (
                            _len word
                          )
                        )
                         (
                          begin (
                            begin (
                              let (
                                (
                                  ch (
                                    _substring word i (
                                      + i 1
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  if vertical (
                                    begin (
                                      list-set! (
                                        list-ref puzzle (
                                          _add row i
                                        )
                                      )
                                       col ch
                                    )
                                  )
                                   (
                                    begin (
                                      list-set! (
                                        list-ref puzzle row
                                      )
                                       (
                                        _add col i
                                      )
                                       ch
                                    )
                                  )
                                )
                              )
                            )
                          )
                           (
                            loop5 (
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
                  loop5 0
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        remove_word puzzle word row col vertical
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            call/cc (
              lambda (
                break9
              )
               (
                letrec (
                  (
                    loop8 (
                      lambda (
                        i
                      )
                       (
                        if (
                          < i (
                            _len word
                          )
                        )
                         (
                          begin (
                            begin (
                              if vertical (
                                begin (
                                  list-set! (
                                    list-ref puzzle (
                                      _add row i
                                    )
                                  )
                                   col ""
                                )
                              )
                               (
                                begin (
                                  list-set! (
                                    list-ref puzzle row
                                  )
                                   (
                                    _add col i
                                  )
                                   ""
                                )
                              )
                            )
                          )
                           (
                            loop8 (
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
                  loop8 0
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        solve_crossword puzzle words used
      )
       (
        call/cc (
          lambda (
            ret10
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
                          row
                        )
                         (
                          if (
                            < row (
                              _len puzzle
                            )
                          )
                           (
                            begin (
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
                                            col
                                          )
                                           (
                                            if (
                                              < col (
                                                _len (
                                                  list-ref puzzle 0
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                begin (
                                                  if (
                                                    string=? (
                                                      cond (
                                                        (
                                                          string? (
                                                            list-ref puzzle row
                                                          )
                                                        )
                                                         (
                                                          _substring (
                                                            list-ref puzzle row
                                                          )
                                                           col (
                                                            + col 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? (
                                                            list-ref puzzle row
                                                          )
                                                        )
                                                         (
                                                          hash-table-ref (
                                                            list-ref puzzle row
                                                          )
                                                           col
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref (
                                                            list-ref puzzle row
                                                          )
                                                           col
                                                        )
                                                      )
                                                    )
                                                     ""
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
                                                                  i
                                                                )
                                                                 (
                                                                  if (
                                                                    < i (
                                                                      _len words
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      begin (
                                                                        if (
                                                                          not (
                                                                            list-ref used i
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                word (
                                                                                  list-ref words i
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                call/cc (
                                                                                  lambda (
                                                                                    break18
                                                                                  )
                                                                                   (
                                                                                    letrec (
                                                                                      (
                                                                                        loop17 (
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
                                                                                                    vertical (
                                                                                                      car xs
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    if (
                                                                                                      is_valid puzzle word row col vertical
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        place_word puzzle word row col vertical
                                                                                                      )
                                                                                                       (
                                                                                                        list-set! used i #t
                                                                                                      )
                                                                                                       (
                                                                                                        if (
                                                                                                          solve_crossword puzzle words used
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            ret10 #t
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          quote (
                                                                                                            
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        list-set! used i #f
                                                                                                      )
                                                                                                       (
                                                                                                        remove_word puzzle word row col vertical
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      quote (
                                                                                                        
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                loop17 (
                                                                                                  cdr xs
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      loop17 (
                                                                                        _list #t #f
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
                                                                    )
                                                                     (
                                                                      loop15 (
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
                                                            loop15 0
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      ret10 #f
                                                    )
                                                  )
                                                   (
                                                    quote (
                                                      
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                loop13 (
                                                  + col 1
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
                                      loop13 0
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop11 (
                                + row 1
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
                    loop11 0
                  )
                )
              )
            )
             (
              ret10 #t
            )
          )
        )
      )
    )
     (
      let (
        (
          puzzle (
            _list (
              _list "" "" ""
            )
             (
              _list "" "" ""
            )
             (
              _list "" "" ""
            )
          )
        )
      )
       (
        begin (
          let (
            (
              words (
                _list "cat" "dog" "car"
              )
            )
          )
           (
            begin (
              let (
                (
                  used (
                    _list #f #f #f
                  )
                )
              )
               (
                begin (
                  if (
                    solve_crossword puzzle words used
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? "Solution found:"
                        )
                         "Solution found:" (
                          to-str "Solution found:"
                        )
                      )
                    )
                     (
                      newline
                    )
                     (
                      call/cc (
                        lambda (
                          break20
                        )
                         (
                          letrec (
                            (
                              loop19 (
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
                                          row (
                                            car xs
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          _display (
                                            if (
                                              string? row
                                            )
                                             row (
                                              to-str row
                                            )
                                          )
                                        )
                                         (
                                          newline
                                        )
                                      )
                                    )
                                     (
                                      loop19 (
                                        cdr xs
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                           (
                            loop19 puzzle
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? "No solution found:"
                        )
                         "No solution found:" (
                          to-str "No solution found:"
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
    )
     (
      let (
        (
          end22 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur23 (
              quotient (
                * (
                  - end22 start21
                )
                 1000000
              )
               jps24
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur23
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
