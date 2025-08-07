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
      start32 (
        current-jiffy
      )
    )
     (
      jps35 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          DIRECTIONS (
            _list (
              _list (
                - 1
              )
               0
            )
             (
              _list 0 (
                - 1
              )
            )
             (
              _list 1 0
            )
             (
              _list 0 1
            )
          )
        )
      )
       (
        begin (
          define (
            iabs x
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                begin (
                  if (
                    < x 0
                  )
                   (
                    begin (
                      ret1 (
                        - x
                      )
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  ret1 x
                )
              )
            )
          )
        )
         (
          define (
            search grid init goal cost heuristic
          )
           (
            call/cc (
              lambda (
                ret2
              )
               (
                let (
                  (
                    closed (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        r 0
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
                                      < r (
                                        _len grid
                                      )
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
                                                c 0
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
                                                              < c (
                                                                _len (
                                                                  list-ref grid 0
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list 0
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! c (
                                                                  + c 1
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
                                                set! closed (
                                                  append closed (
                                                    _list row
                                                  )
                                                )
                                              )
                                               (
                                                set! r (
                                                  + r 1
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
                        list-set! (
                          list-ref closed (
                            list-ref init 0
                          )
                        )
                         (
                          list-ref init 1
                        )
                         1
                      )
                       (
                        let (
                          (
                            action (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            set! r 0
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
                                        
                                      )
                                       (
                                        if (
                                          < r (
                                            _len grid
                                          )
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
                                                    c 0
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
                                                                  < c (
                                                                    _len (
                                                                      list-ref grid 0
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! row (
                                                                      append row (
                                                                        _list 0
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! c (
                                                                      + c 1
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
                                                    set! action (
                                                      append action (
                                                        _list row
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! r (
                                                      + r 1
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop7
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
                                  loop7
                                )
                              )
                            )
                          )
                           (
                            let (
                              (
                                x (
                                  list-ref init 0
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    y (
                                      list-ref init 1
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        g 0
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            f (
                                              + g (
                                                cond (
                                                  (
                                                    string? (
                                                      list-ref heuristic x
                                                    )
                                                  )
                                                   (
                                                    _substring (
                                                      list-ref heuristic x
                                                    )
                                                     y (
                                                      + y 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? (
                                                      list-ref heuristic x
                                                    )
                                                  )
                                                   (
                                                    hash-table-ref (
                                                      list-ref heuristic x
                                                    )
                                                     y
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref (
                                                      list-ref heuristic x
                                                    )
                                                     y
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                cell (
                                                  _list (
                                                    _list f g x y
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    found #f
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        resign #f
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
                                                                      and (
                                                                        not found
                                                                      )
                                                                       (
                                                                        not resign
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          equal? (
                                                                            _len cell
                                                                          )
                                                                           0
                                                                        )
                                                                         (
                                                                          begin (
                                                                            panic "Algorithm is unable to find solution"
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                best_i 0
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    best_f (
                                                                                      cond (
                                                                                        (
                                                                                          string? (
                                                                                            list-ref cell 0
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          _substring (
                                                                                            list-ref cell 0
                                                                                          )
                                                                                           0 (
                                                                                            + 0 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? (
                                                                                            list-ref cell 0
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref (
                                                                                            list-ref cell 0
                                                                                          )
                                                                                           0
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref (
                                                                                            list-ref cell 0
                                                                                          )
                                                                                           0
                                                                                        )
                                                                                      )
                                                                                    )
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
                                                                                                        _len cell
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        if (
                                                                                                          < (
                                                                                                            cond (
                                                                                                              (
                                                                                                                string? (
                                                                                                                  list-ref cell i
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                _substring (
                                                                                                                  list-ref cell i
                                                                                                                )
                                                                                                                 0 (
                                                                                                                  + 0 1
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              (
                                                                                                                hash-table? (
                                                                                                                  list-ref cell i
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                hash-table-ref (
                                                                                                                  list-ref cell i
                                                                                                                )
                                                                                                                 0
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              else (
                                                                                                                list-ref (
                                                                                                                  list-ref cell i
                                                                                                                )
                                                                                                                 0
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           best_f
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            set! best_f (
                                                                                                              cond (
                                                                                                                (
                                                                                                                  string? (
                                                                                                                    list-ref cell i
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  _substring (
                                                                                                                    list-ref cell i
                                                                                                                  )
                                                                                                                   0 (
                                                                                                                    + 0 1
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                (
                                                                                                                  hash-table? (
                                                                                                                    list-ref cell i
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  hash-table-ref (
                                                                                                                    list-ref cell i
                                                                                                                  )
                                                                                                                   0
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                else (
                                                                                                                  list-ref (
                                                                                                                    list-ref cell i
                                                                                                                  )
                                                                                                                   0
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            set! best_i i
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
                                                                                        let (
                                                                                          (
                                                                                            next_cell (
                                                                                              list-ref cell best_i
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                new_cell (
                                                                                                  _list
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! i 0
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
                                                                                                              < i (
                                                                                                                _len cell
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                if (
                                                                                                                  not (
                                                                                                                    equal? i best_i
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    set! new_cell (
                                                                                                                      append new_cell (
                                                                                                                        _list (
                                                                                                                          list-ref cell i
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
                                                                                                set! cell new_cell
                                                                                              )
                                                                                               (
                                                                                                set! x (
                                                                                                  list-ref next_cell 2
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                set! y (
                                                                                                  list-ref next_cell 3
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                set! g (
                                                                                                  list-ref next_cell 1
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                if (
                                                                                                  and (
                                                                                                    equal? x (
                                                                                                      list-ref goal 0
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    equal? y (
                                                                                                      list-ref goal 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    set! found #t
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    let (
                                                                                                      (
                                                                                                        d 0
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
                                                                                                                    
                                                                                                                  )
                                                                                                                   (
                                                                                                                    if (
                                                                                                                      < d (
                                                                                                                        _len DIRECTIONS
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        let (
                                                                                                                          (
                                                                                                                            x2 (
                                                                                                                              + x (
                                                                                                                                cond (
                                                                                                                                  (
                                                                                                                                    string? (
                                                                                                                                      list-ref DIRECTIONS d
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    _substring (
                                                                                                                                      list-ref DIRECTIONS d
                                                                                                                                    )
                                                                                                                                     0 (
                                                                                                                                      + 0 1
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  (
                                                                                                                                    hash-table? (
                                                                                                                                      list-ref DIRECTIONS d
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    hash-table-ref (
                                                                                                                                      list-ref DIRECTIONS d
                                                                                                                                    )
                                                                                                                                     0
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  else (
                                                                                                                                    list-ref (
                                                                                                                                      list-ref DIRECTIONS d
                                                                                                                                    )
                                                                                                                                     0
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            let (
                                                                                                                              (
                                                                                                                                y2 (
                                                                                                                                  + y (
                                                                                                                                    cond (
                                                                                                                                      (
                                                                                                                                        string? (
                                                                                                                                          list-ref DIRECTIONS d
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        _substring (
                                                                                                                                          list-ref DIRECTIONS d
                                                                                                                                        )
                                                                                                                                         1 (
                                                                                                                                          + 1 1
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      (
                                                                                                                                        hash-table? (
                                                                                                                                          list-ref DIRECTIONS d
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        hash-table-ref (
                                                                                                                                          list-ref DIRECTIONS d
                                                                                                                                        )
                                                                                                                                         1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      else (
                                                                                                                                        list-ref (
                                                                                                                                          list-ref DIRECTIONS d
                                                                                                                                        )
                                                                                                                                         1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                if (
                                                                                                                                  and (
                                                                                                                                    and (
                                                                                                                                      and (
                                                                                                                                        and (
                                                                                                                                          and (
                                                                                                                                            >= x2 0
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            < x2 (
                                                                                                                                              _len grid
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          >= y2 0
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        < y2 (
                                                                                                                                          _len (
                                                                                                                                            list-ref grid 0
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      equal? (
                                                                                                                                        cond (
                                                                                                                                          (
                                                                                                                                            string? (
                                                                                                                                              list-ref closed x2
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            _substring (
                                                                                                                                              list-ref closed x2
                                                                                                                                            )
                                                                                                                                             y2 (
                                                                                                                                              + y2 1
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          (
                                                                                                                                            hash-table? (
                                                                                                                                              list-ref closed x2
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            hash-table-ref (
                                                                                                                                              list-ref closed x2
                                                                                                                                            )
                                                                                                                                             y2
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          else (
                                                                                                                                            list-ref (
                                                                                                                                              list-ref closed x2
                                                                                                                                            )
                                                                                                                                             y2
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       0
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    equal? (
                                                                                                                                      cond (
                                                                                                                                        (
                                                                                                                                          string? (
                                                                                                                                            list-ref grid x2
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          _substring (
                                                                                                                                            list-ref grid x2
                                                                                                                                          )
                                                                                                                                           y2 (
                                                                                                                                            + y2 1
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        (
                                                                                                                                          hash-table? (
                                                                                                                                            list-ref grid x2
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          hash-table-ref (
                                                                                                                                            list-ref grid x2
                                                                                                                                          )
                                                                                                                                           y2
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        else (
                                                                                                                                          list-ref (
                                                                                                                                            list-ref grid x2
                                                                                                                                          )
                                                                                                                                           y2
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     0
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  begin (
                                                                                                                                    let (
                                                                                                                                      (
                                                                                                                                        g2 (
                                                                                                                                          + g cost
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      begin (
                                                                                                                                        let (
                                                                                                                                          (
                                                                                                                                            f2 (
                                                                                                                                              + g2 (
                                                                                                                                                cond (
                                                                                                                                                  (
                                                                                                                                                    string? (
                                                                                                                                                      list-ref heuristic x2
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                   (
                                                                                                                                                    _substring (
                                                                                                                                                      list-ref heuristic x2
                                                                                                                                                    )
                                                                                                                                                     y2 (
                                                                                                                                                      + y2 1
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                                 (
                                                                                                                                                  (
                                                                                                                                                    hash-table? (
                                                                                                                                                      list-ref heuristic x2
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                   (
                                                                                                                                                    hash-table-ref (
                                                                                                                                                      list-ref heuristic x2
                                                                                                                                                    )
                                                                                                                                                     y2
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                                 (
                                                                                                                                                  else (
                                                                                                                                                    list-ref (
                                                                                                                                                      list-ref heuristic x2
                                                                                                                                                    )
                                                                                                                                                     y2
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          begin (
                                                                                                                                            set! cell (
                                                                                                                                              append cell (
                                                                                                                                                _list (
                                                                                                                                                  _list f2 g2 x2 y2
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            list-set! (
                                                                                                                                              list-ref closed x2
                                                                                                                                            )
                                                                                                                                             y2 1
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            list-set! (
                                                                                                                                              list-ref action x2
                                                                                                                                            )
                                                                                                                                             y2 d
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
                                                                                                                                set! d (
                                                                                                                                  + d 1
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        loop17
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
                                                                                                              loop17
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
                                                                        loop11
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
                                                              loop11
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        let (
                                                          (
                                                            invpath (
                                                              _list
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! x (
                                                              list-ref goal 0
                                                            )
                                                          )
                                                           (
                                                            set! y (
                                                              list-ref goal 1
                                                            )
                                                          )
                                                           (
                                                            set! invpath (
                                                              append invpath (
                                                                _list (
                                                                  _list x y
                                                                )
                                                              )
                                                            )
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
                                                                        
                                                                      )
                                                                       (
                                                                        if (
                                                                          or (
                                                                            not (
                                                                              equal? x (
                                                                                list-ref init 0
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            not (
                                                                              equal? y (
                                                                                list-ref init 1
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                dir (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref action x
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref action x
                                                                                      )
                                                                                       y (
                                                                                        + y 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref action x
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref action x
                                                                                      )
                                                                                       y
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref action x
                                                                                      )
                                                                                       y
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    x2 (
                                                                                      - x (
                                                                                        cond (
                                                                                          (
                                                                                            string? (
                                                                                              list-ref DIRECTIONS dir
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            _substring (
                                                                                              list-ref DIRECTIONS dir
                                                                                            )
                                                                                             0 (
                                                                                              + 0 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? (
                                                                                              list-ref DIRECTIONS dir
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref (
                                                                                              list-ref DIRECTIONS dir
                                                                                            )
                                                                                             0
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref (
                                                                                              list-ref DIRECTIONS dir
                                                                                            )
                                                                                             0
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        y2 (
                                                                                          - y (
                                                                                            cond (
                                                                                              (
                                                                                                string? (
                                                                                                  list-ref DIRECTIONS dir
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _substring (
                                                                                                  list-ref DIRECTIONS dir
                                                                                                )
                                                                                                 1 (
                                                                                                  + 1 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? (
                                                                                                  list-ref DIRECTIONS dir
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref (
                                                                                                  list-ref DIRECTIONS dir
                                                                                                )
                                                                                                 1
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref (
                                                                                                  list-ref DIRECTIONS dir
                                                                                                )
                                                                                                 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! x x2
                                                                                      )
                                                                                       (
                                                                                        set! y y2
                                                                                      )
                                                                                       (
                                                                                        set! invpath (
                                                                                          append invpath (
                                                                                            _list (
                                                                                              _list x y
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
                                                                            loop19
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
                                                                  loop19
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            let (
                                                              (
                                                                path (
                                                                  _list
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    idx (
                                                                      - (
                                                                        _len invpath
                                                                      )
                                                                       1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    call/cc (
                                                                      lambda (
                                                                        break22
                                                                      )
                                                                       (
                                                                        letrec (
                                                                          (
                                                                            loop21 (
                                                                              lambda (
                                                                                
                                                                              )
                                                                               (
                                                                                if (
                                                                                  >= idx 0
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! path (
                                                                                      append path (
                                                                                        _list (
                                                                                          list-ref invpath idx
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! idx (
                                                                                      - idx 1
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    loop21
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
                                                                          loop21
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    ret2 (
                                                                      alist->hash-table (
                                                                        _list (
                                                                          cons "path" path
                                                                        )
                                                                         (
                                                                          cons "action" action
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
                ret23
              )
               (
                let (
                  (
                    grid (
                      _list (
                        _list 0 1 0 0 0 0
                      )
                       (
                        _list 0 1 0 0 0 0
                      )
                       (
                        _list 0 1 0 0 0 0
                      )
                       (
                        _list 0 1 0 0 1 0
                      )
                       (
                        _list 0 0 0 0 1 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        init (
                          _list 0 0
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            goal (
                              _list (
                                - (
                                  _len grid
                                )
                                 1
                              )
                               (
                                - (
                                  _len (
                                    list-ref grid 0
                                  )
                                )
                                 1
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                cost 1
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    heuristic (
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
                                            break25
                                          )
                                           (
                                            letrec (
                                              (
                                                loop24 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < i (
                                                        _len grid
                                                      )
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
                                                                    break27
                                                                  )
                                                                   (
                                                                    letrec (
                                                                      (
                                                                        loop26 (
                                                                          lambda (
                                                                            
                                                                          )
                                                                           (
                                                                            if (
                                                                              < j (
                                                                                _len (
                                                                                  list-ref grid 0
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    h (
                                                                                      _add (
                                                                                        iabs (
                                                                                          - i (
                                                                                            list-ref goal 0
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        iabs (
                                                                                          - j (
                                                                                            list-ref goal 1
                                                                                          )
                                                                                        )
                                                                                      )
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
                                                                                              list-ref grid i
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            _substring (
                                                                                              list-ref grid i
                                                                                            )
                                                                                             j (
                                                                                              + j 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? (
                                                                                              list-ref grid i
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref (
                                                                                              list-ref grid i
                                                                                            )
                                                                                             j
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref (
                                                                                              list-ref grid i
                                                                                            )
                                                                                             j
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       1
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! row (
                                                                                          append row (
                                                                                            _list 99
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! row (
                                                                                          append row (
                                                                                            _list h
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! j (
                                                                                      + j 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                loop26
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
                                                                      loop26
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! heuristic (
                                                                  append heuristic (
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
                                                        loop24
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
                                              loop24
                                            )
                                          )
                                        )
                                      )
                                       (
                                        let (
                                          (
                                            result (
                                              search grid init goal cost heuristic
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            _display (
                                              if (
                                                string? "ACTION MAP"
                                              )
                                               "ACTION MAP" (
                                                to-str "ACTION MAP"
                                              )
                                            )
                                          )
                                           (
                                            newline
                                          )
                                           (
                                            let (
                                              (
                                                rr 0
                                              )
                                            )
                                             (
                                              begin (
                                                call/cc (
                                                  lambda (
                                                    break29
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop28 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              < rr (
                                                                _len (
                                                                  hash-table-ref result "action"
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                _display (
                                                                  if (
                                                                    string? (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            hash-table-ref result "action"
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            hash-table-ref result "action"
                                                                          )
                                                                           rr (
                                                                            + rr 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            hash-table-ref result "action"
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            hash-table-ref result "action"
                                                                          )
                                                                           rr
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            hash-table-ref result "action"
                                                                          )
                                                                           rr
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          hash-table-ref result "action"
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          hash-table-ref result "action"
                                                                        )
                                                                         rr (
                                                                          + rr 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          hash-table-ref result "action"
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          hash-table-ref result "action"
                                                                        )
                                                                         rr
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          hash-table-ref result "action"
                                                                        )
                                                                         rr
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    to-str (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            hash-table-ref result "action"
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            hash-table-ref result "action"
                                                                          )
                                                                           rr (
                                                                            + rr 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            hash-table-ref result "action"
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            hash-table-ref result "action"
                                                                          )
                                                                           rr
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            hash-table-ref result "action"
                                                                          )
                                                                           rr
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                newline
                                                              )
                                                               (
                                                                set! rr (
                                                                  + rr 1
                                                                )
                                                              )
                                                               (
                                                                loop28
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
                                                      loop28
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                let (
                                                  (
                                                    p 0
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    call/cc (
                                                      lambda (
                                                        break31
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop30 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < p (
                                                                    _len (
                                                                      hash-table-ref result "path"
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    _display (
                                                                      if (
                                                                        string? (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                hash-table-ref result "path"
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                hash-table-ref result "path"
                                                                              )
                                                                               p (
                                                                                + p 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                hash-table-ref result "path"
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                hash-table-ref result "path"
                                                                              )
                                                                               p
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                hash-table-ref result "path"
                                                                              )
                                                                               p
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              hash-table-ref result "path"
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              hash-table-ref result "path"
                                                                            )
                                                                             p (
                                                                              + p 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              hash-table-ref result "path"
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              hash-table-ref result "path"
                                                                            )
                                                                             p
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              hash-table-ref result "path"
                                                                            )
                                                                             p
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        to-str (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                hash-table-ref result "path"
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                hash-table-ref result "path"
                                                                              )
                                                                               p (
                                                                                + p 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                hash-table-ref result "path"
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                hash-table-ref result "path"
                                                                              )
                                                                               p
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                hash-table-ref result "path"
                                                                              )
                                                                               p
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    newline
                                                                  )
                                                                   (
                                                                    set! p (
                                                                      + p 1
                                                                    )
                                                                  )
                                                                   (
                                                                    loop30
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
                                                          loop30
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
          main
        )
      )
    )
     (
      let (
        (
          end33 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur34 (
              quotient (
                * (
                  - end33 start32
                )
                 1000000
              )
               jps35
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur34
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
