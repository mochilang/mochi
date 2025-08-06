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
        contains xs x
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
                                    list-ref xs i
                                  )
                                   x
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
                ret1 #f
              )
            )
          )
        )
      )
    )
     (
      define (
        repeat s times
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                result ""
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
                                  < i times
                                )
                                 (
                                  begin (
                                    set! result (
                                      string-append result s
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
                    ret4 result
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
        build_board pos n
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                board (
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
                                  < i (
                                    _len pos
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        col (
                                          list-ref pos i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            line (
                                              string-append (
                                                string-append (
                                                  repeat ". " col
                                                )
                                                 "Q "
                                              )
                                               (
                                                repeat ". " (
                                                  - (
                                                    - n 1
                                                  )
                                                   col
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! board (
                                              append board (
                                                _list line
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
                    ret7 board
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
        depth_first_search pos dr dl n
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                row (
                  _len pos
                )
              )
            )
             (
              begin (
                if (
                  equal? row n
                )
                 (
                  begin (
                    let (
                      (
                        single (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        set! single (
                          append single (
                            _list (
                              build_board pos n
                            )
                          )
                        )
                      )
                       (
                        ret10 single
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
                let (
                  (
                    boards (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        col 0
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
                                      < col n
                                    )
                                     (
                                      begin (
                                        if (
                                          or (
                                            or (
                                              contains pos col
                                            )
                                             (
                                              contains dr (
                                                - row col
                                              )
                                            )
                                          )
                                           (
                                            contains dl (
                                              + row col
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! col (
                                              + col 1
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
                                       (
                                        let (
                                          (
                                            result (
                                              depth_first_search (
                                                append pos (
                                                  _list col
                                                )
                                              )
                                               (
                                                append dr (
                                                  _list (
                                                    - row col
                                                  )
                                                )
                                              )
                                               (
                                                append dl (
                                                  _list (
                                                    + row col
                                                  )
                                                )
                                              )
                                               n
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! boards (
                                              append boards result
                                            )
                                          )
                                           (
                                            set! col (
                                              + col 1
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
                        ret10 boards
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
        n_queens_solution n
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            let (
              (
                boards (
                  depth_first_search (
                    _list
                  )
                   (
                    _list
                  )
                   (
                    _list
                  )
                   n
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
                        break15
                      )
                       (
                        letrec (
                          (
                            loop14 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len boards
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
                                                      < j (
                                                        _len (
                                                          cond (
                                                            (
                                                              string? boards
                                                            )
                                                             (
                                                              _substring boards i (
                                                                + i 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? boards
                                                            )
                                                             (
                                                              hash-table-ref boards i
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref boards i
                                                            )
                                                          )
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
                                                                    cond (
                                                                      (
                                                                        string? boards
                                                                      )
                                                                       (
                                                                        _substring boards i (
                                                                          + i 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? boards
                                                                      )
                                                                       (
                                                                        hash-table-ref boards i
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref boards i
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  _substring (
                                                                    cond (
                                                                      (
                                                                        string? boards
                                                                      )
                                                                       (
                                                                        _substring boards i (
                                                                          + i 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? boards
                                                                      )
                                                                       (
                                                                        hash-table-ref boards i
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref boards i
                                                                      )
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
                                                                    cond (
                                                                      (
                                                                        string? boards
                                                                      )
                                                                       (
                                                                        _substring boards i (
                                                                          + i 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? boards
                                                                      )
                                                                       (
                                                                        hash-table-ref boards i
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref boards i
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  hash-table-ref (
                                                                    cond (
                                                                      (
                                                                        string? boards
                                                                      )
                                                                       (
                                                                        _substring boards i (
                                                                          + i 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? boards
                                                                      )
                                                                       (
                                                                        hash-table-ref boards i
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref boards i
                                                                      )
                                                                    )
                                                                  )
                                                                   j
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref (
                                                                    cond (
                                                                      (
                                                                        string? boards
                                                                      )
                                                                       (
                                                                        _substring boards i (
                                                                          + i 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? boards
                                                                      )
                                                                       (
                                                                        hash-table-ref boards i
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref boards i
                                                                      )
                                                                    )
                                                                  )
                                                                   j
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            cond (
                                                              (
                                                                string? (
                                                                  cond (
                                                                    (
                                                                      string? boards
                                                                    )
                                                                     (
                                                                      _substring boards i (
                                                                        + i 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? boards
                                                                    )
                                                                     (
                                                                      hash-table-ref boards i
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref boards i
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                _substring (
                                                                  cond (
                                                                    (
                                                                      string? boards
                                                                    )
                                                                     (
                                                                      _substring boards i (
                                                                        + i 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? boards
                                                                    )
                                                                     (
                                                                      hash-table-ref boards i
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref boards i
                                                                    )
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
                                                                  cond (
                                                                    (
                                                                      string? boards
                                                                    )
                                                                     (
                                                                      _substring boards i (
                                                                        + i 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? boards
                                                                    )
                                                                     (
                                                                      hash-table-ref boards i
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref boards i
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                hash-table-ref (
                                                                  cond (
                                                                    (
                                                                      string? boards
                                                                    )
                                                                     (
                                                                      _substring boards i (
                                                                        + i 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? boards
                                                                    )
                                                                     (
                                                                      hash-table-ref boards i
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref boards i
                                                                    )
                                                                  )
                                                                )
                                                                 j
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref (
                                                                  cond (
                                                                    (
                                                                      string? boards
                                                                    )
                                                                     (
                                                                      _substring boards i (
                                                                        + i 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? boards
                                                                    )
                                                                     (
                                                                      hash-table-ref boards i
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref boards i
                                                                    )
                                                                  )
                                                                )
                                                                 j
                                                              )
                                                            )
                                                          )
                                                           (
                                                            to-str (
                                                              cond (
                                                                (
                                                                  string? (
                                                                    cond (
                                                                      (
                                                                        string? boards
                                                                      )
                                                                       (
                                                                        _substring boards i (
                                                                          + i 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? boards
                                                                      )
                                                                       (
                                                                        hash-table-ref boards i
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref boards i
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  _substring (
                                                                    cond (
                                                                      (
                                                                        string? boards
                                                                      )
                                                                       (
                                                                        _substring boards i (
                                                                          + i 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? boards
                                                                      )
                                                                       (
                                                                        hash-table-ref boards i
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref boards i
                                                                      )
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
                                                                    cond (
                                                                      (
                                                                        string? boards
                                                                      )
                                                                       (
                                                                        _substring boards i (
                                                                          + i 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? boards
                                                                      )
                                                                       (
                                                                        hash-table-ref boards i
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref boards i
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  hash-table-ref (
                                                                    cond (
                                                                      (
                                                                        string? boards
                                                                      )
                                                                       (
                                                                        _substring boards i (
                                                                          + i 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? boards
                                                                      )
                                                                       (
                                                                        hash-table-ref boards i
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref boards i
                                                                      )
                                                                    )
                                                                  )
                                                                   j
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref (
                                                                    cond (
                                                                      (
                                                                        string? boards
                                                                      )
                                                                       (
                                                                        _substring boards i (
                                                                          + i 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? boards
                                                                      )
                                                                       (
                                                                        hash-table-ref boards i
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref boards i
                                                                      )
                                                                    )
                                                                  )
                                                                   j
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
                                                        set! j (
                                                          + j 1
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
                                        _display (
                                          if (
                                            string? ""
                                          )
                                           "" (
                                            to-str ""
                                          )
                                        )
                                      )
                                       (
                                        newline
                                      )
                                       (
                                        set! i (
                                          + i 1
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop14
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
                          loop14
                        )
                      )
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          _len boards
                        )
                      )
                       (
                        _len boards
                      )
                       (
                        to-str (
                          _len boards
                        )
                      )
                    )
                  )
                   (
                    _display " "
                  )
                   (
                    _display (
                      if (
                        string? "solutions were found."
                      )
                       "solutions were found." (
                        to-str "solutions were found."
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    ret13 (
                      _len boards
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
      n_queens_solution 4
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
