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
      start10 (
        current-jiffy
      )
    )
     (
      jps13 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        make_list len value
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                arr (
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
                                  < i len
                                )
                                 (
                                  begin (
                                    set! arr (
                                      append arr (
                                        _list value
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
                    ret1 arr
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
        max_int a b
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            if (
              > a b
            )
             (
              begin (
                ret4 a
              )
            )
             (
              begin (
                ret4 b
              )
            )
          )
        )
      )
    )
     (
      define (
        min_int a b
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            if (
              < a b
            )
             (
              begin (
                ret5 a
              )
            )
             (
              begin (
                ret5 b
              )
            )
          )
        )
      )
    )
     (
      define (
        min3 a b c
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            ret6 (
              min_int (
                min_int a b
              )
               c
            )
          )
        )
      )
    )
     (
      define (
        minimum_tickets_cost days costs
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              if (
                equal? (
                  _len days
                )
                 0
              )
               (
                begin (
                  ret7 0
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
                  last_day (
                    list-ref days (
                      - (
                        _len days
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
                      dp (
                        make_list (
                          + last_day 1
                        )
                         0
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          day_index 0
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              d 1
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
                                            <= d last_day
                                          )
                                           (
                                            begin (
                                              if (
                                                and (
                                                  < day_index (
                                                    _len days
                                                  )
                                                )
                                                 (
                                                  equal? d (
                                                    list-ref days day_index
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      cost1 (
                                                        _add (
                                                          cond (
                                                            (
                                                              string? dp
                                                            )
                                                             (
                                                              _substring dp (
                                                                - d 1
                                                              )
                                                               (
                                                                + (
                                                                  - d 1
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
                                                                - d 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref dp (
                                                                - d 1
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          list-ref costs 0
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          cost7 (
                                                            _add (
                                                              cond (
                                                                (
                                                                  string? dp
                                                                )
                                                                 (
                                                                  _substring dp (
                                                                    max_int 0 (
                                                                      - d 7
                                                                    )
                                                                  )
                                                                   (
                                                                    + (
                                                                      max_int 0 (
                                                                        - d 7
                                                                      )
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
                                                                    max_int 0 (
                                                                      - d 7
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref dp (
                                                                    max_int 0 (
                                                                      - d 7
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              list-ref costs 1
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              cost30 (
                                                                _add (
                                                                  cond (
                                                                    (
                                                                      string? dp
                                                                    )
                                                                     (
                                                                      _substring dp (
                                                                        max_int 0 (
                                                                          - d 30
                                                                        )
                                                                      )
                                                                       (
                                                                        + (
                                                                          max_int 0 (
                                                                            - d 30
                                                                          )
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
                                                                        max_int 0 (
                                                                          - d 30
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref dp (
                                                                        max_int 0 (
                                                                          - d 30
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  list-ref costs 2
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              list-set! dp d (
                                                                min3 cost1 cost7 cost30
                                                              )
                                                            )
                                                             (
                                                              set! day_index (
                                                                + day_index 1
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
                                                begin (
                                                  list-set! dp d (
                                                    cond (
                                                      (
                                                        string? dp
                                                      )
                                                       (
                                                        _substring dp (
                                                          - d 1
                                                        )
                                                         (
                                                          + (
                                                            - d 1
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
                                                          - d 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        list-ref dp (
                                                          - d 1
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              set! d (
                                                + d 1
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
                              ret7 (
                                cond (
                                  (
                                    string? dp
                                  )
                                   (
                                    _substring dp last_day (
                                      + last_day 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? dp
                                  )
                                   (
                                    hash-table-ref dp last_day
                                  )
                                )
                                 (
                                  else (
                                    list-ref dp last_day
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
      _display (
        if (
          string? (
            to-str-space (
              minimum_tickets_cost (
                _list 1 4 6 7 8 20
              )
               (
                _list 2 7 15
              )
            )
          )
        )
         (
          to-str-space (
            minimum_tickets_cost (
              _list 1 4 6 7 8 20
            )
             (
              _list 2 7 15
            )
          )
        )
         (
          to-str (
            to-str-space (
              minimum_tickets_cost (
                _list 1 4 6 7 8 20
              )
               (
                _list 2 7 15
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
      _display (
        if (
          string? (
            to-str-space (
              minimum_tickets_cost (
                _list 1 2 3 4 5 6 7 8 9 10 30 31
              )
               (
                _list 2 7 15
              )
            )
          )
        )
         (
          to-str-space (
            minimum_tickets_cost (
              _list 1 2 3 4 5 6 7 8 9 10 30 31
            )
             (
              _list 2 7 15
            )
          )
        )
         (
          to-str (
            to-str-space (
              minimum_tickets_cost (
                _list 1 2 3 4 5 6 7 8 9 10 30 31
              )
               (
                _list 2 7 15
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
      _display (
        if (
          string? (
            to-str-space (
              minimum_tickets_cost (
                _list 1 2 3 4 5 6 7 8 9 10 30 31
              )
               (
                _list 2 90 150
              )
            )
          )
        )
         (
          to-str-space (
            minimum_tickets_cost (
              _list 1 2 3 4 5 6 7 8 9 10 30 31
            )
             (
              _list 2 90 150
            )
          )
        )
         (
          to-str (
            to-str-space (
              minimum_tickets_cost (
                _list 1 2 3 4 5 6 7 8 9 10 30 31
              )
               (
                _list 2 90 150
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
      let (
        (
          end11 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur12 (
              quotient (
                * (
                  - end11 start10
                )
                 1000000
              )
               jps13
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur12
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
