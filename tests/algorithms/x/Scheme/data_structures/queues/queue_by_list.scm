;; Generated on 2025-08-06 23:57 +0700
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
      start16 (
        current-jiffy
      )
    )
     (
      jps19 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        new_queue items
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              alist->hash-table (
                _list (
                  cons "entries" items
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        len_queue q
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            ret2 (
              _len (
                hash-table-ref q "entries"
              )
            )
          )
        )
      )
    )
     (
      define (
        str_queue q
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            let (
              (
                s "Queue(("
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
                                  < i (
                                    _len (
                                      hash-table-ref q "entries"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! s (
                                      string-append s (
                                        to-str-space (
                                          list-ref (
                                            hash-table-ref q "entries"
                                          )
                                           i
                                        )
                                      )
                                    )
                                  )
                                   (
                                    if (
                                      < i (
                                        - (
                                          _len (
                                            hash-table-ref q "entries"
                                          )
                                        )
                                         1
                                      )
                                    )
                                     (
                                      begin (
                                        set! s (
                                          string-append s ", "
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
                    set! s (
                      string-append s "))"
                    )
                  )
                   (
                    ret3 s
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
        put q item
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                e (
                  hash-table-ref q "entries"
                )
              )
            )
             (
              begin (
                set! e (
                  append e (
                    _list item
                  )
                )
              )
               (
                ret6 (
                  alist->hash-table (
                    _list (
                      cons "entries" e
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
        get q
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
                  _len (
                    hash-table-ref q "entries"
                  )
                )
                 0
              )
               (
                begin (
                  panic "Queue is empty"
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
                  value (
                    list-ref (
                      hash-table-ref q "entries"
                    )
                     0
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      new_entries (
                        _list
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
                                          _len (
                                            hash-table-ref q "entries"
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          set! new_entries (
                                            append new_entries (
                                              _list (
                                                list-ref (
                                                  hash-table-ref q "entries"
                                                )
                                                 i
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
                            alist->hash-table (
                              _list (
                                cons "queue" (
                                  alist->hash-table (
                                    _list (
                                      cons "entries" new_entries
                                    )
                                  )
                                )
                              )
                               (
                                cons "value" value
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
        rotate q rotation
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                e (
                  hash-table-ref q "entries"
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
                                  < r rotation
                                )
                                 (
                                  begin (
                                    if (
                                      > (
                                        _len e
                                      )
                                       0
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            first (
                                              list-ref e 0
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                rest (
                                                  _list
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
                                                                    _len e
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! rest (
                                                                      append rest (
                                                                        _list (
                                                                          list-ref e i
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
                                                    set! rest (
                                                      append rest (
                                                        _list first
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! e rest
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
                                   (
                                    set! r (
                                      + r 1
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
                    ret10 (
                      alist->hash-table (
                        _list (
                          cons "entries" e
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
        get_front q
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            ret15 (
              list-ref (
                hash-table-ref q "entries"
              )
               0
            )
          )
        )
      )
    )
     (
      let (
        (
          q (
            new_queue (
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
                len_queue q
              )
            )
             (
              len_queue q
            )
             (
              to-str (
                len_queue q
              )
            )
          )
        )
         (
          newline
        )
         (
          set! q (
            put q 10
          )
        )
         (
          set! q (
            put q 20
          )
        )
         (
          set! q (
            put q 30
          )
        )
         (
          set! q (
            put q 40
          )
        )
         (
          _display (
            if (
              string? (
                str_queue q
              )
            )
             (
              str_queue q
            )
             (
              to-str (
                str_queue q
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
              res (
                get q
              )
            )
          )
           (
            begin (
              set! q (
                hash-table-ref res "queue"
              )
            )
             (
              _display (
                if (
                  string? (
                    hash-table-ref res "value"
                  )
                )
                 (
                  hash-table-ref res "value"
                )
                 (
                  to-str (
                    hash-table-ref res "value"
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
                    str_queue q
                  )
                )
                 (
                  str_queue q
                )
                 (
                  to-str (
                    str_queue q
                  )
                )
              )
            )
             (
              newline
            )
             (
              set! q (
                rotate q 2
              )
            )
             (
              _display (
                if (
                  string? (
                    str_queue q
                  )
                )
                 (
                  str_queue q
                )
                 (
                  to-str (
                    str_queue q
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
                  front (
                    get_front q
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? front
                    )
                     front (
                      to-str front
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
                        str_queue q
                      )
                    )
                     (
                      str_queue q
                    )
                     (
                      to-str (
                        str_queue q
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
          end17 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur18 (
              quotient (
                * (
                  - end17 start16
                )
                 1000000
              )
               jps19
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur18
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
