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
      start23 (
        current-jiffy
      )
    )
     (
      jps26 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        empty_list
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
                  cons "data" (
                    _list
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
        length list
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            ret2 (
              _len (
                hash-table-ref list "data"
              )
            )
          )
        )
      )
    )
     (
      define (
        is_empty list
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            ret3 (
              equal? (
                _len (
                  hash-table-ref list "data"
                )
              )
               0
            )
          )
        )
      )
    )
     (
      define (
        to_string list
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                equal? (
                  _len (
                    hash-table-ref list "data"
                  )
                )
                 0
              )
               (
                begin (
                  ret4 ""
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
                  s (
                    to-str-space (
                      list-ref (
                        hash-table-ref list "data"
                      )
                       0
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
                                      _len (
                                        hash-table-ref list "data"
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      set! s (
                                        string-append (
                                          string-append s "->"
                                        )
                                         (
                                          to-str-space (
                                            list-ref (
                                              hash-table-ref list "data"
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
                      ret4 s
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
        insert_nth list index value
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              if (
                or (
                  < index 0
                )
                 (
                  > index (
                    _len (
                      hash-table-ref list "data"
                    )
                  )
                )
              )
               (
                begin (
                  panic "index out of range"
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
                  res (
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
                                    < i index
                                  )
                                   (
                                    begin (
                                      set! res (
                                        append res (
                                          _list (
                                            list-ref (
                                              hash-table-ref list "data"
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
                      set! res (
                        append res (
                          _list value
                        )
                      )
                    )
                     (
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
                                    < i (
                                      _len (
                                        hash-table-ref list "data"
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      set! res (
                                        append res (
                                          _list (
                                            list-ref (
                                              hash-table-ref list "data"
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
                        alist->hash-table (
                          _list (
                            cons "data" res
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
        insert_head list value
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            ret12 (
              insert_nth list 0 value
            )
          )
        )
      )
    )
     (
      define (
        insert_tail list value
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            ret13 (
              insert_nth list (
                _len (
                  hash-table-ref list "data"
                )
              )
               value
            )
          )
        )
      )
    )
     (
      define (
        delete_nth list index
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            begin (
              if (
                or (
                  < index 0
                )
                 (
                  >= index (
                    _len (
                      hash-table-ref list "data"
                    )
                  )
                )
              )
               (
                begin (
                  panic "index out of range"
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
                  res (
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
                      let (
                        (
                          removed 0
                        )
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
                                      
                                    )
                                     (
                                      if (
                                        < i (
                                          _len (
                                            hash-table-ref list "data"
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            equal? i index
                                          )
                                           (
                                            begin (
                                              set! removed (
                                                list-ref (
                                                  hash-table-ref list "data"
                                                )
                                                 i
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! res (
                                                append res (
                                                  _list (
                                                    list-ref (
                                                      hash-table-ref list "data"
                                                    )
                                                     i
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
                          ret14 (
                            alist->hash-table (
                              _list (
                                cons "list" (
                                  alist->hash-table (
                                    _list (
                                      cons "data" res
                                    )
                                  )
                                )
                              )
                               (
                                cons "value" removed
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
        delete_head list
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            ret17 (
              delete_nth list 0
            )
          )
        )
      )
    )
     (
      define (
        delete_tail list
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            ret18 (
              delete_nth list (
                - (
                  _len (
                    hash-table-ref list "data"
                  )
                )
                 1
              )
            )
          )
        )
      )
    )
     (
      define (
        delete_value list value
      )
       (
        call/cc (
          lambda (
            ret19
          )
           (
            let (
              (
                idx 0
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
                    call/cc (
                      lambda (
                        break21
                      )
                       (
                        letrec (
                          (
                            loop20 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < idx (
                                    _len (
                                      hash-table-ref list "data"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? (
                                        list-ref (
                                          hash-table-ref list "data"
                                        )
                                         idx
                                      )
                                       value
                                    )
                                     (
                                      begin (
                                        set! found #t
                                      )
                                       (
                                        break21 (
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
                                   (
                                    set! idx (
                                      + idx 1
                                    )
                                  )
                                   (
                                    loop20
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
                          loop20
                        )
                      )
                    )
                  )
                   (
                    if (
                      not found
                    )
                     (
                      begin (
                        panic "value not found"
                      )
                    )
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
                    ret19 (
                      delete_nth list idx
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
            ret22
          )
           (
            let (
              (
                dll (
                  empty_list
                )
              )
            )
             (
              begin (
                set! dll (
                  insert_tail dll 1
                )
              )
               (
                set! dll (
                  insert_tail dll 2
                )
              )
               (
                set! dll (
                  insert_tail dll 3
                )
              )
               (
                _display (
                  if (
                    string? (
                      to_string dll
                    )
                  )
                   (
                    to_string dll
                  )
                   (
                    to-str (
                      to_string dll
                    )
                  )
                )
              )
               (
                newline
              )
               (
                set! dll (
                  insert_head dll 0
                )
              )
               (
                _display (
                  if (
                    string? (
                      to_string dll
                    )
                  )
                   (
                    to_string dll
                  )
                   (
                    to-str (
                      to_string dll
                    )
                  )
                )
              )
               (
                newline
              )
               (
                set! dll (
                  insert_nth dll 2 9
                )
              )
               (
                _display (
                  if (
                    string? (
                      to_string dll
                    )
                  )
                   (
                    to_string dll
                  )
                   (
                    to-str (
                      to_string dll
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
                      delete_nth dll 2
                    )
                  )
                )
                 (
                  begin (
                    set! dll (
                      hash-table-ref res "list"
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
                          to_string dll
                        )
                      )
                       (
                        to_string dll
                      )
                       (
                        to-str (
                          to_string dll
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      delete_tail dll
                    )
                  )
                   (
                    set! dll (
                      hash-table-ref res "list"
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
                          to_string dll
                        )
                      )
                       (
                        to_string dll
                      )
                       (
                        to-str (
                          to_string dll
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      delete_value dll 1
                    )
                  )
                   (
                    set! dll (
                      hash-table-ref res "list"
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
                          to_string dll
                        )
                      )
                       (
                        to_string dll
                      )
                       (
                        to-str (
                          to_string dll
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
          end24 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur25 (
              quotient (
                * (
                  - end24 start23
                )
                 1000000
              )
               jps26
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur25
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
