;; Generated on 2025-08-07 16:11 +0700
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
      start40 (
        current-jiffy
      )
    )
     (
      jps43 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        processes_resource_summation alloc
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                resources (
                  _len (
                    list-ref-safe alloc 0
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    sums (
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
                                      < i resources
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            total 0
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
                                                              < j (
                                                                _len alloc
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! total (
                                                                  + total (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref-safe alloc j
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref-safe alloc j
                                                                        )
                                                                         i (
                                                                          + i 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref-safe alloc j
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref-safe alloc j
                                                                        )
                                                                         i
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref-safe (
                                                                          list-ref-safe alloc j
                                                                        )
                                                                         i
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
                                                               (
                                                                loop4
                                                              )
                                                            )
                                                             '(
                                                              
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
                                                set! sums (
                                                  append sums (
                                                    _list total
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
                        ret1 sums
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
        available_resources claim alloc_sum
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                avail (
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
                                  < i (
                                    _len claim
                                  )
                                )
                                 (
                                  begin (
                                    set! avail (
                                      append avail (
                                        _list (
                                          - (
                                            list-ref-safe claim i
                                          )
                                           (
                                            list-ref-safe alloc_sum i
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
                    ret6 avail
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
        need max alloc
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                needs (
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
                                    _len max
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
                                                break13
                                              )
                                               (
                                                letrec (
                                                  (
                                                    loop12 (
                                                      lambda (
                                                        
                                                      )
                                                       (
                                                        if (
                                                          < j (
                                                            _len (
                                                              list-ref-safe max 0
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! row (
                                                              append row (
                                                                _list (
                                                                  - (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref-safe max i
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref-safe max i
                                                                        )
                                                                         j (
                                                                          + j 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref-safe max i
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref-safe max i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref-safe (
                                                                          list-ref-safe max i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref-safe alloc i
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref-safe alloc i
                                                                        )
                                                                         j (
                                                                          + j 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref-safe alloc i
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref-safe alloc i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref-safe (
                                                                          list-ref-safe alloc i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
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
                                                           (
                                                            loop12
                                                          )
                                                        )
                                                         '(
                                                          
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  loop12
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! needs (
                                              append needs (
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
                                    loop10
                                  )
                                )
                                 '(
                                  
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
                    ret9 needs
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
        pretty_print claim alloc max
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            begin (
              _display (
                if (
                  string? "         Allocated Resource Table"
                )
                 "         Allocated Resource Table" (
                  to-str "         Allocated Resource Table"
                )
              )
            )
             (
              newline
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
                                  _len alloc
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      row (
                                        list-ref-safe alloc i
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          line (
                                            string-append (
                                              string-append "P" (
                                                to-str-space (
                                                  + i 1
                                                )
                                              )
                                            )
                                             "       "
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
                                                            < j (
                                                              _len row
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              set! line (
                                                                string-append line (
                                                                  to-str-space (
                                                                    list-ref-safe row j
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              if (
                                                                < j (
                                                                  - (
                                                                    _len row
                                                                  )
                                                                   1
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  set! line (
                                                                    string-append line "        "
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
                                                              loop17
                                                            )
                                                          )
                                                           '(
                                                            
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
                                             (
                                              _display (
                                                if (
                                                  string? line
                                                )
                                                 line (
                                                  to-str line
                                                )
                                              )
                                            )
                                             (
                                              newline
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
                                      )
                                    )
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
                  _display (
                    if (
                      string? "         System Resource Table"
                    )
                     "         System Resource Table" (
                      to-str "         System Resource Table"
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  set! i 0
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
                                < i (
                                  _len max
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      row (
                                        list-ref-safe max i
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          line (
                                            string-append (
                                              string-append "P" (
                                                to-str-space (
                                                  + i 1
                                                )
                                              )
                                            )
                                             "       "
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
                                                            < j (
                                                              _len row
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              set! line (
                                                                string-append line (
                                                                  to-str-space (
                                                                    list-ref-safe row j
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              if (
                                                                < j (
                                                                  - (
                                                                    _len row
                                                                  )
                                                                   1
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  set! line (
                                                                    string-append line "        "
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
                                                              loop21
                                                            )
                                                          )
                                                           '(
                                                            
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
                                              _display (
                                                if (
                                                  string? line
                                                )
                                                 line (
                                                  to-str line
                                                )
                                              )
                                            )
                                             (
                                              newline
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
                                      )
                                    )
                                  )
                                )
                                 (
                                  loop19
                                )
                              )
                               '(
                                
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
                      usage ""
                    )
                  )
                   (
                    begin (
                      set! i 0
                    )
                     (
                      call/cc (
                        lambda (
                          break24
                        )
                         (
                          letrec (
                            (
                              loop23 (
                                lambda (
                                  
                                )
                                 (
                                  if (
                                    < i (
                                      _len claim
                                    )
                                  )
                                   (
                                    begin (
                                      if (
                                        > i 0
                                      )
                                       (
                                        begin (
                                          set! usage (
                                            string-append usage " "
                                          )
                                        )
                                      )
                                       '(
                                        
                                      )
                                    )
                                     (
                                      set! usage (
                                        string-append usage (
                                          to-str-space (
                                            list-ref-safe claim i
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
                                      loop23
                                    )
                                  )
                                   '(
                                    
                                  )
                                )
                              )
                            )
                          )
                           (
                            loop23
                          )
                        )
                      )
                    )
                     (
                      let (
                        (
                          alloc_sum (
                            processes_resource_summation alloc
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              avail (
                                available_resources claim alloc_sum
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  avail_str ""
                                )
                              )
                               (
                                begin (
                                  set! i 0
                                )
                                 (
                                  call/cc (
                                    lambda (
                                      break26
                                    )
                                     (
                                      letrec (
                                        (
                                          loop25 (
                                            lambda (
                                              
                                            )
                                             (
                                              if (
                                                < i (
                                                  _len avail
                                                )
                                              )
                                               (
                                                begin (
                                                  if (
                                                    > i 0
                                                  )
                                                   (
                                                    begin (
                                                      set! avail_str (
                                                        string-append avail_str " "
                                                      )
                                                    )
                                                  )
                                                   '(
                                                    
                                                  )
                                                )
                                                 (
                                                  set! avail_str (
                                                    string-append avail_str (
                                                      to-str-space (
                                                        cond (
                                                          (
                                                            string? avail
                                                          )
                                                           (
                                                            _substring avail i (
                                                              + i 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          (
                                                            hash-table? avail
                                                          )
                                                           (
                                                            hash-table-ref avail i
                                                          )
                                                        )
                                                         (
                                                          else (
                                                            list-ref-safe avail i
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
                                                  loop25
                                                )
                                              )
                                               '(
                                                
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop25
                                      )
                                    )
                                  )
                                )
                                 (
                                  _display (
                                    if (
                                      string? (
                                        string-append "Current Usage by Active Processes: " usage
                                      )
                                    )
                                     (
                                      string-append "Current Usage by Active Processes: " usage
                                    )
                                     (
                                      to-str (
                                        string-append "Current Usage by Active Processes: " usage
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
                                        string-append "Initial Available Resources:       " avail_str
                                      )
                                    )
                                     (
                                      string-append "Initial Available Resources:       " avail_str
                                    )
                                     (
                                      to-str (
                                        string-append "Initial Available Resources:       " avail_str
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
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        bankers_algorithm claim alloc max
      )
       (
        call/cc (
          lambda (
            ret27
          )
           (
            let (
              (
                need_list (
                  need max alloc
                )
              )
            )
             (
              begin (
                let (
                  (
                    alloc_sum (
                      processes_resource_summation alloc
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        avail (
                          available_resources claim alloc_sum
                        )
                      )
                    )
                     (
                      begin (
                        _display (
                          if (
                            string? "__________________________________________________"
                          )
                           "__________________________________________________" (
                            to-str "__________________________________________________"
                          )
                        )
                      )
                       (
                        newline
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
                        let (
                          (
                            finished (
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
                                              < i (
                                                _len need_list
                                              )
                                            )
                                             (
                                              begin (
                                                set! finished (
                                                  append finished (
                                                    _list #f
                                                  )
                                                )
                                              )
                                               (
                                                set! i (
                                                  + i 1
                                                )
                                              )
                                               (
                                                loop28
                                              )
                                            )
                                             '(
                                              
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
                                    remaining (
                                      _len need_list
                                    )
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
                                                  > remaining 0
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        safe #f
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            p 0
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            call/cc (
                                                              lambda (
                                                                break33
                                                              )
                                                               (
                                                                letrec (
                                                                  (
                                                                    loop32 (
                                                                      lambda (
                                                                        
                                                                      )
                                                                       (
                                                                        if (
                                                                          < p (
                                                                            _len need_list
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            if (
                                                                              not (
                                                                                list-ref-safe finished p
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    exec #t
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
                                                                                            break35
                                                                                          )
                                                                                           (
                                                                                            letrec (
                                                                                              (
                                                                                                loop34 (
                                                                                                  lambda (
                                                                                                    
                                                                                                  )
                                                                                                   (
                                                                                                    if (
                                                                                                      < r (
                                                                                                        _len avail
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        if (
                                                                                                          _gt (
                                                                                                            cond (
                                                                                                              (
                                                                                                                string? (
                                                                                                                  cond (
                                                                                                                    (
                                                                                                                      string? need_list
                                                                                                                    )
                                                                                                                     (
                                                                                                                      _substring need_list p (
                                                                                                                        + p 1
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    (
                                                                                                                      hash-table? need_list
                                                                                                                    )
                                                                                                                     (
                                                                                                                      hash-table-ref need_list p
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    else (
                                                                                                                      list-ref-safe need_list p
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                _substring (
                                                                                                                  cond (
                                                                                                                    (
                                                                                                                      string? need_list
                                                                                                                    )
                                                                                                                     (
                                                                                                                      _substring need_list p (
                                                                                                                        + p 1
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    (
                                                                                                                      hash-table? need_list
                                                                                                                    )
                                                                                                                     (
                                                                                                                      hash-table-ref need_list p
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    else (
                                                                                                                      list-ref-safe need_list p
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 r (
                                                                                                                  + r 1
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              (
                                                                                                                hash-table? (
                                                                                                                  cond (
                                                                                                                    (
                                                                                                                      string? need_list
                                                                                                                    )
                                                                                                                     (
                                                                                                                      _substring need_list p (
                                                                                                                        + p 1
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    (
                                                                                                                      hash-table? need_list
                                                                                                                    )
                                                                                                                     (
                                                                                                                      hash-table-ref need_list p
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    else (
                                                                                                                      list-ref-safe need_list p
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                hash-table-ref (
                                                                                                                  cond (
                                                                                                                    (
                                                                                                                      string? need_list
                                                                                                                    )
                                                                                                                     (
                                                                                                                      _substring need_list p (
                                                                                                                        + p 1
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    (
                                                                                                                      hash-table? need_list
                                                                                                                    )
                                                                                                                     (
                                                                                                                      hash-table-ref need_list p
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    else (
                                                                                                                      list-ref-safe need_list p
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 r
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              else (
                                                                                                                list-ref-safe (
                                                                                                                  cond (
                                                                                                                    (
                                                                                                                      string? need_list
                                                                                                                    )
                                                                                                                     (
                                                                                                                      _substring need_list p (
                                                                                                                        + p 1
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    (
                                                                                                                      hash-table? need_list
                                                                                                                    )
                                                                                                                     (
                                                                                                                      hash-table-ref need_list p
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    else (
                                                                                                                      list-ref-safe need_list p
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 r
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            cond (
                                                                                                              (
                                                                                                                string? avail
                                                                                                              )
                                                                                                               (
                                                                                                                _substring avail r (
                                                                                                                  + r 1
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              (
                                                                                                                hash-table? avail
                                                                                                              )
                                                                                                               (
                                                                                                                hash-table-ref avail r
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              else (
                                                                                                                list-ref-safe avail r
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            set! exec #f
                                                                                                          )
                                                                                                           (
                                                                                                            break35 '(
                                                                                                              
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         '(
                                                                                                          
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        set! r (
                                                                                                          + r 1
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        loop34
                                                                                                      )
                                                                                                    )
                                                                                                     '(
                                                                                                      
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              loop34
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        if exec (
                                                                                          begin (
                                                                                            set! safe #t
                                                                                          )
                                                                                           (
                                                                                            _display (
                                                                                              if (
                                                                                                string? (
                                                                                                  string-append (
                                                                                                    string-append "Process " (
                                                                                                      to-str-space (
                                                                                                        + p 1
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   " is executing."
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                string-append (
                                                                                                  string-append "Process " (
                                                                                                    to-str-space (
                                                                                                      + p 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 " is executing."
                                                                                              )
                                                                                               (
                                                                                                to-str (
                                                                                                  string-append (
                                                                                                    string-append "Process " (
                                                                                                      to-str-space (
                                                                                                        + p 1
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   " is executing."
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            newline
                                                                                          )
                                                                                           (
                                                                                            set! r 0
                                                                                          )
                                                                                           (
                                                                                            call/cc (
                                                                                              lambda (
                                                                                                break37
                                                                                              )
                                                                                               (
                                                                                                letrec (
                                                                                                  (
                                                                                                    loop36 (
                                                                                                      lambda (
                                                                                                        
                                                                                                      )
                                                                                                       (
                                                                                                        if (
                                                                                                          < r (
                                                                                                            _len avail
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            list-set! avail r (
                                                                                                              _add (
                                                                                                                cond (
                                                                                                                  (
                                                                                                                    string? avail
                                                                                                                  )
                                                                                                                   (
                                                                                                                    _substring avail r (
                                                                                                                      + r 1
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  (
                                                                                                                    hash-table? avail
                                                                                                                  )
                                                                                                                   (
                                                                                                                    hash-table-ref avail r
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  else (
                                                                                                                    list-ref-safe avail r
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                cond (
                                                                                                                  (
                                                                                                                    string? (
                                                                                                                      list-ref-safe alloc p
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    _substring (
                                                                                                                      list-ref-safe alloc p
                                                                                                                    )
                                                                                                                     r (
                                                                                                                      + r 1
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  (
                                                                                                                    hash-table? (
                                                                                                                      list-ref-safe alloc p
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    hash-table-ref (
                                                                                                                      list-ref-safe alloc p
                                                                                                                    )
                                                                                                                     r
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  else (
                                                                                                                    list-ref-safe (
                                                                                                                      list-ref-safe alloc p
                                                                                                                    )
                                                                                                                     r
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            set! r (
                                                                                                              + r 1
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            loop36
                                                                                                          )
                                                                                                        )
                                                                                                         '(
                                                                                                          
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  loop36
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            let (
                                                                                              (
                                                                                                avail_str ""
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! r 0
                                                                                              )
                                                                                               (
                                                                                                call/cc (
                                                                                                  lambda (
                                                                                                    break39
                                                                                                  )
                                                                                                   (
                                                                                                    letrec (
                                                                                                      (
                                                                                                        loop38 (
                                                                                                          lambda (
                                                                                                            
                                                                                                          )
                                                                                                           (
                                                                                                            if (
                                                                                                              < r (
                                                                                                                _len avail
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                if (
                                                                                                                  > r 0
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    set! avail_str (
                                                                                                                      string-append avail_str " "
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 '(
                                                                                                                  
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                set! avail_str (
                                                                                                                  string-append avail_str (
                                                                                                                    to-str-space (
                                                                                                                      cond (
                                                                                                                        (
                                                                                                                          string? avail
                                                                                                                        )
                                                                                                                         (
                                                                                                                          _substring avail r (
                                                                                                                            + r 1
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        (
                                                                                                                          hash-table? avail
                                                                                                                        )
                                                                                                                         (
                                                                                                                          hash-table-ref avail r
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        else (
                                                                                                                          list-ref-safe avail r
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                set! r (
                                                                                                                  + r 1
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                loop38
                                                                                                              )
                                                                                                            )
                                                                                                             '(
                                                                                                              
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      loop38
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _display (
                                                                                                  if (
                                                                                                    string? (
                                                                                                      string-append "Updated available resource stack for processes: " avail_str
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    string-append "Updated available resource stack for processes: " avail_str
                                                                                                  )
                                                                                                   (
                                                                                                    to-str (
                                                                                                      string-append "Updated available resource stack for processes: " avail_str
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
                                                                                                    string? "The process is in a safe state."
                                                                                                  )
                                                                                                   "The process is in a safe state." (
                                                                                                    to-str "The process is in a safe state."
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                newline
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
                                                                                                list-set! finished p #t
                                                                                              )
                                                                                               (
                                                                                                set! remaining (
                                                                                                  - remaining 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         '(
                                                                                          
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             '(
                                                                              
                                                                            )
                                                                          )
                                                                           (
                                                                            set! p (
                                                                              + p 1
                                                                            )
                                                                          )
                                                                           (
                                                                            loop32
                                                                          )
                                                                        )
                                                                         '(
                                                                          
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  loop32
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            if (
                                                              not safe
                                                            )
                                                             (
                                                              begin (
                                                                _display (
                                                                  if (
                                                                    string? "System in unsafe state. Aborting..."
                                                                  )
                                                                   "System in unsafe state. Aborting..." (
                                                                    to-str "System in unsafe state. Aborting..."
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                newline
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
                                                                break31 '(
                                                                  
                                                                )
                                                              )
                                                            )
                                                             '(
                                                              
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
                                                 '(
                                                  
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
     (
      let (
        (
          claim_vector (
            _list 8 5 9 7
          )
        )
      )
       (
        begin (
          let (
            (
              allocated_resources_table (
                _list (
                  _list 2 0 1 1
                )
                 (
                  _list 0 1 2 1
                )
                 (
                  _list 4 0 0 3
                )
                 (
                  _list 0 2 1 0
                )
                 (
                  _list 1 0 3 0
                )
              )
            )
          )
           (
            begin (
              let (
                (
                  maximum_claim_table (
                    _list (
                      _list 3 2 1 4
                    )
                     (
                      _list 0 2 5 2
                    )
                     (
                      _list 5 1 0 5
                    )
                     (
                      _list 1 5 3 0
                    )
                     (
                      _list 3 0 3 3
                    )
                  )
                )
              )
               (
                begin (
                  pretty_print claim_vector allocated_resources_table maximum_claim_table
                )
                 (
                  bankers_algorithm claim_vector allocated_resources_table maximum_claim_table
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
          end41 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur42 (
              quotient (
                * (
                  - end41 start40
                )
                 1000000
              )
               jps43
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur42
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
