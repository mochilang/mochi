;; Generated on 2025-08-06 23:15 +0700
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
      start26 (
        current-jiffy
      )
    )
     (
      jps29 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        encrypt input_string key
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                <= key 0
              )
               (
                begin (
                  panic "Height of grid can't be 0 or negative"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                or (
                  equal? key 1
                )
                 (
                  <= (
                    _len input_string
                  )
                   key
                )
              )
               (
                begin (
                  ret1 input_string
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
                  lowest (
                    - key 1
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      temp_grid (
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
                                        < i key
                                      )
                                       (
                                        begin (
                                          set! temp_grid (
                                            append temp_grid (
                                              _list (
                                                _list
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
                          let (
                            (
                              position 0
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
                                            < position (
                                              _len input_string
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  num (
                                                    _mod position (
                                                      * lowest 2
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      alt (
                                                        - (
                                                          * lowest 2
                                                        )
                                                         num
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      if (
                                                        > num alt
                                                      )
                                                       (
                                                        begin (
                                                          set! num alt
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
                                                          row (
                                                            list-ref temp_grid num
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          set! row (
                                                            append row (
                                                              _list (
                                                                _substring input_string position (
                                                                  + position 1
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          list-set! temp_grid num row
                                                        )
                                                         (
                                                          set! position (
                                                            + position 1
                                                          )
                                                        )
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
                              let (
                                (
                                  output ""
                                )
                              )
                               (
                                begin (
                                  set! i 0
                                )
                                 (
                                  call/cc (
                                    lambda (
                                      break7
                                    )
                                     (
                                      letrec (
                                        (
                                          loop6 (
                                            lambda (
                                              
                                            )
                                             (
                                              if (
                                                < i key
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      row (
                                                        list-ref temp_grid i
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
                                                                        < j (
                                                                          _len row
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          set! output (
                                                                            string-append output (
                                                                              list-ref row j
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          set! j (
                                                                            + j 1
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
                                                          set! i (
                                                            + i 1
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  loop6
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
                                        loop6
                                      )
                                    )
                                  )
                                )
                                 (
                                  ret1 output
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
        decrypt input_string key
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            begin (
              if (
                <= key 0
              )
               (
                begin (
                  panic "Height of grid can't be 0 or negative"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                equal? key 1
              )
               (
                begin (
                  ret10 input_string
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
                  lowest (
                    - key 1
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
                                        < i key
                                      )
                                       (
                                        begin (
                                          set! counts (
                                            append counts (
                                              _list 0
                                            )
                                          )
                                        )
                                         (
                                          set! i (
                                            + i 1
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
                              pos 0
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
                                            < pos (
                                              _len input_string
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  num (
                                                    _mod pos (
                                                      * lowest 2
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      alt (
                                                        - (
                                                          * lowest 2
                                                        )
                                                         num
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      if (
                                                        > num alt
                                                      )
                                                       (
                                                        begin (
                                                          set! num alt
                                                        )
                                                      )
                                                       (
                                                        quote (
                                                          
                                                        )
                                                      )
                                                    )
                                                     (
                                                      list-set! counts num (
                                                        + (
                                                          list-ref counts num
                                                        )
                                                         1
                                                      )
                                                    )
                                                     (
                                                      set! pos (
                                                        + pos 1
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
                                  grid (
                                    _list
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      counter 0
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
                                                    < i key
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          length (
                                                            list-ref counts i
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              slice (
                                                                _substring input_string counter (
                                                                  + counter length
                                                                )
                                                              )
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
                                                                                      _len slice
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      set! row (
                                                                                        append row (
                                                                                          _list (
                                                                                            _substring slice j (
                                                                                              + j 1
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
                                                                     (
                                                                      set! grid (
                                                                        append grid (
                                                                          _list row
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      set! counter (
                                                                        + counter length
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
                                                          )
                                                        )
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
                                      let (
                                        (
                                          indices (
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
                                                        < i key
                                                      )
                                                       (
                                                        begin (
                                                          set! indices (
                                                            append indices (
                                                              _list 0
                                                            )
                                                          )
                                                        )
                                                         (
                                                          set! i (
                                                            + i 1
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
                                              output ""
                                            )
                                          )
                                           (
                                            begin (
                                              set! pos 0
                                            )
                                             (
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
                                                            < pos (
                                                              _len input_string
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  num (
                                                                    _mod pos (
                                                                      * lowest 2
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      alt (
                                                                        - (
                                                                          * lowest 2
                                                                        )
                                                                         num
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      if (
                                                                        > num alt
                                                                      )
                                                                       (
                                                                        begin (
                                                                          set! num alt
                                                                        )
                                                                      )
                                                                       (
                                                                        quote (
                                                                          
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      set! output (
                                                                        string-append output (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref grid num
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref grid num
                                                                              )
                                                                               (
                                                                                list-ref indices num
                                                                              )
                                                                               (
                                                                                + (
                                                                                  list-ref indices num
                                                                                )
                                                                                 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref grid num
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref grid num
                                                                              )
                                                                               (
                                                                                list-ref indices num
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref grid num
                                                                              )
                                                                               (
                                                                                list-ref indices num
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      list-set! indices num (
                                                                        + (
                                                                          list-ref indices num
                                                                        )
                                                                         1
                                                                      )
                                                                    )
                                                                     (
                                                                      set! pos (
                                                                        + pos 1
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
                                              ret10 output
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
        bruteforce input_string
      )
       (
        call/cc (
          lambda (
            ret23
          )
           (
            let (
              (
                results (
                  alist->hash-table (
                    _list
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    key_guess 1
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
                                  < key_guess (
                                    _len input_string
                                  )
                                )
                                 (
                                  begin (
                                    hash-table-set! results key_guess (
                                      decrypt input_string key_guess
                                    )
                                  )
                                   (
                                    set! key_guess (
                                      + key_guess 1
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
                    ret23 results
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
            encrypt "Hello World" 4
          )
        )
         (
          encrypt "Hello World" 4
        )
         (
          to-str (
            encrypt "Hello World" 4
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
            decrypt "HWe olordll" 4
          )
        )
         (
          decrypt "HWe olordll" 4
        )
         (
          to-str (
            decrypt "HWe olordll" 4
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
          bf (
            bruteforce "HWe olordll"
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
                    string? bf
                  )
                   (
                    _substring bf 4 (
                      + 4 1
                    )
                  )
                )
                 (
                  (
                    hash-table? bf
                  )
                   (
                    hash-table-ref bf 4
                  )
                )
                 (
                  else (
                    list-ref bf 4
                  )
                )
              )
            )
             (
              cond (
                (
                  string? bf
                )
                 (
                  _substring bf 4 (
                    + 4 1
                  )
                )
              )
               (
                (
                  hash-table? bf
                )
                 (
                  hash-table-ref bf 4
                )
              )
               (
                else (
                  list-ref bf 4
                )
              )
            )
             (
              to-str (
                cond (
                  (
                    string? bf
                  )
                   (
                    _substring bf 4 (
                      + 4 1
                    )
                  )
                )
                 (
                  (
                    hash-table? bf
                  )
                   (
                    hash-table-ref bf 4
                  )
                )
                 (
                  else (
                    list-ref bf 4
                  )
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
     (
      let (
        (
          end27 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur28 (
              quotient (
                * (
                  - end27 start26
                )
                 1000000
              )
               jps29
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur28
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
