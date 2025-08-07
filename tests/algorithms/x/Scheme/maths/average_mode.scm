;; Generated on 2025-08-07 10:06 +0700
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
      start37 (
        current-jiffy
      )
    )
     (
      jps40 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        contains_int xs x
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
        contains_string xs x
      )
       (
        call/cc (
          lambda (
            ret4
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
                                _len xs
                              )
                            )
                             (
                              begin (
                                if (
                                  string=? (
                                    list-ref xs i
                                  )
                                   x
                                )
                                 (
                                  begin (
                                    ret4 #t
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
                ret4 #f
              )
            )
          )
        )
      )
    )
     (
      define (
        count_int xs x
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                cnt 0
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
                                        set! cnt (
                                          + cnt 1
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
                    ret7 cnt
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
        count_string xs x
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                cnt 0
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
                                  < i (
                                    _len xs
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      string=? (
                                        list-ref xs i
                                      )
                                       x
                                    )
                                     (
                                      begin (
                                        set! cnt (
                                          + cnt 1
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
                    ret10 cnt
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
        sort_int xs
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            let (
              (
                arr xs
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
                                    _len arr
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        j (
                                          + i 1
                                        )
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
                                                        _len arr
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          < (
                                                            list-ref arr j
                                                          )
                                                           (
                                                            list-ref arr i
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                tmp (
                                                                  list-ref arr i
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                list-set! arr i (
                                                                  list-ref arr j
                                                                )
                                                              )
                                                               (
                                                                list-set! arr j tmp
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
                    ret13 arr
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
        sort_string xs
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            let (
              (
                arr xs
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
                                    _len arr
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        j (
                                          + i 1
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
                                                      < j (
                                                        _len arr
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          string<? (
                                                            list-ref arr j
                                                          )
                                                           (
                                                            list-ref arr i
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                tmp (
                                                                  list-ref arr i
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                list-set! arr i (
                                                                  list-ref arr j
                                                                )
                                                              )
                                                               (
                                                                list-set! arr j tmp
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
                                                        set! j (
                                                          + j 1
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
                                        set! i (
                                          + i 1
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
                    ret18 arr
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
        mode_int lst
      )
       (
        call/cc (
          lambda (
            ret23
          )
           (
            begin (
              if (
                equal? (
                  _len lst
                )
                 0
              )
               (
                begin (
                  ret23 (
                    _list
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
                                      _len lst
                                    )
                                  )
                                   (
                                    begin (
                                      set! counts (
                                        append counts (
                                          _list (
                                            count_int lst (
                                              list-ref lst i
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
                          max_count 0
                        )
                      )
                       (
                        begin (
                          set! i 0
                        )
                         (
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
                                        < i (
                                          _len counts
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            > (
                                              list-ref counts i
                                            )
                                             max_count
                                          )
                                           (
                                            begin (
                                              set! max_count (
                                                list-ref counts i
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
                          let (
                            (
                              modes (
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
                                              _len lst
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                equal? (
                                                  list-ref counts i
                                                )
                                                 max_count
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      v (
                                                        list-ref lst i
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      if (
                                                        not (
                                                          contains_int modes v
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          set! modes (
                                                            append modes (
                                                              _list v
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
                              ret23 (
                                sort_int modes
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
        mode_string lst
      )
       (
        call/cc (
          lambda (
            ret30
          )
           (
            begin (
              if (
                equal? (
                  _len lst
                )
                 0
              )
               (
                begin (
                  ret30 (
                    _list
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
                          break32
                        )
                         (
                          letrec (
                            (
                              loop31 (
                                lambda (
                                  
                                )
                                 (
                                  if (
                                    < i (
                                      _len lst
                                    )
                                  )
                                   (
                                    begin (
                                      set! counts (
                                        append counts (
                                          _list (
                                            count_string lst (
                                              list-ref lst i
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
                                      loop31
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
                            loop31
                          )
                        )
                      )
                    )
                     (
                      let (
                        (
                          max_count 0
                        )
                      )
                       (
                        begin (
                          set! i 0
                        )
                         (
                          call/cc (
                            lambda (
                              break34
                            )
                             (
                              letrec (
                                (
                                  loop33 (
                                    lambda (
                                      
                                    )
                                     (
                                      if (
                                        < i (
                                          _len counts
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            > (
                                              list-ref counts i
                                            )
                                             max_count
                                          )
                                           (
                                            begin (
                                              set! max_count (
                                                list-ref counts i
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
                                          loop33
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
                                loop33
                              )
                            )
                          )
                        )
                         (
                          let (
                            (
                              modes (
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
                                  break36
                                )
                                 (
                                  letrec (
                                    (
                                      loop35 (
                                        lambda (
                                          
                                        )
                                         (
                                          if (
                                            < i (
                                              _len lst
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                equal? (
                                                  list-ref counts i
                                                )
                                                 max_count
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      v (
                                                        list-ref lst i
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      if (
                                                        not (
                                                          contains_string modes v
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          set! modes (
                                                            append modes (
                                                              _list v
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
                                              loop35
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
                                    loop35
                                  )
                                )
                              )
                            )
                             (
                              ret30 (
                                sort_string modes
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
            mode_int (
              _list 2 3 4 5 3 4 2 5 2 2 4 2 2 2
            )
          )
        )
         (
          mode_int (
            _list 2 3 4 5 3 4 2 5 2 2 4 2 2 2
          )
        )
         (
          to-str (
            mode_int (
              _list 2 3 4 5 3 4 2 5 2 2 4 2 2 2
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
            mode_int (
              _list 3 4 5 3 4 2 5 2 2 4 4 2 2 2
            )
          )
        )
         (
          mode_int (
            _list 3 4 5 3 4 2 5 2 2 4 4 2 2 2
          )
        )
         (
          to-str (
            mode_int (
              _list 3 4 5 3 4 2 5 2 2 4 4 2 2 2
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
            mode_int (
              _list 3 4 5 3 4 2 5 2 2 4 4 4 2 2 4 2
            )
          )
        )
         (
          mode_int (
            _list 3 4 5 3 4 2 5 2 2 4 4 4 2 2 4 2
          )
        )
         (
          to-str (
            mode_int (
              _list 3 4 5 3 4 2 5 2 2 4 4 4 2 2 4 2
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
            mode_string (
              _list "x" "y" "y" "z"
            )
          )
        )
         (
          mode_string (
            _list "x" "y" "y" "z"
          )
        )
         (
          to-str (
            mode_string (
              _list "x" "y" "y" "z"
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
            mode_string (
              _list "x" "x" "y" "y" "z"
            )
          )
        )
         (
          mode_string (
            _list "x" "x" "y" "y" "z"
          )
        )
         (
          to-str (
            mode_string (
              _list "x" "x" "y" "y" "z"
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
          end38 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur39 (
              quotient (
                * (
                  - end38 start37
                )
                 1000000
              )
               jps40
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur39
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
