;; Generated on 2025-08-06 22:04 +0700
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
        format_ruleset ruleset
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                rs ruleset
              )
            )
             (
              begin (
                let (
                  (
                    bits_rev (
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
                                      < i 8
                                    )
                                     (
                                      begin (
                                        set! bits_rev (
                                          append bits_rev (
                                            _list (
                                              modulo rs 2
                                            )
                                          )
                                        )
                                      )
                                       (
                                        set! rs (
                                          quotient rs 2
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
                            bits (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                j (
                                  - (
                                    _len bits_rev
                                  )
                                   1
                                )
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
                                              >= j 0
                                            )
                                             (
                                              begin (
                                                set! bits (
                                                  append bits (
                                                    _list (
                                                      list-ref bits_rev j
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! j (
                                                  - j 1
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
                                ret1 bits
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
        new_generation cells rules time
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                population (
                  _len (
                    list-ref cells 0
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    next_generation (
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
                                      < i population
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            left_neighbor (
                                              if (
                                                equal? i 0
                                              )
                                               0 (
                                                cond (
                                                  (
                                                    string? (
                                                      list-ref cells time
                                                    )
                                                  )
                                                   (
                                                    _substring (
                                                      list-ref cells time
                                                    )
                                                     (
                                                      - i 1
                                                    )
                                                     (
                                                      + (
                                                        - i 1
                                                      )
                                                       1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? (
                                                      list-ref cells time
                                                    )
                                                  )
                                                   (
                                                    hash-table-ref (
                                                      list-ref cells time
                                                    )
                                                     (
                                                      - i 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref (
                                                      list-ref cells time
                                                    )
                                                     (
                                                      - i 1
                                                    )
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
                                                right_neighbor (
                                                  if (
                                                    equal? i (
                                                      - population 1
                                                    )
                                                  )
                                                   0 (
                                                    cond (
                                                      (
                                                        string? (
                                                          list-ref cells time
                                                        )
                                                      )
                                                       (
                                                        _substring (
                                                          list-ref cells time
                                                        )
                                                         (
                                                          + i 1
                                                        )
                                                         (
                                                          + (
                                                            + i 1
                                                          )
                                                           1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        hash-table? (
                                                          list-ref cells time
                                                        )
                                                      )
                                                       (
                                                        hash-table-ref (
                                                          list-ref cells time
                                                        )
                                                         (
                                                          + i 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        list-ref (
                                                          list-ref cells time
                                                        )
                                                         (
                                                          + i 1
                                                        )
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
                                                    center (
                                                      cond (
                                                        (
                                                          string? (
                                                            list-ref cells time
                                                          )
                                                        )
                                                         (
                                                          _substring (
                                                            list-ref cells time
                                                          )
                                                           i (
                                                            + i 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? (
                                                            list-ref cells time
                                                          )
                                                        )
                                                         (
                                                          hash-table-ref (
                                                            list-ref cells time
                                                          )
                                                           i
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref (
                                                            list-ref cells time
                                                          )
                                                           i
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        idx (
                                                          - 7 (
                                                            + (
                                                              + (
                                                                * left_neighbor 4
                                                              )
                                                               (
                                                                * center 2
                                                              )
                                                            )
                                                             right_neighbor
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! next_generation (
                                                          append next_generation (
                                                            _list (
                                                              list-ref rules idx
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
                        ret6 next_generation
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
        cells_to_string row
      )
       (
        call/cc (
          lambda (
            ret9
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
                                    _len row
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? (
                                        list-ref row i
                                      )
                                       1
                                    )
                                     (
                                      begin (
                                        set! result (
                                          string-append result "#"
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! result (
                                          string-append result "."
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
                    ret9 result
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
          initial (
            _list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
          )
        )
      )
       (
        begin (
          let (
            (
              cells (
                _list initial
              )
            )
          )
           (
            begin (
              let (
                (
                  rules (
                    format_ruleset 30
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      time 0
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
                                    < time 16
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          next (
                                            new_generation cells rules time
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          set! cells (
                                            append cells (
                                              _list next
                                            )
                                          )
                                        )
                                         (
                                          set! time (
                                            + time 1
                                          )
                                        )
                                      )
                                    )
                                     (
                                      loop12
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
                            loop12
                          )
                        )
                      )
                    )
                     (
                      let (
                        (
                          t 0
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
                                        < t (
                                          _len cells
                                        )
                                      )
                                       (
                                        begin (
                                          _display (
                                            if (
                                              string? (
                                                cells_to_string (
                                                  list-ref cells t
                                                )
                                              )
                                            )
                                             (
                                              cells_to_string (
                                                list-ref cells t
                                              )
                                            )
                                             (
                                              to-str (
                                                cells_to_string (
                                                  list-ref cells t
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          newline
                                        )
                                         (
                                          set! t (
                                            + t 1
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
