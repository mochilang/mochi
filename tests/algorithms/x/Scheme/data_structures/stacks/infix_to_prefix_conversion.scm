;; Generated on 2025-08-07 08:20 +0700
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
      start24 (
        current-jiffy
      )
    )
     (
      jps27 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          PRIORITY (
            alist->hash-table (
              _list (
                cons "^" 3
              )
               (
                cons "*" 2
              )
               (
                cons "/" 2
              )
               (
                cons "%" 2
              )
               (
                cons "+" 1
              )
               (
                cons "-" 1
              )
            )
          )
        )
      )
       (
        begin (
          let (
            (
              LETTERS "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
            )
          )
           (
            begin (
              let (
                (
                  DIGITS "0123456789"
                )
              )
               (
                begin (
                  define (
                    is_alpha ch
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
                                            _len LETTERS
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              string=? (
                                                _substring LETTERS i (
                                                  + i 1
                                                )
                                              )
                                               ch
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
                    is_digit ch
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
                                            _len DIGITS
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              string=? (
                                                _substring DIGITS i (
                                                  + i 1
                                                )
                                              )
                                               ch
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
                    reverse_string s
                  )
                   (
                    call/cc (
                      lambda (
                        ret7
                      )
                       (
                        let (
                          (
                            out ""
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                i (
                                  - (
                                    _len s
                                  )
                                   1
                                )
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
                                              >= i 0
                                            )
                                             (
                                              begin (
                                                set! out (
                                                  string-append out (
                                                    _substring s i (
                                                      + i 1
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! i (
                                                  - i 1
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
                                ret7 out
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
                    infix_to_postfix infix
                  )
                   (
                    call/cc (
                      lambda (
                        ret10
                      )
                       (
                        let (
                          (
                            stack (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                post (
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
                                                  < i (
                                                    _len infix
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        x (
                                                          _substring infix i (
                                                            + i 1
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          or (
                                                            is_alpha x
                                                          )
                                                           (
                                                            is_digit x
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! post (
                                                              append post (
                                                                _list x
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          if (
                                                            string=? x "("
                                                          )
                                                           (
                                                            begin (
                                                              set! stack (
                                                                append stack (
                                                                  _list x
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            if (
                                                              string=? x ")"
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  equal? (
                                                                    _len stack
                                                                  )
                                                                   0
                                                                )
                                                                 (
                                                                  begin (
                                                                    panic "list index out of range"
                                                                  )
                                                                )
                                                                 (
                                                                  quote (
                                                                    
                                                                  )
                                                                )
                                                              )
                                                               (
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
                                                                              not (
                                                                                string=? (
                                                                                  list-ref stack (
                                                                                    - (
                                                                                      _len stack
                                                                                    )
                                                                                     1
                                                                                  )
                                                                                )
                                                                                 "("
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! post (
                                                                                  append post (
                                                                                    _list (
                                                                                      list-ref stack (
                                                                                        - (
                                                                                          _len stack
                                                                                        )
                                                                                         1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! stack (
                                                                                  take (
                                                                                    drop stack 0
                                                                                  )
                                                                                   (
                                                                                    - (
                                                                                      - (
                                                                                        _len stack
                                                                                      )
                                                                                       1
                                                                                    )
                                                                                     0
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
                                                                set! stack (
                                                                  take (
                                                                    drop stack 0
                                                                  )
                                                                   (
                                                                    - (
                                                                      - (
                                                                        _len stack
                                                                      )
                                                                       1
                                                                    )
                                                                     0
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              if (
                                                                equal? (
                                                                  _len stack
                                                                )
                                                                 0
                                                              )
                                                               (
                                                                begin (
                                                                  set! stack (
                                                                    append stack (
                                                                      _list x
                                                                    )
                                                                  )
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
                                                                                and (
                                                                                  and (
                                                                                    > (
                                                                                      _len stack
                                                                                    )
                                                                                     0
                                                                                  )
                                                                                   (
                                                                                    not (
                                                                                      string=? (
                                                                                        list-ref stack (
                                                                                          - (
                                                                                            _len stack
                                                                                          )
                                                                                           1
                                                                                        )
                                                                                      )
                                                                                       "("
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  <= (
                                                                                    hash-table-ref/default PRIORITY x (
                                                                                      quote (
                                                                                        
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref/default PRIORITY (
                                                                                      list-ref stack (
                                                                                        - (
                                                                                          _len stack
                                                                                        )
                                                                                         1
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
                                                                                begin (
                                                                                  set! post (
                                                                                    append post (
                                                                                      _list (
                                                                                        list-ref stack (
                                                                                          - (
                                                                                            _len stack
                                                                                          )
                                                                                           1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  set! stack (
                                                                                    take (
                                                                                      drop stack 0
                                                                                    )
                                                                                     (
                                                                                      - (
                                                                                        - (
                                                                                          _len stack
                                                                                        )
                                                                                         1
                                                                                      )
                                                                                       0
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
                                                                  set! stack (
                                                                    append stack (
                                                                      _list x
                                                                    )
                                                                  )
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
                                                  > (
                                                    _len stack
                                                  )
                                                   0
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      string=? (
                                                        list-ref stack (
                                                          - (
                                                            _len stack
                                                          )
                                                           1
                                                        )
                                                      )
                                                       "("
                                                    )
                                                     (
                                                      begin (
                                                        panic "invalid expression"
                                                      )
                                                    )
                                                     (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! post (
                                                      append post (
                                                        _list (
                                                          list-ref stack (
                                                            - (
                                                              _len stack
                                                            )
                                                             1
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! stack (
                                                      take (
                                                        drop stack 0
                                                      )
                                                       (
                                                        - (
                                                          - (
                                                            _len stack
                                                          )
                                                           1
                                                        )
                                                         0
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
                                   (
                                    let (
                                      (
                                        res ""
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
                                                          < j (
                                                            _len post
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! res (
                                                              string-append res (
                                                                list-ref post j
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! j (
                                                              + j 1
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
                                            ret10 res
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
                    infix_to_prefix infix
                  )
                   (
                    call/cc (
                      lambda (
                        ret21
                      )
                       (
                        let (
                          (
                            reversed ""
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                i (
                                  - (
                                    _len infix
                                  )
                                   1
                                )
                              )
                            )
                             (
                              begin (
                                call/cc (
                                  lambda (
                                    break23
                                  )
                                   (
                                    letrec (
                                      (
                                        loop22 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              >= i 0
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    ch (
                                                      _substring infix i (
                                                        + i 1
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      string=? ch "("
                                                    )
                                                     (
                                                      begin (
                                                        set! reversed (
                                                          string-append reversed ")"
                                                        )
                                                      )
                                                    )
                                                     (
                                                      if (
                                                        string=? ch ")"
                                                      )
                                                       (
                                                        begin (
                                                          set! reversed (
                                                            string-append reversed "("
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          set! reversed (
                                                            string-append reversed ch
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! i (
                                                      - i 1
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                loop22
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
                                      loop22
                                    )
                                  )
                                )
                              )
                               (
                                let (
                                  (
                                    postfix (
                                      infix_to_postfix reversed
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        prefix (
                                          reverse_string postfix
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        ret21 prefix
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
          end25 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur26 (
              quotient (
                * (
                  - end25 start24
                )
                 1000000
              )
               jps27
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur26
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
