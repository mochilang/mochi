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
      start34 (
        current-jiffy
      )
    )
     (
      jps37 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        pow_int base exp
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                result 1
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
                                  < i exp
                                )
                                 (
                                  begin (
                                    set! result (
                                      * result base
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
                    ret1 result
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
        prime_factors n
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                <= n 0
              )
               (
                begin (
                  panic "Only positive integers have prime factors"
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
                  num n
                )
              )
               (
                begin (
                  let (
                    (
                      pf (
                        _list
                      )
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
                                    equal? (
                                      _mod num 2
                                    )
                                     0
                                  )
                                   (
                                    begin (
                                      set! pf (
                                        append pf (
                                          _list 2
                                        )
                                      )
                                    )
                                     (
                                      set! num (
                                        _div num 2
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
                      let (
                        (
                          i 3
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
                                        <= (
                                          * i i
                                        )
                                         num
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
                                                        equal? (
                                                          _mod num i
                                                        )
                                                         0
                                                      )
                                                       (
                                                        begin (
                                                          set! pf (
                                                            append pf (
                                                              _list i
                                                            )
                                                          )
                                                        )
                                                         (
                                                          set! num (
                                                            _div num i
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
                                          set! i (
                                            + i 2
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
                          if (
                            > num 2
                          )
                           (
                            begin (
                              set! pf (
                                append pf (
                                  _list num
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
                          ret4 pf
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
        number_of_divisors n
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            begin (
              if (
                <= n 0
              )
               (
                begin (
                  panic "Only positive numbers are accepted"
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
                  num n
                )
              )
               (
                begin (
                  let (
                    (
                      div 1
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          temp 1
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
                                        equal? (
                                          _mod num 2
                                        )
                                         0
                                      )
                                       (
                                        begin (
                                          set! temp (
                                            + temp 1
                                          )
                                        )
                                         (
                                          set! num (
                                            _div num 2
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
                          set! div (
                            * div temp
                          )
                        )
                         (
                          let (
                            (
                              i 3
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
                                            <= (
                                              * i i
                                            )
                                             num
                                          )
                                           (
                                            begin (
                                              set! temp 1
                                            )
                                             (
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
                                                            equal? (
                                                              _mod num i
                                                            )
                                                             0
                                                          )
                                                           (
                                                            begin (
                                                              set! temp (
                                                                + temp 1
                                                              )
                                                            )
                                                             (
                                                              set! num (
                                                                _div num i
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
                                              set! div (
                                                * div temp
                                              )
                                            )
                                             (
                                              set! i (
                                                + i 2
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
                              if (
                                > num 1
                              )
                               (
                                begin (
                                  set! div (
                                    * div 2
                                  )
                                )
                              )
                               (
                                quote (
                                  
                                )
                              )
                            )
                             (
                              ret11 div
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
        sum_of_divisors n
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            begin (
              if (
                <= n 0
              )
               (
                begin (
                  panic "Only positive numbers are accepted"
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
                  num n
                )
              )
               (
                begin (
                  let (
                    (
                      s 1
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          temp 1
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
                                        equal? (
                                          _mod num 2
                                        )
                                         0
                                      )
                                       (
                                        begin (
                                          set! temp (
                                            + temp 1
                                          )
                                        )
                                         (
                                          set! num (
                                            _div num 2
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
                          if (
                            > temp 1
                          )
                           (
                            begin (
                              set! s (
                                * s (
                                  _div (
                                    - (
                                      pow_int 2 temp
                                    )
                                     1
                                  )
                                   (
                                    - 2 1
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
                          let (
                            (
                              i 3
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
                                            <= (
                                              * i i
                                            )
                                             num
                                          )
                                           (
                                            begin (
                                              set! temp 1
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
                                                            equal? (
                                                              _mod num i
                                                            )
                                                             0
                                                          )
                                                           (
                                                            begin (
                                                              set! temp (
                                                                + temp 1
                                                              )
                                                            )
                                                             (
                                                              set! num (
                                                                _div num i
                                                              )
                                                            )
                                                             (
                                                              loop23
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
                                                    loop23
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              if (
                                                > temp 1
                                              )
                                               (
                                                begin (
                                                  set! s (
                                                    * s (
                                                      _div (
                                                        - (
                                                          pow_int i temp
                                                        )
                                                         1
                                                      )
                                                       (
                                                        - i 1
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
                                                + i 2
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
                              ret18 s
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
        contains arr x
      )
       (
        call/cc (
          lambda (
            ret25
          )
           (
            let (
              (
                idx 0
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
                              < idx (
                                _len arr
                              )
                            )
                             (
                              begin (
                                if (
                                  equal? (
                                    list-ref arr idx
                                  )
                                   x
                                )
                                 (
                                  begin (
                                    ret25 #t
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
                ret25 #f
              )
            )
          )
        )
      )
    )
     (
      define (
        unique arr
      )
       (
        call/cc (
          lambda (
            ret28
          )
           (
            let (
              (
                result (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    idx 0
                  )
                )
                 (
                  begin (
                    call/cc (
                      lambda (
                        break30
                      )
                       (
                        letrec (
                          (
                            loop29 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < idx (
                                    _len arr
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        v (
                                          list-ref arr idx
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          not (
                                            contains result v
                                          )
                                        )
                                         (
                                          begin (
                                            set! result (
                                              append result (
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
                                       (
                                        set! idx (
                                          + idx 1
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop29
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
                          loop29
                        )
                      )
                    )
                  )
                   (
                    ret28 result
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
        euler_phi n
      )
       (
        call/cc (
          lambda (
            ret31
          )
           (
            begin (
              if (
                <= n 0
              )
               (
                begin (
                  panic "Only positive numbers are accepted"
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
                  s n
                )
              )
               (
                begin (
                  let (
                    (
                      factors (
                        unique (
                          prime_factors n
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          idx 0
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
                                        < idx (
                                          _len factors
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              x (
                                                cond (
                                                  (
                                                    string? factors
                                                  )
                                                   (
                                                    _substring factors idx (
                                                      + idx 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? factors
                                                  )
                                                   (
                                                    hash-table-ref factors idx
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref factors idx
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! s (
                                                * (
                                                  _div s x
                                                )
                                                 (
                                                  - x 1
                                                )
                                              )
                                            )
                                             (
                                              set! idx (
                                                + idx 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          loop32
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
                                loop32
                              )
                            )
                          )
                        )
                         (
                          ret31 s
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
              prime_factors 100
            )
          )
        )
         (
          to-str-space (
            prime_factors 100
          )
        )
         (
          to-str (
            to-str-space (
              prime_factors 100
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
              number_of_divisors 100
            )
          )
        )
         (
          to-str-space (
            number_of_divisors 100
          )
        )
         (
          to-str (
            to-str-space (
              number_of_divisors 100
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
              sum_of_divisors 100
            )
          )
        )
         (
          to-str-space (
            sum_of_divisors 100
          )
        )
         (
          to-str (
            to-str-space (
              sum_of_divisors 100
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
              euler_phi 100
            )
          )
        )
         (
          to-str-space (
            euler_phi 100
          )
        )
         (
          to-str (
            to-str-space (
              euler_phi 100
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
          end35 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur36 (
              quotient (
                * (
                  - end35 start34
                )
                 1000000
              )
               jps37
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur36
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
