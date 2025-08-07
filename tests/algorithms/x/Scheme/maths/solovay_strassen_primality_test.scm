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
      start15 (
        current-jiffy
      )
    )
     (
      jps18 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          seed 1
        )
      )
       (
        begin (
          define (
            set_seed s
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                set! seed s
              )
            )
          )
        )
         (
          define (
            randint a b
          )
           (
            call/cc (
              lambda (
                ret2
              )
               (
                begin (
                  set! seed (
                    _mod (
                      + (
                        * seed 1103515245
                      )
                       12345
                    )
                     2147483648
                  )
                )
                 (
                  ret2 (
                    + (
                      _mod seed (
                        + (
                          - b a
                        )
                         1
                      )
                    )
                     a
                  )
                )
              )
            )
          )
        )
         (
          define (
            jacobi_symbol random_a number
          )
           (
            call/cc (
              lambda (
                ret3
              )
               (
                begin (
                  if (
                    or (
                      equal? random_a 0
                    )
                     (
                      equal? random_a 1
                    )
                  )
                   (
                    begin (
                      ret3 random_a
                    )
                  )
                   '(
                    
                  )
                )
                 (
                  set! random_a (
                    _mod random_a number
                  )
                )
                 (
                  let (
                    (
                      t 1
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
                                    not (
                                      equal? random_a 0
                                    )
                                  )
                                   (
                                    begin (
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
                                                    equal? (
                                                      _mod random_a 2
                                                    )
                                                     0
                                                  )
                                                   (
                                                    begin (
                                                      set! random_a (
                                                        _div random_a 2
                                                      )
                                                    )
                                                     (
                                                      let (
                                                        (
                                                          r (
                                                            _mod number 8
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          if (
                                                            or (
                                                              equal? r 3
                                                            )
                                                             (
                                                              equal? r 5
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              set! t (
                                                                - t
                                                              )
                                                            )
                                                          )
                                                           '(
                                                            
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      loop6
                                                    )
                                                  )
                                                   '(
                                                    
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
                                      let (
                                        (
                                          temp random_a
                                        )
                                      )
                                       (
                                        begin (
                                          set! random_a number
                                        )
                                         (
                                          set! number temp
                                        )
                                         (
                                          if (
                                            and (
                                              equal? (
                                                _mod random_a 4
                                              )
                                               3
                                            )
                                             (
                                              equal? (
                                                _mod number 4
                                              )
                                               3
                                            )
                                          )
                                           (
                                            begin (
                                              set! t (
                                                - t
                                              )
                                            )
                                          )
                                           '(
                                            
                                          )
                                        )
                                         (
                                          set! random_a (
                                            _mod random_a number
                                          )
                                        )
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
                      if (
                        equal? number 1
                      )
                       (
                        begin (
                          ret3 t
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      ret3 0
                    )
                  )
                )
              )
            )
          )
        )
         (
          define (
            pow_mod base exp mod
          )
           (
            call/cc (
              lambda (
                ret8
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
                        b (
                          _mod base mod
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            e exp
                          )
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
                                          > e 0
                                        )
                                         (
                                          begin (
                                            if (
                                              equal? (
                                                _mod e 2
                                              )
                                               1
                                            )
                                             (
                                              begin (
                                                set! result (
                                                  _mod (
                                                    * result b
                                                  )
                                                   mod
                                                )
                                              )
                                            )
                                             '(
                                              
                                            )
                                          )
                                           (
                                            set! b (
                                              _mod (
                                                * b b
                                              )
                                               mod
                                            )
                                          )
                                           (
                                            set! e (
                                              _div e 2
                                            )
                                          )
                                           (
                                            loop9
                                          )
                                        )
                                         '(
                                          
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
                            ret8 result
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
            solovay_strassen number iterations
          )
           (
            call/cc (
              lambda (
                ret11
              )
               (
                begin (
                  if (
                    <= number 1
                  )
                   (
                    begin (
                      ret11 #f
                    )
                  )
                   '(
                    
                  )
                )
                 (
                  if (
                    <= number 3
                  )
                   (
                    begin (
                      ret11 #t
                    )
                  )
                   '(
                    
                  )
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
                                    < i iterations
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          a (
                                            randint 2 (
                                              - number 2
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              x (
                                                jacobi_symbol a number
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  y (
                                                    pow_mod a (
                                                      _div (
                                                        - number 1
                                                      )
                                                       2
                                                    )
                                                     number
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      mod_x (
                                                        _mod x number
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      if (
                                                        _lt mod_x 0
                                                      )
                                                       (
                                                        begin (
                                                          set! mod_x (
                                                            _add mod_x number
                                                          )
                                                        )
                                                      )
                                                       '(
                                                        
                                                      )
                                                    )
                                                     (
                                                      if (
                                                        or (
                                                          equal? x 0
                                                        )
                                                         (
                                                          not (
                                                            equal? y mod_x
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          ret11 #f
                                                        )
                                                      )
                                                       '(
                                                        
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
                      ret11 #t
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
                ret14
              )
               (
                begin (
                  set_seed 10
                )
                 (
                  _display (
                    if (
                      string? (
                        to-str-space (
                          solovay_strassen 13 5
                        )
                      )
                    )
                     (
                      to-str-space (
                        solovay_strassen 13 5
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          solovay_strassen 13 5
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
                          solovay_strassen 9 10
                        )
                      )
                    )
                     (
                      to-str-space (
                        solovay_strassen 9 10
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          solovay_strassen 9 10
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
                          solovay_strassen 17 15
                        )
                      )
                    )
                     (
                      to-str-space (
                        solovay_strassen 17 15
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          solovay_strassen 17 15
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
          )
        )
         (
          main
        )
      )
    )
     (
      let (
        (
          end16 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur17 (
              quotient (
                * (
                  - end16 start15
                )
                 1000000
              )
               jps18
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur17
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
