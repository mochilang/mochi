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
      start14 (
        current-jiffy
      )
    )
     (
      jps17 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        totient n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                is_prime (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    totients (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        primes (
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
                                          <= i n
                                        )
                                         (
                                          begin (
                                            set! is_prime (
                                              append is_prime (
                                                _list #t
                                              )
                                            )
                                          )
                                           (
                                            set! totients (
                                              append totients (
                                                _list (
                                                  - i 1
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
                            set! i 2
                          )
                           (
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
                                          <= i n
                                        )
                                         (
                                          begin (
                                            if (
                                              list-ref is_prime i
                                            )
                                             (
                                              begin (
                                                set! primes (
                                                  append primes (
                                                    _list i
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
                                                j 0
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
                                                              < j (
                                                                _len primes
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    p (
                                                                      list-ref primes j
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      >= (
                                                                        * i p
                                                                      )
                                                                       n
                                                                    )
                                                                     (
                                                                      begin (
                                                                        break7 (
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
                                                                    list-set! is_prime (
                                                                      * i p
                                                                    )
                                                                     #f
                                                                  )
                                                                   (
                                                                    if (
                                                                      equal? (
                                                                        _mod i p
                                                                      )
                                                                       0
                                                                    )
                                                                     (
                                                                      begin (
                                                                        list-set! totients (
                                                                          * i p
                                                                        )
                                                                         (
                                                                          * (
                                                                            list-ref totients i
                                                                          )
                                                                           p
                                                                        )
                                                                      )
                                                                       (
                                                                        break7 (
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
                                                                    list-set! totients (
                                                                      * i p
                                                                    )
                                                                     (
                                                                      * (
                                                                        list-ref totients i
                                                                      )
                                                                       (
                                                                        - p 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! j (
                                                                      + j 1
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
                                                set! i (
                                                  + i 1
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
                            ret1 totients
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
        test_totient
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                expected (
                  _list (
                    - 1
                  )
                   0 1 2 2 4 2 6 4 6 9
                )
              )
            )
             (
              begin (
                let (
                  (
                    res (
                      totient 10
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
                                      < idx (
                                        _len expected
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          not (
                                            equal? (
                                              cond (
                                                (
                                                  string? res
                                                )
                                                 (
                                                  _substring res idx (
                                                    + idx 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? res
                                                )
                                                 (
                                                  hash-table-ref res idx
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref res idx
                                                )
                                              )
                                            )
                                             (
                                              list-ref expected idx
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            panic (
                                              string-append "totient mismatch at " (
                                                to-str-space idx
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
            ret11
          )
           (
            begin (
              test_totient
            )
             (
              let (
                (
                  n 10
                )
              )
               (
                begin (
                  let (
                    (
                      res (
                        totient n
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
                                        < i n
                                      )
                                       (
                                        begin (
                                          _display (
                                            if (
                                              string? (
                                                string-append (
                                                  string-append (
                                                    string-append (
                                                      to-str-space i
                                                    )
                                                     " has "
                                                  )
                                                   (
                                                    to-str-space (
                                                      cond (
                                                        (
                                                          string? res
                                                        )
                                                         (
                                                          _substring res i (
                                                            + i 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? res
                                                        )
                                                         (
                                                          hash-table-ref res i
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref res i
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 " relative primes."
                                              )
                                            )
                                             (
                                              string-append (
                                                string-append (
                                                  string-append (
                                                    to-str-space i
                                                  )
                                                   " has "
                                                )
                                                 (
                                                  to-str-space (
                                                    cond (
                                                      (
                                                        string? res
                                                      )
                                                       (
                                                        _substring res i (
                                                          + i 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        hash-table? res
                                                      )
                                                       (
                                                        hash-table-ref res i
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        list-ref res i
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               " relative primes."
                                            )
                                             (
                                              to-str (
                                                string-append (
                                                  string-append (
                                                    string-append (
                                                      to-str-space i
                                                    )
                                                     " has "
                                                  )
                                                   (
                                                    to-str-space (
                                                      cond (
                                                        (
                                                          string? res
                                                        )
                                                         (
                                                          _substring res i (
                                                            + i 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? res
                                                        )
                                                         (
                                                          hash-table-ref res i
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref res i
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 " relative primes."
                                              )
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
      main
    )
     (
      let (
        (
          end15 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur16 (
              quotient (
                * (
                  - end15 start14
                )
                 1000000
              )
               jps17
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur16
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
