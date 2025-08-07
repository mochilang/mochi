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
      start13 (
        current-jiffy
      )
    )
     (
      jps16 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        isqrt n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                r 0
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
                              <= (
                                * (
                                  + r 1
                                )
                                 (
                                  + r 1
                                )
                              )
                               n
                            )
                             (
                              begin (
                                set! r (
                                  + r 1
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
                ret1 r
              )
            )
          )
        )
      )
    )
     (
      define (
        prime_sieve num
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                <= num 0
              )
               (
                begin (
                  panic "Invalid input, please enter a positive integer."
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  sieve (
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
                                    <= i num
                                  )
                                   (
                                    begin (
                                      set! sieve (
                                        append sieve (
                                          _list #t
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
                                   '(
                                    
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
                          prime (
                            _list
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              start 2
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  end (
                                    isqrt num
                                  )
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
                                                _le start end
                                              )
                                               (
                                                begin (
                                                  if (
                                                    list-ref-safe sieve start
                                                  )
                                                   (
                                                    begin (
                                                      set! prime (
                                                        append prime (
                                                          _list start
                                                        )
                                                      )
                                                    )
                                                     (
                                                      let (
                                                        (
                                                          j (
                                                            * start start
                                                          )
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
                                                                        <= j num
                                                                      )
                                                                       (
                                                                        begin (
                                                                          if (
                                                                            list-ref-safe sieve j
                                                                          )
                                                                           (
                                                                            begin (
                                                                              list-set! sieve j #f
                                                                            )
                                                                          )
                                                                           '(
                                                                            
                                                                          )
                                                                        )
                                                                         (
                                                                          set! j (
                                                                            + j start
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
                                                      )
                                                    )
                                                  )
                                                   '(
                                                    
                                                  )
                                                )
                                                 (
                                                  set! start (
                                                    + start 1
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
                                  let (
                                    (
                                      k (
                                        _add end 1
                                      )
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
                                                    _le k num
                                                  )
                                                   (
                                                    begin (
                                                      if (
                                                        list-ref-safe sieve k
                                                      )
                                                       (
                                                        begin (
                                                          set! prime (
                                                            append prime (
                                                              _list k
                                                            )
                                                          )
                                                        )
                                                      )
                                                       '(
                                                        
                                                      )
                                                    )
                                                     (
                                                      set! k (
                                                        _add k 1
                                                      )
                                                    )
                                                     (
                                                      loop11
                                                    )
                                                  )
                                                   '(
                                                    
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
                                      ret4 prime
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
      _display (
        if (
          string? (
            to-str-space (
              prime_sieve 50
            )
          )
        )
         (
          to-str-space (
            prime_sieve 50
          )
        )
         (
          to-str (
            to-str-space (
              prime_sieve 50
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
              prime_sieve 25
            )
          )
        )
         (
          to-str-space (
            prime_sieve 25
          )
        )
         (
          to-str (
            to-str-space (
              prime_sieve 25
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
              prime_sieve 10
            )
          )
        )
         (
          to-str-space (
            prime_sieve 10
          )
        )
         (
          to-str (
            to-str-space (
              prime_sieve 10
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
              prime_sieve 9
            )
          )
        )
         (
          to-str-space (
            prime_sieve 9
          )
        )
         (
          to-str (
            to-str-space (
              prime_sieve 9
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
              prime_sieve 2
            )
          )
        )
         (
          to-str-space (
            prime_sieve 2
          )
        )
         (
          to-str (
            to-str-space (
              prime_sieve 2
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
              prime_sieve 1
            )
          )
        )
         (
          to-str-space (
            prime_sieve 1
          )
        )
         (
          to-str (
            to-str-space (
              prime_sieve 1
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
          end14 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur15 (
              quotient (
                * (
                  - end14 start13
                )
                 1000000
              )
               jps16
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur15
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
