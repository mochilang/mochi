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
        mod_pow base exp mod
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
                    b (
                      modulo base mod
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
                                      > e 0
                                    )
                                     (
                                      begin (
                                        if (
                                          equal? (
                                            modulo e 2
                                          )
                                           1
                                        )
                                         (
                                          begin (
                                            set! result (
                                              modulo (
                                                * result b
                                              )
                                               mod
                                            )
                                          )
                                        )
                                         (
                                          quote (
                                            
                                          )
                                        )
                                      )
                                       (
                                        set! b (
                                          modulo (
                                            * b b
                                          )
                                           mod
                                        )
                                      )
                                       (
                                        set! e (
                                          quotient e 2
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
      )
    )
     (
      define (
        miller_rabin n allow_probable
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                equal? n 2
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
              if (
                or (
                  < n 2
                )
                 (
                  equal? (
                    modulo n 2
                  )
                   0
                )
              )
               (
                begin (
                  ret4 #f
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                > n 5
              )
               (
                begin (
                  let (
                    (
                      last (
                        modulo n 10
                      )
                    )
                  )
                   (
                    begin (
                      if (
                        not (
                          or (
                            or (
                              or (
                                equal? last 1
                              )
                               (
                                equal? last 3
                              )
                            )
                             (
                              equal? last 7
                            )
                          )
                           (
                            equal? last 9
                          )
                        )
                      )
                       (
                        begin (
                          ret4 #f
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
              let (
                (
                  limit 3825123056546413051
                )
              )
               (
                begin (
                  if (
                    and (
                      > n limit
                    )
                     (
                      not allow_probable
                    )
                  )
                   (
                    begin (
                      panic "Warning: upper bound of deterministic test is exceeded. Pass allow_probable=true to allow probabilistic test."
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
                      bounds (
                        _list 2047 1373653 25326001 3215031751 2152302898747 3474749660383 341550071728321 limit
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          primes (
                            _list 2 3 5 7 11 13 17 19
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
                                  plist_len (
                                    _len primes
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
                                                < i (
                                                  _len bounds
                                                )
                                              )
                                               (
                                                begin (
                                                  if (
                                                    < n (
                                                      list-ref bounds i
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! plist_len (
                                                        + i 1
                                                      )
                                                    )
                                                     (
                                                      set! i (
                                                        _len bounds
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! i (
                                                        + i 1
                                                      )
                                                    )
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
                                      d (
                                        - n 1
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          s 0
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
                                                        equal? (
                                                          modulo d 2
                                                        )
                                                         0
                                                      )
                                                       (
                                                        begin (
                                                          set! d (
                                                            quotient d 2
                                                          )
                                                        )
                                                         (
                                                          set! s (
                                                            + s 1
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
                                          let (
                                            (
                                              j 0
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
                                                            < j plist_len
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  prime (
                                                                    list-ref primes j
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      x (
                                                                        mod_pow prime d n
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      let (
                                                                        (
                                                                          pr #f
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          if (
                                                                            or (
                                                                              equal? x 1
                                                                            )
                                                                             (
                                                                              equal? x (
                                                                                - n 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              set! pr #t
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              let (
                                                                                (
                                                                                  r 1
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
                                                                                                and (
                                                                                                  < r s
                                                                                                )
                                                                                                 (
                                                                                                  not pr
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                begin (
                                                                                                  set! x (
                                                                                                    fmod (
                                                                                                      * x x
                                                                                                    )
                                                                                                     n
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  if (
                                                                                                    equal? x (
                                                                                                      - n 1
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    begin (
                                                                                                      set! pr #t
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    quote (
                                                                                                      
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  set! r (
                                                                                                    + r 1
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
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          if (
                                                                            not pr
                                                                          )
                                                                           (
                                                                            begin (
                                                                              ret4 #f
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
                                              ret4 #t
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
      _display (
        if (
          string? (
            to-str-space (
              miller_rabin 561 #f
            )
          )
        )
         (
          to-str-space (
            miller_rabin 561 #f
          )
        )
         (
          to-str (
            to-str-space (
              miller_rabin 561 #f
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
              miller_rabin 563 #f
            )
          )
        )
         (
          to-str-space (
            miller_rabin 563 #f
          )
        )
         (
          to-str (
            to-str-space (
              miller_rabin 563 #f
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
              miller_rabin 838201 #f
            )
          )
        )
         (
          to-str-space (
            miller_rabin 838201 #f
          )
        )
         (
          to-str (
            to-str-space (
              miller_rabin 838201 #f
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
              miller_rabin 838207 #f
            )
          )
        )
         (
          to-str-space (
            miller_rabin 838207 #f
          )
        )
         (
          to-str (
            to-str-space (
              miller_rabin 838207 #f
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
              miller_rabin 17316001 #f
            )
          )
        )
         (
          to-str-space (
            miller_rabin 17316001 #f
          )
        )
         (
          to-str (
            to-str-space (
              miller_rabin 17316001 #f
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
              miller_rabin 17316017 #f
            )
          )
        )
         (
          to-str-space (
            miller_rabin 17316017 #f
          )
        )
         (
          to-str (
            to-str-space (
              miller_rabin 17316017 #f
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
              miller_rabin 3078386641 #f
            )
          )
        )
         (
          to-str-space (
            miller_rabin 3078386641 #f
          )
        )
         (
          to-str (
            to-str-space (
              miller_rabin 3078386641 #f
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
              miller_rabin 3078386653 #f
            )
          )
        )
         (
          to-str-space (
            miller_rabin 3078386653 #f
          )
        )
         (
          to-str (
            to-str-space (
              miller_rabin 3078386653 #f
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
              miller_rabin 1713045574801 #f
            )
          )
        )
         (
          to-str-space (
            miller_rabin 1713045574801 #f
          )
        )
         (
          to-str (
            to-str-space (
              miller_rabin 1713045574801 #f
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
              miller_rabin 1713045574819 #f
            )
          )
        )
         (
          to-str-space (
            miller_rabin 1713045574819 #f
          )
        )
         (
          to-str (
            to-str-space (
              miller_rabin 1713045574819 #f
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
              miller_rabin 2779799728307 #f
            )
          )
        )
         (
          to-str-space (
            miller_rabin 2779799728307 #f
          )
        )
         (
          to-str (
            to-str-space (
              miller_rabin 2779799728307 #f
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
              miller_rabin 2779799728327 #f
            )
          )
        )
         (
          to-str-space (
            miller_rabin 2779799728327 #f
          )
        )
         (
          to-str (
            to-str-space (
              miller_rabin 2779799728327 #f
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
              miller_rabin 113850023909441 #f
            )
          )
        )
         (
          to-str-space (
            miller_rabin 113850023909441 #f
          )
        )
         (
          to-str (
            to-str-space (
              miller_rabin 113850023909441 #f
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
              miller_rabin 113850023909527 #f
            )
          )
        )
         (
          to-str-space (
            miller_rabin 113850023909527 #f
          )
        )
         (
          to-str (
            to-str-space (
              miller_rabin 113850023909527 #f
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
              miller_rabin 1275041018848804351 #f
            )
          )
        )
         (
          to-str-space (
            miller_rabin 1275041018848804351 #f
          )
        )
         (
          to-str (
            to-str-space (
              miller_rabin 1275041018848804351 #f
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
              miller_rabin 1275041018848804391 #f
            )
          )
        )
         (
          to-str-space (
            miller_rabin 1275041018848804391 #f
          )
        )
         (
          to-str (
            to-str-space (
              miller_rabin 1275041018848804391 #f
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
