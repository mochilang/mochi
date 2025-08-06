;; Generated on 2025-08-06 23:15 +0700
(import (scheme base))
(import (scheme time))
(import (chibi string))
(import (only (scheme char) string-upcase string-downcase))
(import (srfi 69))
(import (srfi 1))
(define _list list)
(import (chibi time) (srfi 98))
(define _now_seeded #f)
(define _now_seed 0)
(define (now)
  (when (not _now_seeded)
    (let ((s (get-environment-variable "MOCHI_NOW_SEED")))
      (when (and s (string->number s))
        (set! _now_seed (string->number s))
        (set! _now_seeded #t))))
  (if _now_seeded
      (begin
        (set! _now_seed (modulo (+ (* _now_seed 1664525) 1013904223) 2147483647))
        _now_seed)
      (exact (floor (* (current-second) 1000000000))))
)
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
      start21 (
        current-jiffy
      )
    )
     (
      jps24 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        int_pow base exp
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
        pow_mod base exp mod
      )
       (
        call/cc (
          lambda (
            ret4
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
                                         (
                                          quote (
                                            
                                          )
                                        )
                                      )
                                       (
                                        set! e (
                                          _div e 2
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
                        ret4 result
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
        rand_range low high
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            ret7 (
              + (
                _mod (
                  now
                )
                 (
                  - high low
                )
              )
               low
            )
          )
        )
      )
    )
     (
      define (
        rabin_miller num
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                s (
                  - num 1
                )
              )
            )
             (
              begin (
                let (
                  (
                    t 0
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
                                  equal? (
                                    _mod s 2
                                  )
                                   0
                                )
                                 (
                                  begin (
                                    set! s (
                                      _div s 2
                                    )
                                  )
                                   (
                                    set! t (
                                      + t 1
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
                    let (
                      (
                        k 0
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
                                      < k 5
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            a (
                                              rand_range 2 (
                                                - num 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                v (
                                                  pow_mod a s num
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  not (
                                                    equal? v 1
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
                                                                        equal? v (
                                                                          - num 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          equal? i (
                                                                            - t 1
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            ret8 #f
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
                                                                        set! v (
                                                                          _mod (
                                                                            * v v
                                                                          )
                                                                           num
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
                                                    )
                                                  )
                                                )
                                                 (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                               (
                                                set! k (
                                                  + k 1
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
                        ret8 #t
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
        is_prime_low_num num
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            begin (
              if (
                < num 2
              )
               (
                begin (
                  ret15 #f
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
                  low_primes (
                    _list 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947 953 967 971 977 983 991 997
                  )
                )
              )
               (
                begin (
                  if (
                    cond (
                      (
                        string? low_primes
                      )
                       (
                        if (
                          string-contains low_primes num
                        )
                         #t #f
                      )
                    )
                     (
                      (
                        hash-table? low_primes
                      )
                       (
                        if (
                          hash-table-exists? low_primes num
                        )
                         #t #f
                      )
                    )
                     (
                      else (
                        if (
                          member num low_primes
                        )
                         #t #f
                      )
                    )
                  )
                   (
                    begin (
                      ret15 #t
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
                      i 0
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
                                    < i (
                                      _len low_primes
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          p (
                                            list-ref low_primes i
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            equal? (
                                              _mod num p
                                            )
                                             0
                                          )
                                           (
                                            begin (
                                              ret15 #f
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
                      ret15 (
                        rabin_miller num
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
        generate_large_prime keysize
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            let (
              (
                start (
                  int_pow 2 (
                    - keysize 1
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    end (
                      int_pow 2 keysize
                    )
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
                                if #t (
                                  begin (
                                    let (
                                      (
                                        num (
                                          rand_range start end
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          is_prime_low_num num
                                        )
                                         (
                                          begin (
                                            ret18 num
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
          p (
            generate_large_prime 16
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                string-append "Prime number: " (
                  to-str-space p
                )
              )
            )
             (
              string-append "Prime number: " (
                to-str-space p
              )
            )
             (
              to-str (
                string-append "Prime number: " (
                  to-str-space p
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
                string-append "is_prime_low_num: " (
                  to-str-space (
                    is_prime_low_num p
                  )
                )
              )
            )
             (
              string-append "is_prime_low_num: " (
                to-str-space (
                  is_prime_low_num p
                )
              )
            )
             (
              to-str (
                string-append "is_prime_low_num: " (
                  to-str-space (
                    is_prime_low_num p
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
          end22 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur23 (
              quotient (
                * (
                  - end22 start21
                )
                 1000000
              )
               jps24
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur23
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
