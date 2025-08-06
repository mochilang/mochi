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
      let (
        (
          seed 123456789
        )
      )
       (
        begin (
          define (
            rand
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                begin (
                  set! seed (
                    modulo (
                      + (
                        * seed 1103515245
                      )
                       12345
                    )
                     2147483647
                  )
                )
                 (
                  ret1 seed
                )
              )
            )
          )
        )
         (
          define (
            rand_range min max
          )
           (
            call/cc (
              lambda (
                ret2
              )
               (
                ret2 (
                  _add min (
                    fmod (
                      rand
                    )
                     (
                      + (
                        - max min
                      )
                       1
                    )
                  )
                )
              )
            )
          )
        )
         (
          define (
            mod_pow base exponent modulus
          )
           (
            call/cc (
              lambda (
                ret3
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
                          modulo base modulus
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            e exponent
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
                                                   modulus
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
                                              quotient e 2
                                            )
                                          )
                                           (
                                            set! b (
                                              modulo (
                                                * b b
                                              )
                                               modulus
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
                            ret3 result
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
            extended_gcd a b
          )
           (
            call/cc (
              lambda (
                ret6
              )
               (
                begin (
                  if (
                    equal? b 0
                  )
                   (
                    begin (
                      ret6 (
                        alist->hash-table (
                          _list (
                            cons "g" a
                          )
                           (
                            cons "x" 1
                          )
                           (
                            cons "y" 0
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
                      res (
                        extended_gcd b (
                          modulo a b
                        )
                      )
                    )
                  )
                   (
                    begin (
                      ret6 (
                        alist->hash-table (
                          _list (
                            cons "g" (
                              hash-table-ref res "g"
                            )
                          )
                           (
                            cons "x" (
                              hash-table-ref res "y"
                            )
                          )
                           (
                            cons "y" (
                              - (
                                hash-table-ref res "x"
                              )
                               (
                                * (
                                  quotient a b
                                )
                                 (
                                  hash-table-ref res "y"
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
            mod_inverse a m
          )
           (
            call/cc (
              lambda (
                ret7
              )
               (
                let (
                  (
                    res (
                      extended_gcd a m
                    )
                  )
                )
                 (
                  begin (
                    if (
                      not (
                        equal? (
                          hash-table-ref res "g"
                        )
                         1
                      )
                    )
                     (
                      begin (
                        panic "inverse does not exist"
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
                        r (
                          fmod (
                            hash-table-ref res "x"
                          )
                           m
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          _lt r 0
                        )
                         (
                          begin (
                            ret7 (
                              _add r m
                            )
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        ret7 r
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
            pow2 n
          )
           (
            call/cc (
              lambda (
                ret8
              )
               (
                let (
                  (
                    r 1
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
                                      < i n
                                    )
                                     (
                                      begin (
                                        set! r (
                                          * r 2
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
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
                        ret8 r
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
            is_probable_prime n k
          )
           (
            call/cc (
              lambda (
                ret11
              )
               (
                begin (
                  if (
                    <= n 1
                  )
                   (
                    begin (
                      ret11 #f
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    <= n 3
                  )
                   (
                    begin (
                      ret11 #t
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? (
                      modulo n 2
                    )
                     0
                  )
                   (
                    begin (
                      ret11 #f
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
                      r 0
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          d (
                            - n 1
                          )
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
                                          set! r (
                                            + r 1
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
                                            < i k
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  a (
                                                    rand_range 2 (
                                                      - n 2
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      x (
                                                        mod_pow a d n
                                                      )
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
                                                          set! i (
                                                            + i 1
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
                                                     (
                                                      let (
                                                        (
                                                          j 1
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              found #f
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
                                                                            < j r
                                                                          )
                                                                           (
                                                                            begin (
                                                                              set! x (
                                                                                mod_pow x 2 n
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
                                                                                  set! found #t
                                                                                )
                                                                                 (
                                                                                  break17 (
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
                                                              if (
                                                                not found
                                                              )
                                                               (
                                                                begin (
                                                                  ret11 #f
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
                              ret11 #t
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
            generate_large_prime bits
          )
           (
            call/cc (
              lambda (
                ret18
              )
               (
                let (
                  (
                    min (
                      pow2 (
                        - bits 1
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        max (
                          - (
                            pow2 bits
                          )
                           1
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            p (
                              rand_range min max
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              equal? (
                                fmod p 2
                              )
                               0
                            )
                             (
                              begin (
                                set! p (
                                  _add p 1
                                )
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
                                          not (
                                            is_probable_prime p 5
                                          )
                                        )
                                         (
                                          begin (
                                            set! p (
                                              _add p 2
                                            )
                                          )
                                           (
                                            if (
                                              _gt p max
                                            )
                                             (
                                              begin (
                                                set! p (
                                                  _add min 1
                                                )
                                              )
                                            )
                                             (
                                              quote (
                                                
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
                            ret18 p
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
            primitive_root p
          )
           (
            call/cc (
              lambda (
                ret21
              )
               (
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
                            if #t (
                              begin (
                                let (
                                  (
                                    g (
                                      rand_range 3 (
                                        - p 1
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? (
                                        mod_pow g 2 p
                                      )
                                       1
                                    )
                                     (
                                      begin (
                                        loop22
                                      )
                                    )
                                     (
                                      quote (
                                        
                                      )
                                    )
                                  )
                                   (
                                    if (
                                      equal? (
                                        mod_pow g p p
                                      )
                                       1
                                    )
                                     (
                                      begin (
                                        loop22
                                      )
                                    )
                                     (
                                      quote (
                                        
                                      )
                                    )
                                  )
                                   (
                                    ret21 g
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
            )
          )
        )
         (
          define (
            generate_key key_size
          )
           (
            call/cc (
              lambda (
                ret24
              )
               (
                let (
                  (
                    p (
                      generate_large_prime key_size
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        e1 (
                          primitive_root p
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            d (
                              rand_range 3 (
                                - p 1
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                e2 (
                                  mod_inverse (
                                    mod_pow e1 d p
                                  )
                                   p
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    public_key (
                                      alist->hash-table (
                                        _list (
                                          cons "key_size" key_size
                                        )
                                         (
                                          cons "g" e1
                                        )
                                         (
                                          cons "e2" e2
                                        )
                                         (
                                          cons "p" p
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        private_key (
                                          alist->hash-table (
                                            _list (
                                              cons "key_size" key_size
                                            )
                                             (
                                              cons "d" d
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        ret24 (
                                          alist->hash-table (
                                            _list (
                                              cons "public_key" public_key
                                            )
                                             (
                                              cons "private_key" private_key
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
            main
          )
           (
            call/cc (
              lambda (
                ret25
              )
               (
                let (
                  (
                    key_size 16
                  )
                )
                 (
                  begin (
                    let (
                      (
                        kp (
                          generate_key key_size
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            pub (
                              hash-table-ref kp "public_key"
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                priv (
                                  hash-table-ref kp "private_key"
                                )
                              )
                            )
                             (
                              begin (
                                _display (
                                  if (
                                    string? (
                                      string-append (
                                        string-append (
                                          string-append (
                                            string-append (
                                              string-append (
                                                string-append (
                                                  string-append (
                                                    string-append "public key: (" (
                                                      to-str-space (
                                                        hash-table-ref pub "key_size"
                                                      )
                                                    )
                                                  )
                                                   ", "
                                                )
                                                 (
                                                  to-str-space (
                                                    hash-table-ref pub "g"
                                                  )
                                                )
                                              )
                                               ", "
                                            )
                                             (
                                              to-str-space (
                                                hash-table-ref pub "e2"
                                              )
                                            )
                                          )
                                           ", "
                                        )
                                         (
                                          to-str-space (
                                            hash-table-ref pub "p"
                                          )
                                        )
                                      )
                                       ")"
                                    )
                                  )
                                   (
                                    string-append (
                                      string-append (
                                        string-append (
                                          string-append (
                                            string-append (
                                              string-append (
                                                string-append (
                                                  string-append "public key: (" (
                                                    to-str-space (
                                                      hash-table-ref pub "key_size"
                                                    )
                                                  )
                                                )
                                                 ", "
                                              )
                                               (
                                                to-str-space (
                                                  hash-table-ref pub "g"
                                                )
                                              )
                                            )
                                             ", "
                                          )
                                           (
                                            to-str-space (
                                              hash-table-ref pub "e2"
                                            )
                                          )
                                        )
                                         ", "
                                      )
                                       (
                                        to-str-space (
                                          hash-table-ref pub "p"
                                        )
                                      )
                                    )
                                     ")"
                                  )
                                   (
                                    to-str (
                                      string-append (
                                        string-append (
                                          string-append (
                                            string-append (
                                              string-append (
                                                string-append (
                                                  string-append (
                                                    string-append "public key: (" (
                                                      to-str-space (
                                                        hash-table-ref pub "key_size"
                                                      )
                                                    )
                                                  )
                                                   ", "
                                                )
                                                 (
                                                  to-str-space (
                                                    hash-table-ref pub "g"
                                                  )
                                                )
                                              )
                                               ", "
                                            )
                                             (
                                              to-str-space (
                                                hash-table-ref pub "e2"
                                              )
                                            )
                                          )
                                           ", "
                                        )
                                         (
                                          to-str-space (
                                            hash-table-ref pub "p"
                                          )
                                        )
                                      )
                                       ")"
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
                                      string-append (
                                        string-append (
                                          string-append (
                                            string-append "private key: (" (
                                              to-str-space (
                                                hash-table-ref priv "key_size"
                                              )
                                            )
                                          )
                                           ", "
                                        )
                                         (
                                          to-str-space (
                                            hash-table-ref priv "d"
                                          )
                                        )
                                      )
                                       ")"
                                    )
                                  )
                                   (
                                    string-append (
                                      string-append (
                                        string-append (
                                          string-append "private key: (" (
                                            to-str-space (
                                              hash-table-ref priv "key_size"
                                            )
                                          )
                                        )
                                         ", "
                                      )
                                       (
                                        to-str-space (
                                          hash-table-ref priv "d"
                                        )
                                      )
                                    )
                                     ")"
                                  )
                                   (
                                    to-str (
                                      string-append (
                                        string-append (
                                          string-append (
                                            string-append "private key: (" (
                                              to-str-space (
                                                hash-table-ref priv "key_size"
                                              )
                                            )
                                          )
                                           ", "
                                        )
                                         (
                                          to-str-space (
                                            hash-table-ref priv "d"
                                          )
                                        )
                                      )
                                       ")"
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
