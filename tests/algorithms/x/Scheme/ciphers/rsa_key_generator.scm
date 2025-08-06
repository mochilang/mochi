;; Generated on 2025-08-06 23:15 +0700
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
        pow2 exp
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                res 1
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
                                    set! res (
                                      * res 2
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
                    ret1 res
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
          seed 1
        )
      )
       (
        begin (
          define (
            next_seed x
          )
           (
            call/cc (
              lambda (
                ret4
              )
               (
                ret4 (
                  _mod (
                    + (
                      * x 1103515245
                    )
                     12345
                  )
                   2147483648
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
                ret5
              )
               (
                begin (
                  set! seed (
                    next_seed seed
                  )
                )
                 (
                  ret5 (
                    + min (
                      _mod seed (
                        - max min
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
            gcd a b
          )
           (
            call/cc (
              lambda (
                ret6
              )
               (
                let (
                  (
                    x a
                  )
                )
                 (
                  begin (
                    let (
                      (
                        y b
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
                                      not (
                                        equal? y 0
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            temp (
                                              _mod x y
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! x y
                                          )
                                           (
                                            set! y temp
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
                        ret6 x
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
            mod_inverse e phi
          )
           (
            call/cc (
              lambda (
                ret9
              )
               (
                let (
                  (
                    t 0
                  )
                )
                 (
                  begin (
                    let (
                      (
                        newt 1
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            r phi
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                newr e
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
                                              not (
                                                equal? newr 0
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    quotient (
                                                      _div r newr
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        tmp newt
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! newt (
                                                          - t (
                                                            * quotient newt
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! t tmp
                                                      )
                                                       (
                                                        let (
                                                          (
                                                            tmp_r newr
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! newr (
                                                              - r (
                                                                * quotient newr
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! r tmp_r
                                                          )
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
                                if (
                                  > r 1
                                )
                                 (
                                  begin (
                                    ret9 0
                                  )
                                )
                                 (
                                  quote (
                                    
                                  )
                                )
                              )
                               (
                                if (
                                  < t 0
                                )
                                 (
                                  begin (
                                    set! t (
                                      + t phi
                                    )
                                  )
                                )
                                 (
                                  quote (
                                    
                                  )
                                )
                              )
                               (
                                ret9 t
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
            is_prime n
          )
           (
            call/cc (
              lambda (
                ret12
              )
               (
                begin (
                  if (
                    < n 2
                  )
                   (
                    begin (
                      ret12 #f
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
                      i 2
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
                                    <= (
                                      * i i
                                    )
                                     n
                                  )
                                   (
                                    begin (
                                      if (
                                        equal? (
                                          _mod n i
                                        )
                                         0
                                      )
                                       (
                                        begin (
                                          ret12 #f
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
                      ret12 #t
                    )
                  )
                )
              )
            )
          )
        )
         (
          define (
            generate_prime bits
          )
           (
            call/cc (
              lambda (
                ret15
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
                          pow2 bits
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
                                _mod p 2
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
                                          not (
                                            is_prime p
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
                                              _ge p max
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
                            ret15 p
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
            generate_key bits
          )
           (
            call/cc (
              lambda (
                ret18
              )
               (
                let (
                  (
                    p (
                      generate_prime bits
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        q (
                          generate_prime bits
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            n (
                              * p q
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                phi (
                                  * (
                                    - p 1
                                  )
                                   (
                                    - q 1
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    e (
                                      rand_range 2 phi
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
                                                if (
                                                  not (
                                                    equal? (
                                                      gcd e phi
                                                    )
                                                     1
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! e (
                                                      _add e 1
                                                    )
                                                  )
                                                   (
                                                    if (
                                                      _ge e phi
                                                    )
                                                     (
                                                      begin (
                                                        set! e 2
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
                                    let (
                                      (
                                        d (
                                          mod_inverse e phi
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        ret18 (
                                          alist->hash-table (
                                            _list (
                                              cons "public_key" (
                                                _list n e
                                              )
                                            )
                                             (
                                              cons "private_key" (
                                                _list n d
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
          let (
            (
              keys (
                generate_key 8
              )
            )
          )
           (
            begin (
              let (
                (
                  pub (
                    hash-table-ref keys "public_key"
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      priv (
                        hash-table-ref keys "private_key"
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
                                  string-append "Public key: (" (
                                    to-str-space (
                                      cond (
                                        (
                                          string? pub
                                        )
                                         (
                                          _substring pub 0 (
                                            + 0 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? pub
                                        )
                                         (
                                          hash-table-ref pub 0
                                        )
                                      )
                                       (
                                        else (
                                          list-ref pub 0
                                        )
                                      )
                                    )
                                  )
                                )
                                 ", "
                              )
                               (
                                to-str-space (
                                  cond (
                                    (
                                      string? pub
                                    )
                                     (
                                      _substring pub 1 (
                                        + 1 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? pub
                                    )
                                     (
                                      hash-table-ref pub 1
                                    )
                                  )
                                   (
                                    else (
                                      list-ref pub 1
                                    )
                                  )
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
                                string-append "Public key: (" (
                                  to-str-space (
                                    cond (
                                      (
                                        string? pub
                                      )
                                       (
                                        _substring pub 0 (
                                          + 0 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? pub
                                      )
                                       (
                                        hash-table-ref pub 0
                                      )
                                    )
                                     (
                                      else (
                                        list-ref pub 0
                                      )
                                    )
                                  )
                                )
                              )
                               ", "
                            )
                             (
                              to-str-space (
                                cond (
                                  (
                                    string? pub
                                  )
                                   (
                                    _substring pub 1 (
                                      + 1 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? pub
                                  )
                                   (
                                    hash-table-ref pub 1
                                  )
                                )
                                 (
                                  else (
                                    list-ref pub 1
                                  )
                                )
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
                                  string-append "Public key: (" (
                                    to-str-space (
                                      cond (
                                        (
                                          string? pub
                                        )
                                         (
                                          _substring pub 0 (
                                            + 0 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? pub
                                        )
                                         (
                                          hash-table-ref pub 0
                                        )
                                      )
                                       (
                                        else (
                                          list-ref pub 0
                                        )
                                      )
                                    )
                                  )
                                )
                                 ", "
                              )
                               (
                                to-str-space (
                                  cond (
                                    (
                                      string? pub
                                    )
                                     (
                                      _substring pub 1 (
                                        + 1 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? pub
                                    )
                                     (
                                      hash-table-ref pub 1
                                    )
                                  )
                                   (
                                    else (
                                      list-ref pub 1
                                    )
                                  )
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
                                  string-append "Private key: (" (
                                    to-str-space (
                                      cond (
                                        (
                                          string? priv
                                        )
                                         (
                                          _substring priv 0 (
                                            + 0 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? priv
                                        )
                                         (
                                          hash-table-ref priv 0
                                        )
                                      )
                                       (
                                        else (
                                          list-ref priv 0
                                        )
                                      )
                                    )
                                  )
                                )
                                 ", "
                              )
                               (
                                to-str-space (
                                  cond (
                                    (
                                      string? priv
                                    )
                                     (
                                      _substring priv 1 (
                                        + 1 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? priv
                                    )
                                     (
                                      hash-table-ref priv 1
                                    )
                                  )
                                   (
                                    else (
                                      list-ref priv 1
                                    )
                                  )
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
                                string-append "Private key: (" (
                                  to-str-space (
                                    cond (
                                      (
                                        string? priv
                                      )
                                       (
                                        _substring priv 0 (
                                          + 0 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? priv
                                      )
                                       (
                                        hash-table-ref priv 0
                                      )
                                    )
                                     (
                                      else (
                                        list-ref priv 0
                                      )
                                    )
                                  )
                                )
                              )
                               ", "
                            )
                             (
                              to-str-space (
                                cond (
                                  (
                                    string? priv
                                  )
                                   (
                                    _substring priv 1 (
                                      + 1 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? priv
                                  )
                                   (
                                    hash-table-ref priv 1
                                  )
                                )
                                 (
                                  else (
                                    list-ref priv 1
                                  )
                                )
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
                                  string-append "Private key: (" (
                                    to-str-space (
                                      cond (
                                        (
                                          string? priv
                                        )
                                         (
                                          _substring priv 0 (
                                            + 0 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? priv
                                        )
                                         (
                                          hash-table-ref priv 0
                                        )
                                      )
                                       (
                                        else (
                                          list-ref priv 0
                                        )
                                      )
                                    )
                                  )
                                )
                                 ", "
                              )
                               (
                                to-str-space (
                                  cond (
                                    (
                                      string? priv
                                    )
                                     (
                                      _substring priv 1 (
                                        + 1 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? priv
                                    )
                                     (
                                      hash-table-ref priv 1
                                    )
                                  )
                                   (
                                    else (
                                      list-ref priv 1
                                    )
                                  )
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
