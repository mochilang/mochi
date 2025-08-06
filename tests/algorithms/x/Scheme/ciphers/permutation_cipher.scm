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
      start25 (
        current-jiffy
      )
    )
     (
      jps28 (
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
            rand max
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
                  ret1 (
                    modulo seed max
                  )
                )
              )
            )
          )
        )
         (
          define (
            generate_valid_block_size message_length
          )
           (
            call/cc (
              lambda (
                ret2
              )
               (
                let (
                  (
                    factors (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        i 2
                      )
                    )
                     (
                      begin (
                        call/cc (
                          lambda (
                            break4
                          )
                           (
                            letrec (
                              (
                                loop3 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      <= i message_length
                                    )
                                     (
                                      begin (
                                        if (
                                          equal? (
                                            modulo message_length i
                                          )
                                           0
                                        )
                                         (
                                          begin (
                                            set! factors (
                                              append factors (
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
                                        set! i (
                                          + i 1
                                        )
                                      )
                                       (
                                        loop3
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
                              loop3
                            )
                          )
                        )
                      )
                       (
                        let (
                          (
                            idx (
                              rand (
                                _len factors
                              )
                            )
                          )
                        )
                         (
                          begin (
                            ret2 (
                              list-ref factors idx
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
            generate_permutation_key block_size
          )
           (
            call/cc (
              lambda (
                ret5
              )
               (
                let (
                  (
                    digits (
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
                                      < i block_size
                                    )
                                     (
                                      begin (
                                        set! digits (
                                          append digits (
                                            _list i
                                          )
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
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
                        let (
                          (
                            j (
                              - block_size 1
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
                                          > j 0
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                k (
                                                  rand (
                                                    + j 1
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    temp (
                                                      list-ref digits j
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    list-set! digits j (
                                                      list-ref digits k
                                                    )
                                                  )
                                                   (
                                                    list-set! digits k temp
                                                  )
                                                   (
                                                    set! j (
                                                      - j 1
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
                            ret5 digits
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
            encrypt message key block_size
          )
           (
            call/cc (
              lambda (
                ret10
              )
               (
                let (
                  (
                    encrypted ""
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
                                        _len message
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            block (
                                              _substring message i (
                                                + i block_size
                                              )
                                            )
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
                                                              < j block_size
                                                            )
                                                             (
                                                              begin (
                                                                set! encrypted (
                                                                  string-append encrypted (
                                                                    _substring block (
                                                                      list-ref key j
                                                                    )
                                                                     (
                                                                      + (
                                                                        list-ref key j
                                                                      )
                                                                       1
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! j (
                                                                  + j 1
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
                                                set! i (
                                                  + i block_size
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
                        ret10 encrypted
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
            repeat_string times
          )
           (
            call/cc (
              lambda (
                ret15
              )
               (
                let (
                  (
                    res (
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
                                      < i times
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list ""
                                          )
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
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
                        ret15 res
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
            decrypt encrypted key
          )
           (
            call/cc (
              lambda (
                ret18
              )
               (
                let (
                  (
                    klen (
                      _len key
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        decrypted ""
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
                                          < i (
                                            _len encrypted
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                block (
                                                  _substring encrypted i (
                                                    + i klen
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    original (
                                                      repeat_string klen
                                                    )
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
                                                                      < j klen
                                                                    )
                                                                     (
                                                                      begin (
                                                                        list-set! original (
                                                                          list-ref key j
                                                                        )
                                                                         (
                                                                          _substring block j (
                                                                            + j 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        set! j (
                                                                          + j 1
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
                                                        set! j 0
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
                                                                      < j klen
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! decrypted (
                                                                          string-append decrypted (
                                                                            cond (
                                                                              (
                                                                                string? original
                                                                              )
                                                                               (
                                                                                _substring original j (
                                                                                  + j 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? original
                                                                              )
                                                                               (
                                                                                hash-table-ref original j
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref original j
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        set! j (
                                                                          + j 1
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
                                                        set! i (
                                                          + i klen
                                                        )
                                                      )
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
                            ret18 decrypted
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
              message "HELLO WORLD"
            )
          )
           (
            begin (
              let (
                (
                  block_size (
                    generate_valid_block_size (
                      _len message
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      key (
                        generate_permutation_key block_size
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          encrypted (
                            encrypt message key block_size
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              decrypted (
                                decrypt encrypted key
                              )
                            )
                          )
                           (
                            begin (
                              _display (
                                if (
                                  string? (
                                    string-append "Block size: " (
                                      to-str-space block_size
                                    )
                                  )
                                )
                                 (
                                  string-append "Block size: " (
                                    to-str-space block_size
                                  )
                                )
                                 (
                                  to-str (
                                    string-append "Block size: " (
                                      to-str-space block_size
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
                                    string-append "Key: " (
                                      to-str-space key
                                    )
                                  )
                                )
                                 (
                                  string-append "Key: " (
                                    to-str-space key
                                  )
                                )
                                 (
                                  to-str (
                                    string-append "Key: " (
                                      to-str-space key
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
                                    string-append "Encrypted: " encrypted
                                  )
                                )
                                 (
                                  string-append "Encrypted: " encrypted
                                )
                                 (
                                  to-str (
                                    string-append "Encrypted: " encrypted
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
                                    string-append "Decrypted: " decrypted
                                  )
                                )
                                 (
                                  string-append "Decrypted: " decrypted
                                )
                                 (
                                  to-str (
                                    string-append "Decrypted: " decrypted
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
    )
     (
      let (
        (
          end26 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur27 (
              quotient (
                * (
                  - end26 start25
                )
                 1000000
              )
               jps28
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur27
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
