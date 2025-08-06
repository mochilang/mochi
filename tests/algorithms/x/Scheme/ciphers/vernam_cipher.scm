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
      start16 (
        current-jiffy
      )
    )
     (
      jps19 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        indexOf s ch
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
                                _len s
                              )
                            )
                             (
                              begin (
                                if (
                                  string=? (
                                    _substring s i (
                                      + i 1
                                    )
                                  )
                                   ch
                                )
                                 (
                                  begin (
                                    ret1 i
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
                ret1 (
                  - 1
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        ord ch
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              )
            )
             (
              begin (
                let (
                  (
                    idx (
                      indexOf upper ch
                    )
                  )
                )
                 (
                  begin (
                    if (
                      _ge idx 0
                    )
                     (
                      begin (
                        ret4 (
                          _add 65 idx
                        )
                      )
                    )
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
                    ret4 0
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
        chr n
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              )
            )
             (
              begin (
                if (
                  and (
                    >= n 65
                  )
                   (
                    < n 91
                  )
                )
                 (
                  begin (
                    ret5 (
                      _substring upper (
                        - n 65
                      )
                       (
                        - n 64
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
                ret5 "?"
              )
            )
          )
        )
      )
    )
     (
      define (
        vernam_encrypt plaintext key
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                ciphertext ""
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
                                  < i (
                                    _len plaintext
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        p (
                                          - (
                                            ord (
                                              _substring plaintext i (
                                                + i 1
                                              )
                                            )
                                          )
                                           65
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            k (
                                              - (
                                                ord (
                                                  _substring key (
                                                    _mod i (
                                                      _len key
                                                    )
                                                  )
                                                   (
                                                    + (
                                                      _mod i (
                                                        _len key
                                                      )
                                                    )
                                                     1
                                                  )
                                                )
                                              )
                                               65
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                ct (
                                                  _add p k
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
                                                              _gt ct 25
                                                            )
                                                             (
                                                              begin (
                                                                set! ct (
                                                                  - ct 26
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
                                                set! ciphertext (
                                                  string-append ciphertext (
                                                    chr (
                                                      _add ct 65
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
                                        )
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
                    ret6 ciphertext
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
        vernam_decrypt ciphertext key
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
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
                                  < i (
                                    _len ciphertext
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        c (
                                          ord (
                                            _substring ciphertext i (
                                              + i 1
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            k (
                                              ord (
                                                _substring key (
                                                  _mod i (
                                                    _len key
                                                  )
                                                )
                                                 (
                                                  + (
                                                    _mod i (
                                                      _len key
                                                    )
                                                  )
                                                   1
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                val (
                                                  - c k
                                                )
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
                                                              _lt val 0
                                                            )
                                                             (
                                                              begin (
                                                                set! val (
                                                                  _add val 26
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
                                                set! decrypted (
                                                  string-append decrypted (
                                                    chr (
                                                      _add val 65
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
                                        )
                                      )
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
                    ret11 decrypted
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
          plaintext "HELLO"
        )
      )
       (
        begin (
          let (
            (
              key "KEY"
            )
          )
           (
            begin (
              let (
                (
                  encrypted (
                    vernam_encrypt plaintext key
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      decrypted (
                        vernam_decrypt encrypted key
                      )
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            string-append "Plaintext: " plaintext
                          )
                        )
                         (
                          string-append "Plaintext: " plaintext
                        )
                         (
                          to-str (
                            string-append "Plaintext: " plaintext
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
     (
      let (
        (
          end17 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur18 (
              quotient (
                * (
                  - end17 start16
                )
                 1000000
              )
               jps19
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur18
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
