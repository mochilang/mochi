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
      start28 (
        current-jiffy
      )
    )
     (
      jps31 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        index_in_string s ch
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
        contains_char s ch
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            ret4 (
              _ge (
                index_in_string s ch
              )
               0
            )
          )
        )
      )
    )
     (
      define (
        is_alpha ch
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                lower "abcdefghijklmnopqrstuvwxyz"
              )
            )
             (
              begin (
                let (
                  (
                    upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                  )
                )
                 (
                  begin (
                    ret5 (
                      or (
                        contains_char lower ch
                      )
                       (
                        contains_char upper ch
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
        to_upper s
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                lower "abcdefghijklmnopqrstuvwxyz"
              )
            )
             (
              begin (
                let (
                  (
                    upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                  )
                )
                 (
                  begin (
                    let (
                      (
                        res ""
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
                                            _len s
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                ch (
                                                  _substring s i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    idx (
                                                      index_in_string lower ch
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
                                                        set! res (
                                                          string-append res (
                                                            _substring upper idx (
                                                              + idx 1
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! res (
                                                          string-append res ch
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
                            ret6 res
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
        remove_duplicates key
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                res ""
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
                                  < i (
                                    _len key
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        ch (
                                          _substring key i (
                                            + i 1
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          or (
                                            string=? ch " "
                                          )
                                           (
                                            and (
                                              is_alpha ch
                                            )
                                             (
                                              eq? (
                                                contains_char res ch
                                              )
                                               #f
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! res (
                                              string-append res ch
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
                    ret9 res
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
        create_cipher_map key
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              )
            )
             (
              begin (
                let (
                  (
                    cleaned (
                      remove_duplicates (
                        to_upper key
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        cipher (
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
                                          < i (
                                            _len cleaned
                                          )
                                        )
                                         (
                                          begin (
                                            set! cipher (
                                              append cipher (
                                                _list (
                                                  cond (
                                                    (
                                                      string? cleaned
                                                    )
                                                     (
                                                      _substring cleaned i (
                                                        + i 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? cleaned
                                                    )
                                                     (
                                                      hash-table-ref cleaned i
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref cleaned i
                                                    )
                                                  )
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
                            let (
                              (
                                offset (
                                  _len cleaned
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    j (
                                      _len cipher
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    call/cc (
                                      lambda (
                                        break16
                                      )
                                       (
                                        letrec (
                                          (
                                            loop15 (
                                              lambda (
                                                
                                              )
                                               (
                                                if (
                                                  < j 26
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        char (
                                                          _substring alphabet (
                                                            - j offset
                                                          )
                                                           (
                                                            + (
                                                              - j offset
                                                            )
                                                             1
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        call/cc (
                                                          lambda (
                                                            break18
                                                          )
                                                           (
                                                            letrec (
                                                              (
                                                                loop17 (
                                                                  lambda (
                                                                    
                                                                  )
                                                                   (
                                                                    if (
                                                                      contains_char cleaned char
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! offset (
                                                                          - offset 1
                                                                        )
                                                                      )
                                                                       (
                                                                        set! char (
                                                                          _substring alphabet (
                                                                            - j offset
                                                                          )
                                                                           (
                                                                            + (
                                                                              - j offset
                                                                            )
                                                                             1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        loop17
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
                                                              loop17
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! cipher (
                                                          append cipher (
                                                            _list char
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
                                                    loop15
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
                                          loop15
                                        )
                                      )
                                    )
                                  )
                                   (
                                    ret12 cipher
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
        index_in_list lst ch
      )
       (
        call/cc (
          lambda (
            ret19
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
                    break21
                  )
                   (
                    letrec (
                      (
                        loop20 (
                          lambda (
                            
                          )
                           (
                            if (
                              < i (
                                _len lst
                              )
                            )
                             (
                              begin (
                                if (
                                  string=? (
                                    list-ref lst i
                                  )
                                   ch
                                )
                                 (
                                  begin (
                                    ret19 i
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
                                loop20
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
                      loop20
                    )
                  )
                )
              )
               (
                ret19 (
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
        encipher message cipher
      )
       (
        call/cc (
          lambda (
            ret22
          )
           (
            let (
              (
                alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              )
            )
             (
              begin (
                let (
                  (
                    msg (
                      to_upper message
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        res ""
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
                                          < i (
                                            _len msg
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                ch (
                                                  cond (
                                                    (
                                                      string? msg
                                                    )
                                                     (
                                                      _substring msg i (
                                                        + i 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? msg
                                                    )
                                                     (
                                                      hash-table-ref msg i
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref msg i
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    idx (
                                                      index_in_string alphabet ch
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
                                                        set! res (
                                                          string-append res (
                                                            list-ref cipher idx
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! res (
                                                          string-append res ch
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
                            ret22 res
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
        decipher message cipher
      )
       (
        call/cc (
          lambda (
            ret25
          )
           (
            let (
              (
                alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              )
            )
             (
              begin (
                let (
                  (
                    msg (
                      to_upper message
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        res ""
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
                                          < i (
                                            _len msg
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                ch (
                                                  cond (
                                                    (
                                                      string? msg
                                                    )
                                                     (
                                                      _substring msg i (
                                                        + i 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? msg
                                                    )
                                                     (
                                                      hash-table-ref msg i
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref msg i
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    idx (
                                                      index_in_list cipher ch
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
                                                        set! res (
                                                          string-append res (
                                                            _substring alphabet idx (
                                                              + idx 1
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! res (
                                                          string-append res ch
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
                            ret25 res
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
          cipher_map (
            create_cipher_map "Goodbye!!"
          )
        )
      )
       (
        begin (
          let (
            (
              encoded (
                encipher "Hello World!!" cipher_map
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? encoded
                )
                 encoded (
                  to-str encoded
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
                    decipher encoded cipher_map
                  )
                )
                 (
                  decipher encoded cipher_map
                )
                 (
                  to-str (
                    decipher encoded cipher_map
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
      let (
        (
          end29 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur30 (
              quotient (
                * (
                  - end29 start28
                )
                 1000000
              )
               jps31
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur30
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
