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
      start17 (
        current-jiffy
      )
    )
     (
      jps20 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          encode_map (
            alist->hash-table (
              _list (
                cons "a" "AAAAA"
              )
               (
                cons "b" "AAAAB"
              )
               (
                cons "c" "AAABA"
              )
               (
                cons "d" "AAABB"
              )
               (
                cons "e" "AABAA"
              )
               (
                cons "f" "AABAB"
              )
               (
                cons "g" "AABBA"
              )
               (
                cons "h" "AABBB"
              )
               (
                cons "i" "ABAAA"
              )
               (
                cons "j" "BBBAA"
              )
               (
                cons "k" "ABAAB"
              )
               (
                cons "l" "ABABA"
              )
               (
                cons "m" "ABABB"
              )
               (
                cons "n" "ABBAA"
              )
               (
                cons "o" "ABBAB"
              )
               (
                cons "p" "ABBBA"
              )
               (
                cons "q" "ABBBB"
              )
               (
                cons "r" "BAAAA"
              )
               (
                cons "s" "BAAAB"
              )
               (
                cons "t" "BAABA"
              )
               (
                cons "u" "BAABB"
              )
               (
                cons "v" "BBBAB"
              )
               (
                cons "w" "BABAA"
              )
               (
                cons "x" "BABAB"
              )
               (
                cons "y" "BABBA"
              )
               (
                cons "z" "BABBB"
              )
               (
                cons " " " "
              )
            )
          )
        )
      )
       (
        begin (
          define (
            make_decode_map
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                let (
                  (
                    m (
                      alist->hash-table (
                        _list
                      )
                    )
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
                                xs
                              )
                               (
                                if (
                                  null? xs
                                )
                                 (
                                  quote (
                                    
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        k (
                                          car xs
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        hash-table-set! m (
                                          hash-table-ref/default encode_map k (
                                            quote (
                                              
                                            )
                                          )
                                        )
                                         k
                                      )
                                    )
                                  )
                                   (
                                    loop2 (
                                      cdr xs
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          loop2 (
                            hash-table-keys encode_map
                          )
                        )
                      )
                    )
                  )
                   (
                    ret1 m
                  )
                )
              )
            )
          )
        )
         (
          let (
            (
              decode_map (
                make_decode_map
              )
            )
          )
           (
            begin (
              define (
                split_spaces s
              )
               (
                call/cc (
                  lambda (
                    ret4
                  )
                   (
                    let (
                      (
                        parts (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            current ""
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
                                                    if (
                                                      string=? ch " "
                                                    )
                                                     (
                                                      begin (
                                                        set! parts (
                                                          append parts (
                                                            _list current
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! current ""
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! current (
                                                          string-append current ch
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
                                set! parts (
                                  append parts (
                                    _list current
                                  )
                                )
                              )
                               (
                                ret4 parts
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
                encode word
              )
               (
                call/cc (
                  lambda (
                    ret7
                  )
                   (
                    let (
                      (
                        w (
                          lower word
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            encoded ""
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
                                              < i (
                                                _len w
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    ch (
                                                      _substring w i (
                                                        + i 1
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      cond (
                                                        (
                                                          string? encode_map
                                                        )
                                                         (
                                                          if (
                                                            string-contains encode_map ch
                                                          )
                                                           #t #f
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? encode_map
                                                        )
                                                         (
                                                          if (
                                                            hash-table-exists? encode_map ch
                                                          )
                                                           #t #f
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          if (
                                                            member ch encode_map
                                                          )
                                                           #t #f
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! encoded (
                                                          string-append encoded (
                                                            hash-table-ref/default encode_map ch (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        panic "encode() accepts only letters of the alphabet and spaces"
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
                                ret7 encoded
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
                decode coded
              )
               (
                call/cc (
                  lambda (
                    ret10
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
                                        _len coded
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            ch (
                                              _substring coded i (
                                                + i 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              and (
                                                and (
                                                  not (
                                                    string=? ch "A"
                                                  )
                                                )
                                                 (
                                                  not (
                                                    string=? ch "B"
                                                  )
                                                )
                                              )
                                               (
                                                not (
                                                  string=? ch " "
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                panic "decode() accepts only 'A', 'B' and spaces"
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
                        let (
                          (
                            words (
                              split_spaces coded
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                decoded ""
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    w 0
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
                                                  < w (
                                                    _len words
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        word (
                                                          cond (
                                                            (
                                                              string? words
                                                            )
                                                             (
                                                              _substring words w (
                                                                + w 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? words
                                                            )
                                                             (
                                                              hash-table-ref words w
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref words w
                                                            )
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
                                                                          < j (
                                                                            _len word
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                segment (
                                                                                  _substring word j (
                                                                                    + j 5
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! decoded (
                                                                                  string-append decoded (
                                                                                    hash-table-ref/default decode_map segment (
                                                                                      quote (
                                                                                        
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! j (
                                                                                  + j 5
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
                                                            if (
                                                              < w (
                                                                - (
                                                                  _len words
                                                                )
                                                                 1
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! decoded (
                                                                  string-append decoded " "
                                                                )
                                                              )
                                                            )
                                                             (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! w (
                                                              + w 1
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
                                    ret10 decoded
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
                    encode "hello"
                  )
                )
                 (
                  encode "hello"
                )
                 (
                  to-str (
                    encode "hello"
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
                    encode "hello world"
                  )
                )
                 (
                  encode "hello world"
                )
                 (
                  to-str (
                    encode "hello world"
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
                    decode "AABBBAABAAABABAABABAABBAB BABAAABBABBAAAAABABAAAABB"
                  )
                )
                 (
                  decode "AABBBAABAAABABAABABAABBAB BABAAABBABBAAAAABABAAAABB"
                )
                 (
                  to-str (
                    decode "AABBBAABAAABABAABABAABBAB BABAAABBABBAAAAABABAAAABB"
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
                    decode "AABBBAABAAABABAABABAABBAB"
                  )
                )
                 (
                  decode "AABBBAABAAABABAABABAABBAB"
                )
                 (
                  to-str (
                    decode "AABBBAABAAABABAABABAABBAB"
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
          end18 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur19 (
              quotient (
                * (
                  - end18 start17
                )
                 1000000
              )
               jps20
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur19
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
