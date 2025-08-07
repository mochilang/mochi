;; Generated on 2025-08-07 14:57 +0700
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
      start54 (
        current-jiffy
      )
    )
     (
      jps57 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          MOD 4294967296
        )
      )
       (
        begin (
          let (
            (
              ASCII " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
            )
          )
           (
            begin (
              define (
                ord ch
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
                                        _len ASCII
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          string=? (
                                            _substring ASCII i (
                                              + i 1
                                            )
                                          )
                                           ch
                                        )
                                         (
                                          begin (
                                            ret1 (
                                              + 32 i
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
                        ret1 0
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                to_little_endian s
              )
               (
                call/cc (
                  lambda (
                    ret4
                  )
                   (
                    begin (
                      if (
                        not (
                          equal? (
                            _len s
                          )
                           32
                        )
                      )
                       (
                        begin (
                          panic "Input must be of length 32"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret4 (
                        string-append (
                          string-append (
                            string-append (
                              _substring s 24 32
                            )
                             (
                              _substring s 16 24
                            )
                          )
                           (
                            _substring s 8 16
                          )
                        )
                         (
                          _substring s 0 8
                        )
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                int_to_bits n width
              )
               (
                call/cc (
                  lambda (
                    ret5
                  )
                   (
                    let (
                      (
                        bits ""
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            num n
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
                                          > num 0
                                        )
                                         (
                                          begin (
                                            set! bits (
                                              string-append (
                                                to-str-space (
                                                  _mod num 2
                                                )
                                              )
                                               bits
                                            )
                                          )
                                           (
                                            set! num (
                                              _div num 2
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
                                          < (
                                            _len bits
                                          )
                                           width
                                        )
                                         (
                                          begin (
                                            set! bits (
                                              string-append "0" bits
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
                            if (
                              > (
                                _len bits
                              )
                               width
                            )
                             (
                              begin (
                                set! bits (
                                  _substring bits (
                                    - (
                                      _len bits
                                    )
                                     width
                                  )
                                   (
                                    _len bits
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
                            ret5 bits
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
                bits_to_int bits
              )
               (
                call/cc (
                  lambda (
                    ret10
                  )
                   (
                    let (
                      (
                        num 0
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
                                            _len bits
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              string=? (
                                                _substring bits i (
                                                  + i 1
                                                )
                                              )
                                               "1"
                                            )
                                             (
                                              begin (
                                                set! num (
                                                  + (
                                                    * num 2
                                                  )
                                                   1
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! num (
                                                  * num 2
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
                            ret10 num
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
                to_hex n
              )
               (
                call/cc (
                  lambda (
                    ret13
                  )
                   (
                    let (
                      (
                        digits "0123456789abcdef"
                      )
                    )
                     (
                      begin (
                        if (
                          equal? n 0
                        )
                         (
                          begin (
                            ret13 "0"
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
                            num n
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                s ""
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
                                              > num 0
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    d (
                                                      _mod num 16
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! s (
                                                      string-append (
                                                        _substring digits d (
                                                          + d 1
                                                        )
                                                      )
                                                       s
                                                    )
                                                  )
                                                   (
                                                    set! num (
                                                      _div num 16
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
                                ret13 s
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
                reformat_hex i
              )
               (
                call/cc (
                  lambda (
                    ret16
                  )
                   (
                    begin (
                      if (
                        < i 0
                      )
                       (
                        begin (
                          panic "Input must be non-negative"
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
                          hex (
                            to_hex i
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
                                        < (
                                          _len hex
                                        )
                                         8
                                      )
                                       (
                                        begin (
                                          set! hex (
                                            string-append "0" hex
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
                          if (
                            > (
                              _len hex
                            )
                             8
                          )
                           (
                            begin (
                              set! hex (
                                if (
                                  string? hex
                                )
                                 (
                                  _substring hex (
                                    - (
                                      _len hex
                                    )
                                     8
                                  )
                                   (
                                    _len hex
                                  )
                                )
                                 (
                                  take (
                                    drop hex (
                                      - (
                                        _len hex
                                      )
                                       8
                                    )
                                  )
                                   (
                                    - (
                                      _len hex
                                    )
                                     (
                                      - (
                                        _len hex
                                      )
                                       8
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
                              le ""
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  j (
                                    - (
                                      _len hex
                                    )
                                     2
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
                                                >= j 0
                                              )
                                               (
                                                begin (
                                                  set! le (
                                                    string-append le (
                                                      if (
                                                        string? hex
                                                      )
                                                       (
                                                        _substring hex j (
                                                          + j 2
                                                        )
                                                      )
                                                       (
                                                        take (
                                                          drop hex j
                                                        )
                                                         (
                                                          - (
                                                            + j 2
                                                          )
                                                           j
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  set! j (
                                                    - j 2
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
                                  ret16 le
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
                preprocess message
              )
               (
                call/cc (
                  lambda (
                    ret21
                  )
                   (
                    let (
                      (
                        bit_string ""
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
                                break23
                              )
                               (
                                letrec (
                                  (
                                    loop22 (
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
                                                ch (
                                                  _substring message i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! bit_string (
                                                  string-append bit_string (
                                                    int_to_bits (
                                                      ord ch
                                                    )
                                                     8
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
                           (
                            let (
                              (
                                start_len (
                                  int_to_bits (
                                    _len bit_string
                                  )
                                   64
                                )
                              )
                            )
                             (
                              begin (
                                set! bit_string (
                                  string-append bit_string "1"
                                )
                              )
                               (
                                call/cc (
                                  lambda (
                                    break25
                                  )
                                   (
                                    letrec (
                                      (
                                        loop24 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              not (
                                                equal? (
                                                  _mod (
                                                    _len bit_string
                                                  )
                                                   512
                                                )
                                                 448
                                              )
                                            )
                                             (
                                              begin (
                                                set! bit_string (
                                                  string-append bit_string "0"
                                                )
                                              )
                                               (
                                                loop24
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
                                      loop24
                                    )
                                  )
                                )
                              )
                               (
                                set! bit_string (
                                  string-append (
                                    string-append bit_string (
                                      to_little_endian (
                                        if (
                                          string? start_len
                                        )
                                         (
                                          _substring start_len 32 64
                                        )
                                         (
                                          take (
                                            drop start_len 32
                                          )
                                           (
                                            - 64 32
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    to_little_endian (
                                      if (
                                        string? start_len
                                      )
                                       (
                                        _substring start_len 0 32
                                      )
                                       (
                                        take (
                                          drop start_len 0
                                        )
                                         (
                                          - 32 0
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                ret21 bit_string
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
                get_block_words bit_string
              )
               (
                call/cc (
                  lambda (
                    ret26
                  )
                   (
                    begin (
                      if (
                        not (
                          equal? (
                            _mod (
                              _len bit_string
                            )
                             512
                          )
                           0
                        )
                      )
                       (
                        begin (
                          panic "Input must have length that's a multiple of 512"
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
                          blocks (
                            _list
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              pos 0
                            )
                          )
                           (
                            begin (
                              call/cc (
                                lambda (
                                  break28
                                )
                                 (
                                  letrec (
                                    (
                                      loop27 (
                                        lambda (
                                          
                                        )
                                         (
                                          if (
                                            < pos (
                                              _len bit_string
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  block (
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
                                                          break30
                                                        )
                                                         (
                                                          letrec (
                                                            (
                                                              loop29 (
                                                                lambda (
                                                                  
                                                                )
                                                                 (
                                                                  if (
                                                                    < i 512
                                                                  )
                                                                   (
                                                                    begin (
                                                                      let (
                                                                        (
                                                                          part (
                                                                            _substring bit_string (
                                                                              + pos i
                                                                            )
                                                                             (
                                                                              + (
                                                                                + pos i
                                                                              )
                                                                               32
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          let (
                                                                            (
                                                                              word (
                                                                                bits_to_int (
                                                                                  to_little_endian part
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              set! block (
                                                                                append block (
                                                                                  _list word
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              set! i (
                                                                                + i 32
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      loop29
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
                                                            loop29
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      set! blocks (
                                                        append blocks (
                                                          _list block
                                                        )
                                                      )
                                                    )
                                                     (
                                                      set! pos (
                                                        + pos 512
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              loop27
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
                                    loop27
                                  )
                                )
                              )
                            )
                             (
                              ret26 blocks
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
                bit_and a b
              )
               (
                call/cc (
                  lambda (
                    ret31
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
                            let (
                              (
                                res 0
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    bit 1
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
                                            break33
                                          )
                                           (
                                            letrec (
                                              (
                                                loop32 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < i 32
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          and (
                                                            equal? (
                                                              _mod x 2
                                                            )
                                                             1
                                                          )
                                                           (
                                                            equal? (
                                                              _mod y 2
                                                            )
                                                             1
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! res (
                                                              + res bit
                                                            )
                                                          )
                                                        )
                                                         (
                                                          quote (
                                                            
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! x (
                                                          _div x 2
                                                        )
                                                      )
                                                       (
                                                        set! y (
                                                          _div y 2
                                                        )
                                                      )
                                                       (
                                                        set! bit (
                                                          * bit 2
                                                        )
                                                      )
                                                       (
                                                        set! i (
                                                          + i 1
                                                        )
                                                      )
                                                       (
                                                        loop32
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
                                              loop32
                                            )
                                          )
                                        )
                                      )
                                       (
                                        ret31 res
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
                bit_or a b
              )
               (
                call/cc (
                  lambda (
                    ret34
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
                            let (
                              (
                                res 0
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    bit 1
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
                                            break36
                                          )
                                           (
                                            letrec (
                                              (
                                                loop35 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < i 32
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            abit (
                                                              _mod x 2
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                bbit (
                                                                  _mod y 2
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  or (
                                                                    equal? abit 1
                                                                  )
                                                                   (
                                                                    equal? bbit 1
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! res (
                                                                      + res bit
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  quote (
                                                                    
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! x (
                                                                  _div x 2
                                                                )
                                                              )
                                                               (
                                                                set! y (
                                                                  _div y 2
                                                                )
                                                              )
                                                               (
                                                                set! bit (
                                                                  * bit 2
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
                                                        loop35
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
                                              loop35
                                            )
                                          )
                                        )
                                      )
                                       (
                                        ret34 res
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
                bit_xor a b
              )
               (
                call/cc (
                  lambda (
                    ret37
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
                            let (
                              (
                                res 0
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    bit 1
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
                                            break39
                                          )
                                           (
                                            letrec (
                                              (
                                                loop38 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < i 32
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            abit (
                                                              _mod x 2
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                bbit (
                                                                  _mod y 2
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  equal? (
                                                                    _mod (
                                                                      + abit bbit
                                                                    )
                                                                     2
                                                                  )
                                                                   1
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! res (
                                                                      + res bit
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  quote (
                                                                    
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! x (
                                                                  _div x 2
                                                                )
                                                              )
                                                               (
                                                                set! y (
                                                                  _div y 2
                                                                )
                                                              )
                                                               (
                                                                set! bit (
                                                                  * bit 2
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
                                                        loop38
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
                                              loop38
                                            )
                                          )
                                        )
                                      )
                                       (
                                        ret37 res
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
                not_32 i
              )
               (
                call/cc (
                  lambda (
                    ret40
                  )
                   (
                    begin (
                      if (
                        < i 0
                      )
                       (
                        begin (
                          panic "Input must be non-negative"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret40 (
                        - 4294967295 i
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                sum_32 a b
              )
               (
                call/cc (
                  lambda (
                    ret41
                  )
                   (
                    ret41 (
                      _mod (
                        + a b
                      )
                       MOD
                    )
                  )
                )
              )
            )
             (
              define (
                lshift num k
              )
               (
                call/cc (
                  lambda (
                    ret42
                  )
                   (
                    let (
                      (
                        result (
                          _mod num MOD
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
                                break44
                              )
                               (
                                letrec (
                                  (
                                    loop43 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i k
                                        )
                                         (
                                          begin (
                                            set! result (
                                              _mod (
                                                * result 2
                                              )
                                               MOD
                                            )
                                          )
                                           (
                                            set! i (
                                              + i 1
                                            )
                                          )
                                           (
                                            loop43
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
                                  loop43
                                )
                              )
                            )
                          )
                           (
                            ret42 result
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
                rshift num k
              )
               (
                call/cc (
                  lambda (
                    ret45
                  )
                   (
                    let (
                      (
                        result num
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
                                break47
                              )
                               (
                                letrec (
                                  (
                                    loop46 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i k
                                        )
                                         (
                                          begin (
                                            set! result (
                                              _div result 2
                                            )
                                          )
                                           (
                                            set! i (
                                              + i 1
                                            )
                                          )
                                           (
                                            loop46
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
                                  loop46
                                )
                              )
                            )
                          )
                           (
                            ret45 result
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
                left_rotate_32 i shift
              )
               (
                call/cc (
                  lambda (
                    ret48
                  )
                   (
                    begin (
                      if (
                        < i 0
                      )
                       (
                        begin (
                          panic "Input must be non-negative"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        < shift 0
                      )
                       (
                        begin (
                          panic "Shift must be non-negative"
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
                          left (
                            lshift i shift
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              right (
                                rshift i (
                                  - 32 shift
                                )
                              )
                            )
                          )
                           (
                            begin (
                              ret48 (
                                _mod (
                                  _add left right
                                )
                                 MOD
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
                md5_me message
              )
               (
                call/cc (
                  lambda (
                    ret49
                  )
                   (
                    let (
                      (
                        bit_string (
                          preprocess message
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            added_consts (
                              _list 3614090360 3905402710 606105819 3250441966 4118548399 1200080426 2821735955 4249261313 1770035416 2336552879 4294925233 2304563134 1804603682 4254626195 2792965006 1236535329 4129170786 3225465664 643717713 3921069994 3593408605 38016083 3634488961 3889429448 568446438 3275163606 4107603335 1163531501 2850285829 4243563512 1735328473 2368359562 4294588738 2272392833 1839030562 4259657740 2763975236 1272893353 4139469664 3200236656 681279174 3936430074 3572445317 76029189 3654602809 3873151461 530742520 3299628645 4096336452 1126891415 2878612391 4237533241 1700485571 2399980690 4293915773 2240044497 1873313359 4264355552 2734768916 1309151649 4149444226 3174756917 718787259 3951481745
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                shift_amounts (
                                  _list 7 12 17 22 7 12 17 22 7 12 17 22 7 12 17 22 5 9 14 20 5 9 14 20 5 9 14 20 5 9 14 20 4 11 16 23 4 11 16 23 4 11 16 23 4 11 16 23 6 10 15 21 6 10 15 21 6 10 15 21 6 10 15 21
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    a0 1732584193
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        b0 4023233417
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            c0 2562383102
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                d0 271733878
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    blocks (
                                                      get_block_words bit_string
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        bi 0
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        call/cc (
                                                          lambda (
                                                            break51
                                                          )
                                                           (
                                                            letrec (
                                                              (
                                                                loop50 (
                                                                  lambda (
                                                                    
                                                                  )
                                                                   (
                                                                    if (
                                                                      < bi (
                                                                        _len blocks
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            block (
                                                                              cond (
                                                                                (
                                                                                  string? blocks
                                                                                )
                                                                                 (
                                                                                  _substring blocks bi (
                                                                                    + bi 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? blocks
                                                                                )
                                                                                 (
                                                                                  hash-table-ref blocks bi
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref-safe blocks bi
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                a a0
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    b b0
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        c c0
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            d d0
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
                                                                                                    break53
                                                                                                  )
                                                                                                   (
                                                                                                    letrec (
                                                                                                      (
                                                                                                        loop52 (
                                                                                                          lambda (
                                                                                                            
                                                                                                          )
                                                                                                           (
                                                                                                            if (
                                                                                                              < i 64
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                let (
                                                                                                                  (
                                                                                                                    f 0
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    let (
                                                                                                                      (
                                                                                                                        g 0
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        if (
                                                                                                                          <= i 15
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            set! f (
                                                                                                                              bit_xor d (
                                                                                                                                bit_and b (
                                                                                                                                  bit_xor c d
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            set! g i
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          if (
                                                                                                                            <= i 31
                                                                                                                          )
                                                                                                                           (
                                                                                                                            begin (
                                                                                                                              set! f (
                                                                                                                                bit_xor c (
                                                                                                                                  bit_and d (
                                                                                                                                    bit_xor b c
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              set! g (
                                                                                                                                _mod (
                                                                                                                                  + (
                                                                                                                                    * 5 i
                                                                                                                                  )
                                                                                                                                   1
                                                                                                                                )
                                                                                                                                 16
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            if (
                                                                                                                              <= i 47
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                set! f (
                                                                                                                                  bit_xor (
                                                                                                                                    bit_xor b c
                                                                                                                                  )
                                                                                                                                   d
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                set! g (
                                                                                                                                  _mod (
                                                                                                                                    + (
                                                                                                                                      * 3 i
                                                                                                                                    )
                                                                                                                                     5
                                                                                                                                  )
                                                                                                                                   16
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                set! f (
                                                                                                                                  bit_xor c (
                                                                                                                                    bit_or b (
                                                                                                                                      not_32 d
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                set! g (
                                                                                                                                  _mod (
                                                                                                                                    * 7 i
                                                                                                                                  )
                                                                                                                                   16
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        set! f (
                                                                                                                          sum_32 f a
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        set! f (
                                                                                                                          sum_32 f (
                                                                                                                            list-ref-safe added_consts i
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        set! f (
                                                                                                                          sum_32 f (
                                                                                                                            cond (
                                                                                                                              (
                                                                                                                                string? block
                                                                                                                              )
                                                                                                                               (
                                                                                                                                _substring block g (
                                                                                                                                  + g 1
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              (
                                                                                                                                hash-table? block
                                                                                                                              )
                                                                                                                               (
                                                                                                                                hash-table-ref block g
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              else (
                                                                                                                                list-ref-safe block g
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        let (
                                                                                                                          (
                                                                                                                            rotated (
                                                                                                                              left_rotate_32 f (
                                                                                                                                list-ref-safe shift_amounts i
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            let (
                                                                                                                              (
                                                                                                                                new_b (
                                                                                                                                  sum_32 b rotated
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                set! a d
                                                                                                                              )
                                                                                                                               (
                                                                                                                                set! d c
                                                                                                                              )
                                                                                                                               (
                                                                                                                                set! c b
                                                                                                                              )
                                                                                                                               (
                                                                                                                                set! b new_b
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
                                                                                                                loop52
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
                                                                                                      loop52
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                set! a0 (
                                                                                                  sum_32 a0 a
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                set! b0 (
                                                                                                  sum_32 b0 b
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                set! c0 (
                                                                                                  sum_32 c0 c
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                set! d0 (
                                                                                                  sum_32 d0 d
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                set! bi (
                                                                                                  + bi 1
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
                                                                        loop50
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
                                                              loop50
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        let (
                                                          (
                                                            digest (
                                                              _add (
                                                                _add (
                                                                  _add (
                                                                    reformat_hex a0
                                                                  )
                                                                   (
                                                                    reformat_hex b0
                                                                  )
                                                                )
                                                                 (
                                                                  reformat_hex c0
                                                                )
                                                              )
                                                               (
                                                                reformat_hex d0
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            ret49 digest
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
          end55 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur56 (
              quotient (
                * (
                  - end55 start54
                )
                 1000000
              )
               jps57
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur56
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
