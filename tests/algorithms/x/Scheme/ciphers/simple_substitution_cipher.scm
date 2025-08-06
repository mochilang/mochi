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
      start31 (
        current-jiffy
      )
    )
     (
      jps34 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          LETTERS "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        )
      )
       (
        begin (
          let (
            (
              LOWERCASE "abcdefghijklmnopqrstuvwxyz"
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
                    rand n
                  )
                   (
                    call/cc (
                      lambda (
                        ret1
                      )
                       (
                        begin (
                          set! seed (
                            _mod (
                              + (
                                * seed 1664525
                              )
                               1013904223
                            )
                             2147483647
                          )
                        )
                         (
                          ret1 (
                            _mod seed n
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    get_random_key
                  )
                   (
                    call/cc (
                      lambda (
                        ret2
                      )
                       (
                        let (
                          (
                            chars (
                              quote (
                                
                              )
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
                                              < i (
                                                _len LETTERS
                                              )
                                            )
                                             (
                                              begin (
                                                set! chars (
                                                  append chars (
                                                    _list (
                                                      _substring LETTERS i (
                                                        + i 1
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
                                    j (
                                      - (
                                        _len chars
                                      )
                                       1
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
                                                            tmp (
                                                              list-ref chars j
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            list-set! chars j (
                                                              list-ref chars k
                                                            )
                                                          )
                                                           (
                                                            list-set! chars k tmp
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
                                        res ""
                                      )
                                    )
                                     (
                                      begin (
                                        set! i 0
                                      )
                                       (
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
                                                        _len chars
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! res (
                                                          string-append res (
                                                            list-ref chars i
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! i (
                                                          + i 1
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
                                        ret2 res
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
                    check_valid_key key
                  )
                   (
                    call/cc (
                      lambda (
                        ret9
                      )
                       (
                        begin (
                          if (
                            not (
                              equal? (
                                _len key
                              )
                               (
                                _len LETTERS
                              )
                            )
                          )
                           (
                            begin (
                              ret9 #f
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
                              used (
                                alist->hash-table (
                                  _list
                                )
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
                                                        hash-table-ref/default used ch (
                                                          quote (
                                                            
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          ret9 #f
                                                        )
                                                      )
                                                       (
                                                        quote (
                                                          
                                                        )
                                                      )
                                                    )
                                                     (
                                                      hash-table-set! used ch #t
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
                                  set! i 0
                                )
                                 (
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
                                                  _len LETTERS
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      ch (
                                                        _substring LETTERS i (
                                                          + i 1
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      if (
                                                        not (
                                                          hash-table-ref/default used ch (
                                                            quote (
                                                              
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          ret9 #f
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
                                  ret9 #t
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
                    index_in s ch
                  )
                   (
                    call/cc (
                      lambda (
                        ret14
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
                                                ret14 i
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
                            ret14 (
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
                    char_to_upper c
                  )
                   (
                    call/cc (
                      lambda (
                        ret17
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
                                break19
                              )
                               (
                                letrec (
                                  (
                                    loop18 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i (
                                            _len LOWERCASE
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              string=? c (
                                                _substring LOWERCASE i (
                                                  + i 1
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                ret17 (
                                                  _substring LETTERS i (
                                                    + i 1
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
                                            loop18
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
                                  loop18
                                )
                              )
                            )
                          )
                           (
                            ret17 c
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    char_to_lower c
                  )
                   (
                    call/cc (
                      lambda (
                        ret20
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
                                          < i (
                                            _len LETTERS
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              string=? c (
                                                _substring LETTERS i (
                                                  + i 1
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                ret20 (
                                                  _substring LOWERCASE i (
                                                    + i 1
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
                            ret20 c
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    is_upper c
                  )
                   (
                    call/cc (
                      lambda (
                        ret23
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
                                          < i (
                                            _len LETTERS
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              string=? c (
                                                _substring LETTERS i (
                                                  + i 1
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                ret23 #t
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
                            ret23 #f
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    translate_message key message mode
                  )
                   (
                    call/cc (
                      lambda (
                        ret26
                      )
                       (
                        let (
                          (
                            chars_a LETTERS
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                chars_b key
                              )
                            )
                             (
                              begin (
                                if (
                                  string=? mode "decrypt"
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        tmp chars_a
                                      )
                                    )
                                     (
                                      begin (
                                        set! chars_a chars_b
                                      )
                                       (
                                        set! chars_b tmp
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
                                    translated ""
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
                                                      < i (
                                                        _len message
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            symbol (
                                                              _substring message i (
                                                                + i 1
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                upper_symbol (
                                                                  char_to_upper symbol
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    idx (
                                                                      index_in chars_a upper_symbol
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
                                                                        let (
                                                                          (
                                                                            mapped (
                                                                              _substring chars_b idx (
                                                                                + idx 1
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            if (
                                                                              is_upper symbol
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! translated (
                                                                                  string-append translated mapped
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! translated (
                                                                                  string-append translated (
                                                                                    char_to_lower mapped
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! translated (
                                                                          string-append translated symbol
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
                                        ret26 translated
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
                    encrypt_message key message
                  )
                   (
                    call/cc (
                      lambda (
                        ret29
                      )
                       (
                        let (
                          (
                            res (
                              translate_message key message "encrypt"
                            )
                          )
                        )
                         (
                          begin (
                            ret29 res
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    decrypt_message key message
                  )
                   (
                    call/cc (
                      lambda (
                        ret30
                      )
                       (
                        let (
                          (
                            res (
                              translate_message key message "decrypt"
                            )
                          )
                        )
                         (
                          begin (
                            ret30 res
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  let (
                    (
                      key "LFWOAYUISVKMNXPBDCRJTQEGHZ"
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            encrypt_message key "Harshil Darji"
                          )
                        )
                         (
                          encrypt_message key "Harshil Darji"
                        )
                         (
                          to-str (
                            encrypt_message key "Harshil Darji"
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
                            decrypt_message key "Ilcrism Olcvs"
                          )
                        )
                         (
                          decrypt_message key "Ilcrism Olcvs"
                        )
                         (
                          to-str (
                            decrypt_message key "Ilcrism Olcvs"
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
          end32 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur33 (
              quotient (
                * (
                  - end32 start31
                )
                 1000000
              )
               jps34
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur33
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
