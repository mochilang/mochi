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
      let (
        (
          LOWER "abcdefghijklmnopqrstuvwxyz"
        )
      )
       (
        begin (
          let (
            (
              UPPER "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            )
          )
           (
            begin (
              define (
                to_lowercase s
              )
               (
                call/cc (
                  lambda (
                    ret1
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
                                            let (
                                              (
                                                c (
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
                                                    j 0
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
                                                                      < j 26
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          string=? c (
                                                                            _substring UPPER j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! res (
                                                                              string-append res (
                                                                                _substring LOWER j (
                                                                                  + j 1
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! found #t
                                                                          )
                                                                           (
                                                                            break5 (
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
                                                        if (
                                                          not found
                                                        )
                                                         (
                                                          begin (
                                                            set! res (
                                                              string-append res c
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
                                                )
                                              )
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
              define (
                char_index c
              )
               (
                call/cc (
                  lambda (
                    ret6
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
                                      < i 26
                                    )
                                     (
                                      begin (
                                        if (
                                          string=? c (
                                            _substring LOWER i (
                                              + i 1
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            ret6 i
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
                        ret6 (
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
                index_char i
              )
               (
                call/cc (
                  lambda (
                    ret9
                  )
                   (
                    ret9 (
                      _substring LOWER i (
                        + i 1
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                encrypt plaintext key
              )
               (
                call/cc (
                  lambda (
                    ret10
                  )
                   (
                    begin (
                      if (
                        equal? (
                          _len plaintext
                        )
                         0
                      )
                       (
                        begin (
                          panic "plaintext is empty"
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
                          _len key
                        )
                         0
                      )
                       (
                        begin (
                          panic "key is empty"
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
                          full_key (
                            string-append key plaintext
                          )
                        )
                      )
                       (
                        begin (
                          set! plaintext (
                            to_lowercase plaintext
                          )
                        )
                         (
                          set! full_key (
                            to_lowercase full_key
                          )
                        )
                         (
                          let (
                            (
                              p_i 0
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  k_i 0
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      ciphertext ""
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
                                                    < p_i (
                                                      _len plaintext
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          p_char (
                                                            _substring plaintext p_i (
                                                              + p_i 1
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              p_idx (
                                                                char_index p_char
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              if (
                                                                _lt p_idx 0
                                                              )
                                                               (
                                                                begin (
                                                                  set! ciphertext (
                                                                    string-append ciphertext p_char
                                                                  )
                                                                )
                                                                 (
                                                                  set! p_i (
                                                                    + p_i 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      k_char (
                                                                        _substring full_key k_i (
                                                                          + k_i 1
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      let (
                                                                        (
                                                                          k_idx (
                                                                            char_index k_char
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          if (
                                                                            _lt k_idx 0
                                                                          )
                                                                           (
                                                                            begin (
                                                                              set! k_i (
                                                                                + k_i 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              let (
                                                                                (
                                                                                  c_idx (
                                                                                    fmod (
                                                                                      _add p_idx k_idx
                                                                                    )
                                                                                     26
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  set! ciphertext (
                                                                                    string-append ciphertext (
                                                                                      index_char c_idx
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  set! k_i (
                                                                                    + k_i 1
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  set! p_i (
                                                                                    + p_i 1
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
                                      ret10 ciphertext
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
                decrypt ciphertext key
              )
               (
                call/cc (
                  lambda (
                    ret13
                  )
                   (
                    begin (
                      if (
                        equal? (
                          _len ciphertext
                        )
                         0
                      )
                       (
                        begin (
                          panic "ciphertext is empty"
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
                          _len key
                        )
                         0
                      )
                       (
                        begin (
                          panic "key is empty"
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
                          current_key (
                            to_lowercase key
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              c_i 0
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  k_i 0
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      plaintext ""
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
                                                    < c_i (
                                                      _len ciphertext
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          c_char (
                                                            _substring ciphertext c_i (
                                                              + c_i 1
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              c_idx (
                                                                char_index c_char
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              if (
                                                                _lt c_idx 0
                                                              )
                                                               (
                                                                begin (
                                                                  set! plaintext (
                                                                    string-append plaintext c_char
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      k_char (
                                                                        cond (
                                                                          (
                                                                            string? current_key
                                                                          )
                                                                           (
                                                                            _substring current_key k_i (
                                                                              + k_i 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? current_key
                                                                          )
                                                                           (
                                                                            hash-table-ref current_key k_i
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref current_key k_i
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      let (
                                                                        (
                                                                          k_idx (
                                                                            char_index k_char
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          let (
                                                                            (
                                                                              p_idx (
                                                                                fmod (
                                                                                  _add (
                                                                                    - c_idx k_idx
                                                                                  )
                                                                                   26
                                                                                )
                                                                                 26
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              let (
                                                                                (
                                                                                  p_char (
                                                                                    index_char p_idx
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  set! plaintext (
                                                                                    string-append plaintext p_char
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  set! current_key (
                                                                                    _add current_key p_char
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  set! k_i (
                                                                                    + k_i 1
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
                                                              set! c_i (
                                                                + c_i 1
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
                                      ret13 plaintext
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
                    encrypt "hello world" "coffee"
                  )
                )
                 (
                  encrypt "hello world" "coffee"
                )
                 (
                  to-str (
                    encrypt "hello world" "coffee"
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
                    decrypt "jsqqs avvwo" "coffee"
                  )
                )
                 (
                  decrypt "jsqqs avvwo" "coffee"
                )
                 (
                  to-str (
                    decrypt "jsqqs avvwo" "coffee"
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
                    encrypt "coffee is good as python" "TheAlgorithms"
                  )
                )
                 (
                  encrypt "coffee is good as python" "TheAlgorithms"
                )
                 (
                  to-str (
                    encrypt "coffee is good as python" "TheAlgorithms"
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
                    decrypt "vvjfpk wj ohvp su ddylsv" "TheAlgorithms"
                  )
                )
                 (
                  decrypt "vvjfpk wj ohvp su ddylsv" "TheAlgorithms"
                )
                 (
                  to-str (
                    decrypt "vvjfpk wj ohvp su ddylsv" "TheAlgorithms"
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
