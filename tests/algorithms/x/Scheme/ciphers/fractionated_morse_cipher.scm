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
      let (
        (
          MORSE_CODE_DICT (
            alist->hash-table (
              _list (
                cons "A" ".-"
              )
               (
                cons "B" "-..."
              )
               (
                cons "C" "-.-."
              )
               (
                cons "D" "-.."
              )
               (
                cons "E" "."
              )
               (
                cons "F" "..-."
              )
               (
                cons "G" "--."
              )
               (
                cons "H" "...."
              )
               (
                cons "I" ".."
              )
               (
                cons "J" ".---"
              )
               (
                cons "K" "-.-"
              )
               (
                cons "L" ".-.."
              )
               (
                cons "M" "--"
              )
               (
                cons "N" "-."
              )
               (
                cons "O" "---"
              )
               (
                cons "P" ".--."
              )
               (
                cons "Q" "--.-"
              )
               (
                cons "R" ".-."
              )
               (
                cons "S" "..."
              )
               (
                cons "T" "-"
              )
               (
                cons "U" "..-"
              )
               (
                cons "V" "...-"
              )
               (
                cons "W" ".--"
              )
               (
                cons "X" "-..-"
              )
               (
                cons "Y" "-.--"
              )
               (
                cons "Z" "--.."
              )
               (
                cons " " ""
              )
            )
          )
        )
      )
       (
        begin (
          let (
            (
              MORSE_COMBINATIONS (
                _list "..." "..-" "..x" ".-." ".--" ".-x" ".x." ".x-" ".xx" "-.." "-.-" "-.x" "--." "---" "--x" "-x." "-x-" "-xx" "x.." "x.-" "x.x" "x-." "x--" "x-x" "xx." "xx-" "xxx"
              )
            )
          )
           (
            begin (
              let (
                (
                  REVERSE_DICT (
                    alist->hash-table (
                      _list (
                        cons ".-" "A"
                      )
                       (
                        cons "-..." "B"
                      )
                       (
                        cons "-.-." "C"
                      )
                       (
                        cons "-.." "D"
                      )
                       (
                        cons "." "E"
                      )
                       (
                        cons "..-." "F"
                      )
                       (
                        cons "--." "G"
                      )
                       (
                        cons "...." "H"
                      )
                       (
                        cons ".." "I"
                      )
                       (
                        cons ".---" "J"
                      )
                       (
                        cons "-.-" "K"
                      )
                       (
                        cons ".-.." "L"
                      )
                       (
                        cons "--" "M"
                      )
                       (
                        cons "-." "N"
                      )
                       (
                        cons "---" "O"
                      )
                       (
                        cons ".--." "P"
                      )
                       (
                        cons "--.-" "Q"
                      )
                       (
                        cons ".-." "R"
                      )
                       (
                        cons "..." "S"
                      )
                       (
                        cons "-" "T"
                      )
                       (
                        cons "..-" "U"
                      )
                       (
                        cons "...-" "V"
                      )
                       (
                        cons ".--" "W"
                      )
                       (
                        cons "-..-" "X"
                      )
                       (
                        cons "-.--" "Y"
                      )
                       (
                        cons "--.." "Z"
                      )
                       (
                        cons "" " "
                      )
                    )
                  )
                )
              )
               (
                begin (
                  define (
                    encodeToMorse plaintext
                  )
                   (
                    call/cc (
                      lambda (
                        ret1
                      )
                       (
                        let (
                          (
                            morse ""
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
                                                _len plaintext
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    ch (
                                                      upper (
                                                        _substring plaintext i (
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
                                                        code ""
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          cond (
                                                            (
                                                              string? MORSE_CODE_DICT
                                                            )
                                                             (
                                                              if (
                                                                string-contains MORSE_CODE_DICT ch
                                                              )
                                                               #t #f
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? MORSE_CODE_DICT
                                                            )
                                                             (
                                                              if (
                                                                hash-table-exists? MORSE_CODE_DICT ch
                                                              )
                                                               #t #f
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              if (
                                                                member ch MORSE_CODE_DICT
                                                              )
                                                               #t #f
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! code (
                                                              hash-table-ref/default MORSE_CODE_DICT ch (
                                                                quote (
                                                                  
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
                                                        if (
                                                          > i 0
                                                        )
                                                         (
                                                          begin (
                                                            set! morse (
                                                              string-append morse "x"
                                                            )
                                                          )
                                                        )
                                                         (
                                                          quote (
                                                            
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! morse (
                                                          string-append morse code
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
                                ret1 morse
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
                    encryptFractionatedMorse plaintext key
                  )
                   (
                    call/cc (
                      lambda (
                        ret4
                      )
                       (
                        let (
                          (
                            morseCode (
                              encodeToMorse plaintext
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                combinedKey (
                                  string-append (
                                    upper key
                                  )
                                   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    dedupKey ""
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
                                                        _len combinedKey
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            ch (
                                                              _substring combinedKey i (
                                                                + i 1
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              not (
                                                                cond (
                                                                  (
                                                                    string? dedupKey
                                                                  )
                                                                   (
                                                                    if (
                                                                      string-contains dedupKey ch
                                                                    )
                                                                     #t #f
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? dedupKey
                                                                  )
                                                                   (
                                                                    if (
                                                                      hash-table-exists? dedupKey ch
                                                                    )
                                                                     #t #f
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    if (
                                                                      member ch dedupKey
                                                                    )
                                                                     #t #f
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! dedupKey (
                                                                  string-append dedupKey ch
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
                                            paddingLength (
                                              - 3 (
                                                modulo (
                                                  _len morseCode
                                                )
                                                 3
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                p 0
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
                                                              < p paddingLength
                                                            )
                                                             (
                                                              begin (
                                                                set! morseCode (
                                                                  string-append morseCode "x"
                                                                )
                                                              )
                                                               (
                                                                set! p (
                                                                  + p 1
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
                                                let (
                                                  (
                                                    dict (
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
                                                        j 0
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
                                                                      < j 26
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            combo (
                                                                              list-ref MORSE_COMBINATIONS j
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                letter (
                                                                                  _substring dedupKey j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                hash-table-set! dict combo letter
                                                                              )
                                                                               (
                                                                                set! j (
                                                                                  + j 1
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
                                                        hash-table-set! dict "xxx" ""
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
                                                                              < k (
                                                                                _len morseCode
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    group (
                                                                                      if (
                                                                                        string? morseCode
                                                                                      )
                                                                                       (
                                                                                        _substring morseCode k (
                                                                                          + k 3
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        take (
                                                                                          drop morseCode k
                                                                                        )
                                                                                         (
                                                                                          - (
                                                                                            + k 3
                                                                                          )
                                                                                           k
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! encrypted (
                                                                                      string-append encrypted (
                                                                                        hash-table-ref/default dict group (
                                                                                          quote (
                                                                                            
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! k (
                                                                                      + k 3
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
                                                                ret4 encrypted
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
                  define (
                    decryptFractionatedMorse ciphertext key
                  )
                   (
                    call/cc (
                      lambda (
                        ret13
                      )
                       (
                        let (
                          (
                            combinedKey (
                              string-append (
                                upper key
                              )
                               "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                dedupKey ""
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
                                                  < i (
                                                    _len combinedKey
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        ch (
                                                          _substring combinedKey i (
                                                            + i 1
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          not (
                                                            cond (
                                                              (
                                                                string? dedupKey
                                                              )
                                                               (
                                                                if (
                                                                  string-contains dedupKey ch
                                                                )
                                                                 #t #f
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? dedupKey
                                                              )
                                                               (
                                                                if (
                                                                  hash-table-exists? dedupKey ch
                                                                )
                                                                 #t #f
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                if (
                                                                  member ch dedupKey
                                                                )
                                                                 #t #f
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! dedupKey (
                                                              string-append dedupKey ch
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
                                    let (
                                      (
                                        inv (
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
                                            j 0
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
                                                          < j 26
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                letter (
                                                                  _substring dedupKey j (
                                                                    + j 1
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                hash-table-set! inv letter (
                                                                  list-ref MORSE_COMBINATIONS j
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
                                            let (
                                              (
                                                morse ""
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    k 0
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
                                                                  < k (
                                                                    _len ciphertext
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        ch (
                                                                          _substring ciphertext k (
                                                                            + k 1
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          cond (
                                                                            (
                                                                              string? inv
                                                                            )
                                                                             (
                                                                              if (
                                                                                string-contains inv ch
                                                                              )
                                                                               #t #f
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? inv
                                                                            )
                                                                             (
                                                                              if (
                                                                                hash-table-exists? inv ch
                                                                              )
                                                                               #t #f
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              if (
                                                                                member ch inv
                                                                              )
                                                                               #t #f
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! morse (
                                                                              string-append morse (
                                                                                hash-table-ref/default inv ch (
                                                                                  quote (
                                                                                    
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
                                                    let (
                                                      (
                                                        codes (
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
                                                                m 0
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
                                                                              < m (
                                                                                _len morse
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    ch (
                                                                                      _substring morse m (
                                                                                        + m 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    if (
                                                                                      string=? ch "x"
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! codes (
                                                                                          append codes (
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
                                                                                    set! m (
                                                                                      + m 1
                                                                                    )
                                                                                  )
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
                                                                set! codes (
                                                                  append codes (
                                                                    _list current
                                                                  )
                                                                )
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
                                                                        idx 0
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
                                                                                      < idx (
                                                                                        _len codes
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            code (
                                                                                              list-ref codes idx
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! decrypted (
                                                                                              string-append decrypted (
                                                                                                hash-table-ref/default REVERSE_DICT code (
                                                                                                  quote (
                                                                                                    
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! idx (
                                                                                              + idx 1
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
                                                                            start 0
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
                                                                                        if #t (
                                                                                          begin (
                                                                                            if (
                                                                                              < start (
                                                                                                _len decrypted
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                if (
                                                                                                  string=? (
                                                                                                    _substring decrypted start (
                                                                                                      + start 1
                                                                                                    )
                                                                                                  )
                                                                                                   " "
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    set! start (
                                                                                                      + start 1
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
                                                                                             (
                                                                                              quote (
                                                                                                
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            break25 (
                                                                                              quote (
                                                                                                
                                                                                              )
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
                                                                            let (
                                                                              (
                                                                                end (
                                                                                  _len decrypted
                                                                                )
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
                                                                                            if #t (
                                                                                              begin (
                                                                                                if (
                                                                                                  > end start
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    if (
                                                                                                      string=? (
                                                                                                        _substring decrypted (
                                                                                                          - end 1
                                                                                                        )
                                                                                                         end
                                                                                                      )
                                                                                                       " "
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        set! end (
                                                                                                          - end 1
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
                                                                                                 (
                                                                                                  quote (
                                                                                                    
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                break27 (
                                                                                                  quote (
                                                                                                    
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
                                                                                ret13 (
                                                                                  _substring decrypted start end
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
                        )
                      )
                    )
                  )
                )
                 (
                  let (
                    (
                      plaintext "defend the east"
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? "Plain Text:"
                        )
                         "Plain Text:" (
                          to-str "Plain Text:"
                        )
                      )
                    )
                     (
                      _display " "
                    )
                     (
                      _display (
                        if (
                          string? plaintext
                        )
                         plaintext (
                          to-str plaintext
                        )
                      )
                    )
                     (
                      newline
                    )
                     (
                      let (
                        (
                          key "ROUNDTABLE"
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              ciphertext (
                                encryptFractionatedMorse plaintext key
                              )
                            )
                          )
                           (
                            begin (
                              _display (
                                if (
                                  string? "Encrypted:"
                                )
                                 "Encrypted:" (
                                  to-str "Encrypted:"
                                )
                              )
                            )
                             (
                              _display " "
                            )
                             (
                              _display (
                                if (
                                  string? ciphertext
                                )
                                 ciphertext (
                                  to-str ciphertext
                                )
                              )
                            )
                             (
                              newline
                            )
                             (
                              let (
                                (
                                  decrypted (
                                    decryptFractionatedMorse ciphertext key
                                  )
                                )
                              )
                               (
                                begin (
                                  _display (
                                    if (
                                      string? "Decrypted:"
                                    )
                                     "Decrypted:" (
                                      to-str "Decrypted:"
                                    )
                                  )
                                )
                                 (
                                  _display " "
                                )
                                 (
                                  _display (
                                    if (
                                      string? decrypted
                                    )
                                     decrypted (
                                      to-str decrypted
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
