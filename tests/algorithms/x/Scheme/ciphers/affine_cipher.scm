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
      start18 (
        current-jiffy
      )
    )
     (
      jps21 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          SYMBOLS " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
        )
      )
       (
        begin (
          define (
            gcd a b
          )
           (
            call/cc (
              lambda (
                ret1
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
                                      not (
                                        equal? y 0
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            temp (
                                              modulo x y
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
                        ret1 x
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
                ret4
              )
               (
                begin (
                  if (
                    not (
                      equal? (
                        gcd a m
                      )
                       1
                    )
                  )
                   (
                    begin (
                      panic (
                        string-append (
                          string-append (
                            string-append (
                              string-append "mod inverse of " (
                                to-str-space a
                              )
                            )
                             " and "
                          )
                           (
                            to-str-space m
                          )
                        )
                         " does not exist"
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
                      u1 1
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          u2 0
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              u3 a
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  v1 0
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      v2 1
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          v3 m
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
                                                        not (
                                                          equal? v3 0
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              q (
                                                                quotient u3 v3
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  t1 (
                                                                    - u1 (
                                                                      * q v1
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      t2 (
                                                                        - u2 (
                                                                          * q v2
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      let (
                                                                        (
                                                                          t3 (
                                                                            - u3 (
                                                                              * q v3
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          set! u1 v1
                                                                        )
                                                                         (
                                                                          set! u2 v2
                                                                        )
                                                                         (
                                                                          set! u3 v3
                                                                        )
                                                                         (
                                                                          set! v1 t1
                                                                        )
                                                                         (
                                                                          set! v2 t2
                                                                        )
                                                                         (
                                                                          set! v3 t3
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
                                              res (
                                                modulo u1 m
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                < res 0
                                              )
                                               (
                                                begin (
                                                  ret4 (
                                                    + res m
                                                  )
                                                )
                                              )
                                               (
                                                quote (
                                                  
                                                )
                                              )
                                            )
                                             (
                                              ret4 res
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
            find_symbol ch
          )
           (
            call/cc (
              lambda (
                ret7
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
                                    _len SYMBOLS
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      string=? (
                                        _substring SYMBOLS i (
                                          + i 1
                                        )
                                      )
                                       ch
                                    )
                                     (
                                      begin (
                                        ret7 i
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
                    ret7 (
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
            check_keys key_a key_b mode
          )
           (
            call/cc (
              lambda (
                ret10
              )
               (
                let (
                  (
                    m (
                      _len SYMBOLS
                    )
                  )
                )
                 (
                  begin (
                    if (
                      string=? mode "encrypt"
                    )
                     (
                      begin (
                        if (
                          equal? key_a 1
                        )
                         (
                          begin (
                            panic "The affine cipher becomes weak when key A is set to 1. Choose different key"
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        if (
                          equal? key_b 0
                        )
                         (
                          begin (
                            panic "The affine cipher becomes weak when key B is set to 0. Choose different key"
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
                    if (
                      or (
                        or (
                          < key_a 0
                        )
                         (
                          < key_b 0
                        )
                      )
                       (
                        > key_b (
                          - m 1
                        )
                      )
                    )
                     (
                      begin (
                        panic (
                          string-append "Key A must be greater than 0 and key B must be between 0 and " (
                            to-str-space (
                              - m 1
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
                      not (
                        equal? (
                          gcd key_a m
                        )
                         1
                      )
                    )
                     (
                      begin (
                        panic (
                          string-append (
                            string-append (
                              string-append (
                                string-append "Key A " (
                                  to-str-space key_a
                                )
                              )
                               " and the symbol set size "
                            )
                             (
                              to-str-space m
                            )
                          )
                           " are not relatively prime. Choose a different key."
                        )
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
          )
        )
         (
          define (
            encrypt_message key message
          )
           (
            call/cc (
              lambda (
                ret11
              )
               (
                let (
                  (
                    m (
                      _len SYMBOLS
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        key_a (
                          quotient key m
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            key_b (
                              modulo key m
                            )
                          )
                        )
                         (
                          begin (
                            check_keys key_a key_b "encrypt"
                          )
                           (
                            let (
                              (
                                cipher_text ""
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
                                                        let (
                                                          (
                                                            index (
                                                              find_symbol ch
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              _ge index 0
                                                            )
                                                             (
                                                              begin (
                                                                set! cipher_text (
                                                                  string-append cipher_text (
                                                                    _substring SYMBOLS (
                                                                      fmod (
                                                                        _add (
                                                                          * index key_a
                                                                        )
                                                                         key_b
                                                                      )
                                                                       m
                                                                    )
                                                                     (
                                                                      + (
                                                                        fmod (
                                                                          _add (
                                                                            * index key_a
                                                                          )
                                                                           key_b
                                                                        )
                                                                         m
                                                                      )
                                                                       1
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! cipher_text (
                                                                  string-append cipher_text ch
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
                                    ret11 cipher_text
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
            decrypt_message key message
          )
           (
            call/cc (
              lambda (
                ret14
              )
               (
                let (
                  (
                    m (
                      _len SYMBOLS
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        key_a (
                          quotient key m
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            key_b (
                              modulo key m
                            )
                          )
                        )
                         (
                          begin (
                            check_keys key_a key_b "decrypt"
                          )
                           (
                            let (
                              (
                                inv (
                                  mod_inverse key_a m
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    plain_text ""
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
                                                            let (
                                                              (
                                                                index (
                                                                  find_symbol ch
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  _ge index 0
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        n (
                                                                          * (
                                                                            - index key_b
                                                                          )
                                                                           inv
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            pos (
                                                                              fmod n m
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                final (
                                                                                  if (
                                                                                    _lt pos 0
                                                                                  )
                                                                                   (
                                                                                    _add pos m
                                                                                  )
                                                                                   pos
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! plain_text (
                                                                                  string-append plain_text (
                                                                                    _substring SYMBOLS final (
                                                                                      + final 1
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
                                                                  begin (
                                                                    set! plain_text (
                                                                      string-append plain_text ch
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
                                        ret14 plain_text
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
                ret17
              )
               (
                let (
                  (
                    key 4545
                  )
                )
                 (
                  begin (
                    let (
                      (
                        msg "The affine cipher is a type of monoalphabetic substitution cipher."
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            enc (
                              encrypt_message key msg
                            )
                          )
                        )
                         (
                          begin (
                            _display (
                              if (
                                string? enc
                              )
                               enc (
                                to-str enc
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
                                  decrypt_message key enc
                                )
                              )
                               (
                                decrypt_message key enc
                              )
                               (
                                to-str (
                                  decrypt_message key enc
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
         (
          main
        )
      )
    )
     (
      let (
        (
          end19 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur20 (
              quotient (
                * (
                  - end19 start18
                )
                 1000000
              )
               jps21
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur20
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
