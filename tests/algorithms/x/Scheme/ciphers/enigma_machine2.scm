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
      start32 (
        current-jiffy
      )
    )
     (
      jps35 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          abc "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        )
      )
       (
        begin (
          let (
            (
              low_abc "abcdefghijklmnopqrstuvwxyz"
            )
          )
           (
            begin (
              let (
                (
                  rotor1 "EGZWVONAHDCLFQMSIPJBYUKXTR"
                )
              )
               (
                begin (
                  let (
                    (
                      rotor2 "FOBHMDKEXQNRAULPGSJVTYICZW"
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          rotor3 "ZJXESIUQLHAVRMDOYGTNFWPBKC"
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              rotor4 "RMDJXFUWGISLHVTCQNKYPBEZOA"
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  rotor5 "SGLCPQWZHKXAREONTFBVIYJUDM"
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      rotor6 "HVSICLTYKQUBXDWAJZOMFGPREN"
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          rotor7 "RZWQHFMVDBKICJLNTUXAGYPSOE"
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              rotor8 "LFKIJODBEGAMQPXVUHYSTCZRWN"
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  rotor9 "KOAEGVDHXPQZMLFTYWJNBRCIUS"
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      reflector_pairs (
                                                        _list "AN" "BO" "CP" "DQ" "ER" "FS" "GT" "HU" "IV" "JW" "KX" "LY" "MZ"
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      define (
                                                        list_contains xs x
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
                                                                                _len xs
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                if (
                                                                                  string=? (
                                                                                    list-ref xs i
                                                                                  )
                                                                                   x
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    ret1 #t
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
                                                                ret1 #f
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      define (
                                                        index_in_string s ch
                                                      )
                                                       (
                                                        call/cc (
                                                          lambda (
                                                            ret4
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
                                                                                    ret4 i
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
                                                                ret4 (
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
                                                            ret7
                                                          )
                                                           (
                                                            ret7 (
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
                                                        to_uppercase s
                                                      )
                                                       (
                                                        call/cc (
                                                          lambda (
                                                            ret8
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
                                                                                              index_in_string low_abc ch
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
                                                                                                    _substring abc idx (
                                                                                                      _add idx 1
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
                                                                    ret8 res
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
                                                        plugboard_map pb ch
                                                      )
                                                       (
                                                        call/cc (
                                                          lambda (
                                                            ret11
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
                                                                                _len pb
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    pair (
                                                                                      list-ref pb i
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        a (
                                                                                          _substring pair 0 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            b (
                                                                                              _substring pair 1 2
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            if (
                                                                                              string=? ch a
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                ret11 b
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              quote (
                                                                                                
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            if (
                                                                                              string=? ch b
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                ret11 a
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
                                                                ret11 ch
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      define (
                                                        reflector_map ch
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
                                                                                _len reflector_pairs
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    pair (
                                                                                      list-ref reflector_pairs i
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        a (
                                                                                          _substring pair 0 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            b (
                                                                                              _substring pair 1 2
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            if (
                                                                                              string=? ch a
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                ret14 b
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              quote (
                                                                                                
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            if (
                                                                                              string=? ch b
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                ret14 a
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
                                                                ret14 ch
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      define (
                                                        count_unique xs
                                                      )
                                                       (
                                                        call/cc (
                                                          lambda (
                                                            ret17
                                                          )
                                                           (
                                                            let (
                                                              (
                                                                unique (
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
                                                                                    _len xs
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    if (
                                                                                      not (
                                                                                        list_contains unique (
                                                                                          list-ref xs i
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! unique (
                                                                                          append unique (
                                                                                            _list (
                                                                                              list-ref xs i
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
                                                                    ret17 (
                                                                      _len unique
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
                                                        build_plugboard pbstring
                                                      )
                                                       (
                                                        call/cc (
                                                          lambda (
                                                            ret20
                                                          )
                                                           (
                                                            begin (
                                                              if (
                                                                equal? (
                                                                  _len pbstring
                                                                )
                                                                 0
                                                              )
                                                               (
                                                                begin (
                                                                  ret20 (
                                                                    _list
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
                                                                    modulo (
                                                                      _len pbstring
                                                                    )
                                                                     2
                                                                  )
                                                                   0
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  panic (
                                                                    string-append (
                                                                      string-append "Odd number of symbols(" (
                                                                        to-str-space (
                                                                          _len pbstring
                                                                        )
                                                                      )
                                                                    )
                                                                     ")"
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
                                                                  pbstring_nospace ""
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
                                                                                      _len pbstring
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      let (
                                                                                        (
                                                                                          ch (
                                                                                            _substring pbstring i (
                                                                                              + i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        begin (
                                                                                          if (
                                                                                            not (
                                                                                              string=? ch " "
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            begin (
                                                                                              set! pbstring_nospace (
                                                                                                string-append pbstring_nospace ch
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
                                                                      let (
                                                                        (
                                                                          seen (
                                                                            _list
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          set! i 0
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
                                                                                        < i (
                                                                                          _len pbstring_nospace
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        begin (
                                                                                          let (
                                                                                            (
                                                                                              ch (
                                                                                                _substring pbstring_nospace i (
                                                                                                  + i 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            begin (
                                                                                              if (
                                                                                                not (
                                                                                                  contains_char abc ch
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                begin (
                                                                                                  panic (
                                                                                                    string-append (
                                                                                                      string-append "'" ch
                                                                                                    )
                                                                                                     "' not in list of symbols"
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
                                                                                                list_contains seen ch
                                                                                              )
                                                                                               (
                                                                                                begin (
                                                                                                  panic (
                                                                                                    string-append (
                                                                                                      string-append "Duplicate symbol(" ch
                                                                                                    )
                                                                                                     ")"
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                quote (
                                                                                                  
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              set! seen (
                                                                                                append seen (
                                                                                                  _list ch
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
                                                                          let (
                                                                            (
                                                                              pb (
                                                                                _list
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              set! i 0
                                                                            )
                                                                             (
                                                                              call/cc (
                                                                                lambda (
                                                                                  break26
                                                                                )
                                                                                 (
                                                                                  letrec (
                                                                                    (
                                                                                      loop25 (
                                                                                        lambda (
                                                                                          
                                                                                        )
                                                                                         (
                                                                                          if (
                                                                                            < i (
                                                                                              - (
                                                                                                _len pbstring_nospace
                                                                                              )
                                                                                               1
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            begin (
                                                                                              let (
                                                                                                (
                                                                                                  a (
                                                                                                    _substring pbstring_nospace i (
                                                                                                      + i 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                begin (
                                                                                                  let (
                                                                                                    (
                                                                                                      b (
                                                                                                        _substring pbstring_nospace (
                                                                                                          + i 1
                                                                                                        )
                                                                                                         (
                                                                                                          + i 2
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    begin (
                                                                                                      set! pb (
                                                                                                        append pb (
                                                                                                          _list (
                                                                                                            string-append a b
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      set! i (
                                                                                                        + i 2
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              loop25
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
                                                                                    loop25
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              ret20 pb
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
                                                        validator rotpos rotsel pb
                                                      )
                                                       (
                                                        call/cc (
                                                          lambda (
                                                            ret27
                                                          )
                                                           (
                                                            begin (
                                                              if (
                                                                _lt (
                                                                  count_unique rotsel
                                                                )
                                                                 3
                                                              )
                                                               (
                                                                begin (
                                                                  panic (
                                                                    string-append (
                                                                      string-append "Please use 3 unique rotors (not " (
                                                                        to-str-space (
                                                                          count_unique rotsel
                                                                        )
                                                                      )
                                                                    )
                                                                     ")"
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
                                                                    _len rotpos
                                                                  )
                                                                   3
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  panic "Rotor position must have 3 values"
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
                                                                  r1 (
                                                                    list-ref rotpos 0
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      r2 (
                                                                        list-ref rotpos 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      let (
                                                                        (
                                                                          r3 (
                                                                            list-ref rotpos 2
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          if (
                                                                            not (
                                                                              and (
                                                                                < 0 r1
                                                                              )
                                                                               (
                                                                                <= r1 (
                                                                                  _len abc
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              panic (
                                                                                string-append (
                                                                                  string-append "First rotor position is not within range of 1..26 (" (
                                                                                    to-str-space r1
                                                                                  )
                                                                                )
                                                                                 ")"
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
                                                                              and (
                                                                                < 0 r2
                                                                              )
                                                                               (
                                                                                <= r2 (
                                                                                  _len abc
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              panic (
                                                                                string-append (
                                                                                  string-append "Second rotor position is not within range of 1..26 (" (
                                                                                    to-str-space r2
                                                                                  )
                                                                                )
                                                                                 ")"
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
                                                                              and (
                                                                                < 0 r3
                                                                              )
                                                                               (
                                                                                <= r3 (
                                                                                  _len abc
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              panic (
                                                                                string-append (
                                                                                  string-append "Third rotor position is not within range of 1..26 (" (
                                                                                    to-str-space r3
                                                                                  )
                                                                                )
                                                                                 ")"
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
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      define (
                                                        enigma text rotor_position rotor_selection plugb
                                                      )
                                                       (
                                                        call/cc (
                                                          lambda (
                                                            ret28
                                                          )
                                                           (
                                                            let (
                                                              (
                                                                up_text (
                                                                  to_uppercase text
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    up_pb (
                                                                      to_uppercase plugb
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    validator rotor_position rotor_selection up_pb
                                                                  )
                                                                   (
                                                                    let (
                                                                      (
                                                                        plugboard (
                                                                          build_plugboard up_pb
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            rotorpos1 (
                                                                              - (
                                                                                list-ref rotor_position 0
                                                                              )
                                                                               1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                rotorpos2 (
                                                                                  - (
                                                                                    list-ref rotor_position 1
                                                                                  )
                                                                                   1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    rotorpos3 (
                                                                                      - (
                                                                                        list-ref rotor_position 2
                                                                                      )
                                                                                       1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        rotor_a (
                                                                                          list-ref rotor_selection 0
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            rotor_b (
                                                                                              list-ref rotor_selection 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                rotor_c (
                                                                                                  list-ref rotor_selection 2
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                let (
                                                                                                  (
                                                                                                    result ""
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
                                                                                                                      < i (
                                                                                                                        _len up_text
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        let (
                                                                                                                          (
                                                                                                                            symbol (
                                                                                                                              _substring up_text i (
                                                                                                                                + i 1
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            if (
                                                                                                                              contains_char abc symbol
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                set! symbol (
                                                                                                                                  plugboard_map plugboard symbol
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                let (
                                                                                                                                  (
                                                                                                                                    index (
                                                                                                                                      _add (
                                                                                                                                        index_in_string abc symbol
                                                                                                                                      )
                                                                                                                                       rotorpos1
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  begin (
                                                                                                                                    set! symbol (
                                                                                                                                      _substring rotor_a (
                                                                                                                                        fmod index (
                                                                                                                                          _len abc
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        _add (
                                                                                                                                          fmod index (
                                                                                                                                            _len abc
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    set! index (
                                                                                                                                      _add (
                                                                                                                                        index_in_string abc symbol
                                                                                                                                      )
                                                                                                                                       rotorpos2
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    set! symbol (
                                                                                                                                      _substring rotor_b (
                                                                                                                                        fmod index (
                                                                                                                                          _len abc
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        _add (
                                                                                                                                          fmod index (
                                                                                                                                            _len abc
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    set! index (
                                                                                                                                      _add (
                                                                                                                                        index_in_string abc symbol
                                                                                                                                      )
                                                                                                                                       rotorpos3
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    set! symbol (
                                                                                                                                      _substring rotor_c (
                                                                                                                                        fmod index (
                                                                                                                                          _len abc
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        _add (
                                                                                                                                          fmod index (
                                                                                                                                            _len abc
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    set! symbol (
                                                                                                                                      reflector_map symbol
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    set! index (
                                                                                                                                      - (
                                                                                                                                        index_in_string rotor_c symbol
                                                                                                                                      )
                                                                                                                                       rotorpos3
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    if (
                                                                                                                                      _lt index 0
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      begin (
                                                                                                                                        set! index (
                                                                                                                                          _add index (
                                                                                                                                            _len abc
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
                                                                                                                                    set! symbol (
                                                                                                                                      _substring abc index (
                                                                                                                                        _add index 1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    set! index (
                                                                                                                                      - (
                                                                                                                                        index_in_string rotor_b symbol
                                                                                                                                      )
                                                                                                                                       rotorpos2
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    if (
                                                                                                                                      _lt index 0
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      begin (
                                                                                                                                        set! index (
                                                                                                                                          _add index (
                                                                                                                                            _len abc
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
                                                                                                                                    set! symbol (
                                                                                                                                      _substring abc index (
                                                                                                                                        _add index 1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    set! index (
                                                                                                                                      - (
                                                                                                                                        index_in_string rotor_a symbol
                                                                                                                                      )
                                                                                                                                       rotorpos1
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    if (
                                                                                                                                      _lt index 0
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      begin (
                                                                                                                                        set! index (
                                                                                                                                          _add index (
                                                                                                                                            _len abc
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
                                                                                                                                    set! symbol (
                                                                                                                                      _substring abc index (
                                                                                                                                        _add index 1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    set! symbol (
                                                                                                                                      plugboard_map plugboard symbol
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    set! rotorpos1 (
                                                                                                                                      + rotorpos1 1
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    if (
                                                                                                                                      >= rotorpos1 (
                                                                                                                                        _len abc
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      begin (
                                                                                                                                        set! rotorpos1 0
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        set! rotorpos2 (
                                                                                                                                          + rotorpos2 1
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
                                                                                                                                      >= rotorpos2 (
                                                                                                                                        _len abc
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      begin (
                                                                                                                                        set! rotorpos2 0
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        set! rotorpos3 (
                                                                                                                                          + rotorpos3 1
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
                                                                                                                                      >= rotorpos3 (
                                                                                                                                        _len abc
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      begin (
                                                                                                                                        set! rotorpos3 0
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
                                                                                                                              quote (
                                                                                                                                
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            set! result (
                                                                                                                              string-append result symbol
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
                                                                                                        ret28 result
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
                                                      define (
                                                        main
                                                      )
                                                       (
                                                        call/cc (
                                                          lambda (
                                                            ret31
                                                          )
                                                           (
                                                            let (
                                                              (
                                                                message "This is my Python script that emulates the Enigma machine from WWII."
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    rotor_pos (
                                                                      _list 1 1 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        pb "pictures"
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            rotor_sel (
                                                                              _list rotor2 rotor4 rotor8
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                en (
                                                                                  enigma message rotor_pos rotor_sel pb
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                _display (
                                                                                  if (
                                                                                    string? (
                                                                                      string-append "Encrypted message: " en
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    string-append "Encrypted message: " en
                                                                                  )
                                                                                   (
                                                                                    to-str (
                                                                                      string-append "Encrypted message: " en
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
                                                                                      string-append "Decrypted message: " (
                                                                                        enigma en rotor_pos rotor_sel pb
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    string-append "Decrypted message: " (
                                                                                      enigma en rotor_pos rotor_sel pb
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    to-str (
                                                                                      string-append "Decrypted message: " (
                                                                                        enigma en rotor_pos rotor_sel pb
                                                                                      )
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
                                                    )
                                                     (
                                                      main
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
          end33 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur34 (
              quotient (
                * (
                  - end33 start32
                )
                 1000000
              )
               jps35
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur34
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
