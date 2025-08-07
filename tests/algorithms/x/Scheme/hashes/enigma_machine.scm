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
      start23 (
        current-jiffy
      )
    )
     (
      jps26 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          ASCII " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}"
        )
      )
       (
        begin (
          define (
            build_alphabet
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                let (
                  (
                    result (
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
                                        set! result (
                                          append result (
                                            _list (
                                              _substring ASCII i (
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
                        ret1 result
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
            range_list n
          )
           (
            call/cc (
              lambda (
                ret4
              )
               (
                let (
                  (
                    lst (
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
                                      < i n
                                    )
                                     (
                                      begin (
                                        set! lst (
                                          append lst (
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
                        ret4 lst
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
            reversed_range_list n
          )
           (
            call/cc (
              lambda (
                ret7
              )
               (
                let (
                  (
                    lst (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        i (
                          - n 1
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
                                      >= i 0
                                    )
                                     (
                                      begin (
                                        set! lst (
                                          append lst (
                                            _list i
                                          )
                                        )
                                      )
                                       (
                                        set! i (
                                          - i 1
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
                        ret7 lst
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
            index_of_char lst ch
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
                                    _len lst
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      string=? (
                                        list-ref-safe lst i
                                      )
                                       ch
                                    )
                                     (
                                      begin (
                                        ret10 i
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
                    ret10 (
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
            index_of_int lst value
          )
           (
            call/cc (
              lambda (
                ret13
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
                                    _len lst
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? (
                                        list-ref-safe lst i
                                      )
                                       value
                                    )
                                     (
                                      begin (
                                        ret13 i
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
                    ret13 (
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
            enigma_encrypt message token
          )
           (
            call/cc (
              lambda (
                ret16
              )
               (
                let (
                  (
                    alphabets (
                      build_alphabet
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        n (
                          _len alphabets
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            gear_one (
                              range_list n
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                gear_two (
                                  range_list n
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    gear_three (
                                      range_list n
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        reflector (
                                          reversed_range_list n
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            gear_one_pos 0
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                gear_two_pos 0
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    gear_three_pos 0
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    define (
                                                      rotator
                                                    )
                                                     (
                                                      call/cc (
                                                        lambda (
                                                          ret17
                                                        )
                                                         (
                                                          let (
                                                            (
                                                              i (
                                                                cond (
                                                                  (
                                                                    string? gear_one
                                                                  )
                                                                   (
                                                                    _substring gear_one 0 (
                                                                      + 0 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? gear_one
                                                                  )
                                                                   (
                                                                    hash-table-ref gear_one 0
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref-safe gear_one 0
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              set! gear_one (
                                                                if (
                                                                  string? gear_one
                                                                )
                                                                 (
                                                                  _substring gear_one 1 (
                                                                    _len gear_one
                                                                  )
                                                                )
                                                                 (
                                                                  take (
                                                                    drop gear_one 1
                                                                  )
                                                                   (
                                                                    - (
                                                                      _len gear_one
                                                                    )
                                                                     1
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              set! gear_one (
                                                                append gear_one (
                                                                  _list i
                                                                )
                                                              )
                                                            )
                                                             (
                                                              set! gear_one_pos (
                                                                + gear_one_pos 1
                                                              )
                                                            )
                                                             (
                                                              if (
                                                                equal? (
                                                                  _mod gear_one_pos n
                                                                )
                                                                 0
                                                              )
                                                               (
                                                                begin (
                                                                  set! i (
                                                                    cond (
                                                                      (
                                                                        string? gear_two
                                                                      )
                                                                       (
                                                                        _substring gear_two 0 (
                                                                          + 0 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? gear_two
                                                                      )
                                                                       (
                                                                        hash-table-ref gear_two 0
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref-safe gear_two 0
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  set! gear_two (
                                                                    if (
                                                                      string? gear_two
                                                                    )
                                                                     (
                                                                      _substring gear_two 1 (
                                                                        _len gear_two
                                                                      )
                                                                    )
                                                                     (
                                                                      take (
                                                                        drop gear_two 1
                                                                      )
                                                                       (
                                                                        - (
                                                                          _len gear_two
                                                                        )
                                                                         1
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  set! gear_two (
                                                                    append gear_two (
                                                                      _list i
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  set! gear_two_pos (
                                                                    + gear_two_pos 1
                                                                  )
                                                                )
                                                                 (
                                                                  if (
                                                                    equal? (
                                                                      _mod gear_two_pos n
                                                                    )
                                                                     0
                                                                  )
                                                                   (
                                                                    begin (
                                                                      set! i (
                                                                        cond (
                                                                          (
                                                                            string? gear_three
                                                                          )
                                                                           (
                                                                            _substring gear_three 0 (
                                                                              + 0 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? gear_three
                                                                          )
                                                                           (
                                                                            hash-table-ref gear_three 0
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref-safe gear_three 0
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      set! gear_three (
                                                                        if (
                                                                          string? gear_three
                                                                        )
                                                                         (
                                                                          _substring gear_three 1 (
                                                                            _len gear_three
                                                                          )
                                                                        )
                                                                         (
                                                                          take (
                                                                            drop gear_three 1
                                                                          )
                                                                           (
                                                                            - (
                                                                              _len gear_three
                                                                            )
                                                                             1
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      set! gear_three (
                                                                        append gear_three (
                                                                          _list i
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      set! gear_three_pos (
                                                                        + gear_three_pos 1
                                                                      )
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
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    define (
                                                      engine ch
                                                    )
                                                     (
                                                      call/cc (
                                                        lambda (
                                                          ret18
                                                        )
                                                         (
                                                          let (
                                                            (
                                                              target (
                                                                index_of_char alphabets ch
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              set! target (
                                                                cond (
                                                                  (
                                                                    string? gear_one
                                                                  )
                                                                   (
                                                                    _substring gear_one target (
                                                                      + target 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? gear_one
                                                                  )
                                                                   (
                                                                    hash-table-ref gear_one target
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref-safe gear_one target
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              set! target (
                                                                cond (
                                                                  (
                                                                    string? gear_two
                                                                  )
                                                                   (
                                                                    _substring gear_two target (
                                                                      + target 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? gear_two
                                                                  )
                                                                   (
                                                                    hash-table-ref gear_two target
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref-safe gear_two target
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              set! target (
                                                                cond (
                                                                  (
                                                                    string? gear_three
                                                                  )
                                                                   (
                                                                    _substring gear_three target (
                                                                      + target 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? gear_three
                                                                  )
                                                                   (
                                                                    hash-table-ref gear_three target
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref-safe gear_three target
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              set! target (
                                                                cond (
                                                                  (
                                                                    string? reflector
                                                                  )
                                                                   (
                                                                    _substring reflector target (
                                                                      + target 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? reflector
                                                                  )
                                                                   (
                                                                    hash-table-ref reflector target
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref-safe reflector target
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              set! target (
                                                                index_of_int gear_three target
                                                              )
                                                            )
                                                             (
                                                              set! target (
                                                                index_of_int gear_two target
                                                              )
                                                            )
                                                             (
                                                              set! target (
                                                                index_of_int gear_one target
                                                              )
                                                            )
                                                             (
                                                              rotator
                                                            )
                                                             (
                                                              ret18 (
                                                                cond (
                                                                  (
                                                                    string? alphabets
                                                                  )
                                                                   (
                                                                    _substring alphabets target (
                                                                      + target 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? alphabets
                                                                  )
                                                                   (
                                                                    hash-table-ref alphabets target
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref-safe alphabets target
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
                                                        t 0
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
                                                                      < t token
                                                                    )
                                                                     (
                                                                      begin (
                                                                        rotator
                                                                      )
                                                                       (
                                                                        set! t (
                                                                          + t 1
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
                                                        let (
                                                          (
                                                            result ""
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
                                                                              < idx (
                                                                                _len message
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! result (
                                                                                  string-append result (
                                                                                    engine (
                                                                                      _substring message idx (
                                                                                        + idx 1
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
                                                                ret16 result
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
              message "HELLO WORLD"
            )
          )
           (
            begin (
              let (
                (
                  token 123
                )
              )
               (
                begin (
                  let (
                    (
                      encoded (
                        enigma_encrypt message token
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
          end24 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur25 (
              quotient (
                * (
                  - end24 start23
                )
                 1000000
              )
               jps26
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur25
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
