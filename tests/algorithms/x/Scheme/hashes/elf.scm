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
      start22 (
        current-jiffy
      )
    )
     (
      jps25 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          ascii " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
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
                                    _len ascii
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      string=? (
                                        _substring ascii i (
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
            bit_and a b
          )
           (
            call/cc (
              lambda (
                ret4
              )
               (
                let (
                  (
                    ua a
                  )
                )
                 (
                  begin (
                    let (
                      (
                        ub b
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
                                              or (
                                                > ua 0
                                              )
                                               (
                                                > ub 0
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  and (
                                                    equal? (
                                                      _mod ua 2
                                                    )
                                                     1
                                                  )
                                                   (
                                                    equal? (
                                                      _mod ub 2
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
                                                set! ua (
                                                  let (
                                                    (
                                                      v7 (
                                                        _div ua 2
                                                      )
                                                    )
                                                  )
                                                   (
                                                    cond (
                                                      (
                                                        string? v7
                                                      )
                                                       (
                                                        inexact->exact (
                                                          floor (
                                                            string->number v7
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        boolean? v7
                                                      )
                                                       (
                                                        if v7 1 0
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        inexact->exact (
                                                          floor v7
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! ub (
                                                  let (
                                                    (
                                                      v8 (
                                                        _div ub 2
                                                      )
                                                    )
                                                  )
                                                   (
                                                    cond (
                                                      (
                                                        string? v8
                                                      )
                                                       (
                                                        inexact->exact (
                                                          floor (
                                                            string->number v8
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        boolean? v8
                                                      )
                                                       (
                                                        if v8 1 0
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        inexact->exact (
                                                          floor v8
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! bit (
                                                  * bit 2
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
         (
          define (
            bit_xor a b
          )
           (
            call/cc (
              lambda (
                ret9
              )
               (
                let (
                  (
                    ua a
                  )
                )
                 (
                  begin (
                    let (
                      (
                        ub b
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
                                              or (
                                                > ua 0
                                              )
                                               (
                                                > ub 0
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    abit (
                                                      _mod ua 2
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        bbit (
                                                          _mod ub 2
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          not (
                                                            equal? abit bbit
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
                                                        set! ua (
                                                          let (
                                                            (
                                                              v12 (
                                                                _div ua 2
                                                              )
                                                            )
                                                          )
                                                           (
                                                            cond (
                                                              (
                                                                string? v12
                                                              )
                                                               (
                                                                inexact->exact (
                                                                  floor (
                                                                    string->number v12
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                boolean? v12
                                                              )
                                                               (
                                                                if v12 1 0
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                inexact->exact (
                                                                  floor v12
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! ub (
                                                          let (
                                                            (
                                                              v13 (
                                                                _div ub 2
                                                              )
                                                            )
                                                          )
                                                           (
                                                            cond (
                                                              (
                                                                string? v13
                                                              )
                                                               (
                                                                inexact->exact (
                                                                  floor (
                                                                    string->number v13
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                boolean? v13
                                                              )
                                                               (
                                                                if v13 1 0
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                inexact->exact (
                                                                  floor v13
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! bit (
                                                          * bit 2
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
              )
            )
          )
        )
         (
          define (
            bit_not32 x
          )
           (
            call/cc (
              lambda (
                ret14
              )
               (
                let (
                  (
                    ux x
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
                                count 0
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
                                              < count 32
                                            )
                                             (
                                              begin (
                                                if (
                                                  equal? (
                                                    _mod ux 2
                                                  )
                                                   0
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
                                                set! ux (
                                                  let (
                                                    (
                                                      v17 (
                                                        _div ux 2
                                                      )
                                                    )
                                                  )
                                                   (
                                                    cond (
                                                      (
                                                        string? v17
                                                      )
                                                       (
                                                        inexact->exact (
                                                          floor (
                                                            string->number v17
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        boolean? v17
                                                      )
                                                       (
                                                        if v17 1 0
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        inexact->exact (
                                                          floor v17
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! bit (
                                                  * bit 2
                                                )
                                              )
                                               (
                                                set! count (
                                                  + count 1
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
                                ret14 res
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
            elf_hash data
          )
           (
            call/cc (
              lambda (
                ret18
              )
               (
                let (
                  (
                    hash_ 0
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
                                      < i (
                                        _len data
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            c (
                                              ord (
                                                _substring data i (
                                                  + i 1
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! hash_ (
                                              _add (
                                                * hash_ 16
                                              )
                                               c
                                            )
                                          )
                                           (
                                            let (
                                              (
                                                x (
                                                  bit_and hash_ 4026531840
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  not (
                                                    equal? x 0
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! hash_ (
                                                      bit_xor hash_ (
                                                        let (
                                                          (
                                                            v21 (
                                                              _div x 16777216
                                                            )
                                                          )
                                                        )
                                                         (
                                                          cond (
                                                            (
                                                              string? v21
                                                            )
                                                             (
                                                              inexact->exact (
                                                                floor (
                                                                  string->number v21
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              boolean? v21
                                                            )
                                                             (
                                                              if v21 1 0
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              inexact->exact (
                                                                floor v21
                                                              )
                                                            )
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
                                                set! hash_ (
                                                  bit_and hash_ (
                                                    bit_not32 x
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
                        ret18 hash_
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
                to-str-space (
                  elf_hash "lorem ipsum"
                )
              )
            )
             (
              to-str-space (
                elf_hash "lorem ipsum"
              )
            )
             (
              to-str (
                to-str-space (
                  elf_hash "lorem ipsum"
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
     (
      let (
        (
          end23 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur24 (
              quotient (
                * (
                  - end23 start22
                )
                 1000000
              )
               jps25
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur24
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
