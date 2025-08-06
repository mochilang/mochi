;; Generated on 2025-08-06 23:57 +0700
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
      start39 (
        current-jiffy
      )
    )
     (
      jps42 (
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
            new_bloom size
          )
           (
            call/cc (
              lambda (
                ret4
              )
               (
                let (
                  (
                    bits (
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
                                      < i size
                                    )
                                     (
                                      begin (
                                        set! bits (
                                          append bits (
                                            _list 0
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
                          alist->hash-table (
                            _list (
                              cons "size" size
                            )
                             (
                              cons "bits" bits
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
            hash1 value size
          )
           (
            call/cc (
              lambda (
                ret7
              )
               (
                let (
                  (
                    h 0
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
                                        _len value
                                      )
                                    )
                                     (
                                      begin (
                                        set! h (
                                          _mod (
                                            _add (
                                              * h 31
                                            )
                                             (
                                              ord (
                                                _substring value i (
                                                  + i 1
                                                )
                                              )
                                            )
                                          )
                                           size
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
                        ret7 h
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
            hash2 value size
          )
           (
            call/cc (
              lambda (
                ret10
              )
               (
                let (
                  (
                    h 0
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
                                        _len value
                                      )
                                    )
                                     (
                                      begin (
                                        set! h (
                                          _mod (
                                            _add (
                                              * h 131
                                            )
                                             (
                                              ord (
                                                _substring value i (
                                                  + i 1
                                                )
                                              )
                                            )
                                          )
                                           size
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
                        ret10 h
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
            hash_positions value size
          )
           (
            call/cc (
              lambda (
                ret13
              )
               (
                let (
                  (
                    h1 (
                      hash1 value size
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        h2 (
                          hash2 value size
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            res (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            set! res (
                              append res (
                                _list h1
                              )
                            )
                          )
                           (
                            set! res (
                              append res (
                                _list h2
                              )
                            )
                          )
                           (
                            ret13 res
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
            bloom_add b value
          )
           (
            call/cc (
              lambda (
                ret14
              )
               (
                let (
                  (
                    pos (
                      hash_positions value (
                        hash-table-ref b "size"
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        bits (
                          hash-table-ref b "bits"
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
                                            _len pos
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                idx (
                                                  - (
                                                    - (
                                                      hash-table-ref b "size"
                                                    )
                                                     1
                                                  )
                                                   (
                                                    cond (
                                                      (
                                                        string? pos
                                                      )
                                                       (
                                                        _substring pos i (
                                                          + i 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        hash-table? pos
                                                      )
                                                       (
                                                        hash-table-ref pos i
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        list-ref pos i
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                list-set! bits idx 1
                                              )
                                               (
                                                set! i (
                                                  + i 1
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
                            ret14 (
                              alist->hash-table (
                                _list (
                                  cons "size" (
                                    hash-table-ref b "size"
                                  )
                                )
                                 (
                                  cons "bits" bits
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
            bloom_exists b value
          )
           (
            call/cc (
              lambda (
                ret17
              )
               (
                let (
                  (
                    pos (
                      hash_positions value (
                        hash-table-ref b "size"
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
                                        _len pos
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            idx (
                                              - (
                                                - (
                                                  hash-table-ref b "size"
                                                )
                                                 1
                                              )
                                               (
                                                cond (
                                                  (
                                                    string? pos
                                                  )
                                                   (
                                                    _substring pos i (
                                                      + i 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? pos
                                                  )
                                                   (
                                                    hash-table-ref pos i
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref pos i
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              not (
                                                equal? (
                                                  list-ref (
                                                    hash-table-ref b "bits"
                                                  )
                                                   idx
                                                )
                                                 1
                                              )
                                            )
                                             (
                                              begin (
                                                ret17 #f
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
                        ret17 #t
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
            bitstring b
          )
           (
            call/cc (
              lambda (
                ret20
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
                                        hash-table-ref b "size"
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          string-append res (
                                            to-str-space (
                                              list-ref (
                                                hash-table-ref b "bits"
                                              )
                                               i
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
                        ret20 res
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
            format_hash b value
          )
           (
            call/cc (
              lambda (
                ret23
              )
               (
                let (
                  (
                    pos (
                      hash_positions value (
                        hash-table-ref b "size"
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        bits (
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
                                            hash-table-ref b "size"
                                          )
                                        )
                                         (
                                          begin (
                                            set! bits (
                                              append bits (
                                                _list 0
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
                            set! i 0
                          )
                           (
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
                                            _len pos
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                idx (
                                                  - (
                                                    - (
                                                      hash-table-ref b "size"
                                                    )
                                                     1
                                                  )
                                                   (
                                                    cond (
                                                      (
                                                        string? pos
                                                      )
                                                       (
                                                        _substring pos i (
                                                          + i 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        hash-table? pos
                                                      )
                                                       (
                                                        hash-table-ref pos i
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        list-ref pos i
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                list-set! bits idx 1
                                              )
                                               (
                                                set! i (
                                                  + i 1
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
                                    break29
                                  )
                                   (
                                    letrec (
                                      (
                                        loop28 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i (
                                                hash-table-ref b "size"
                                              )
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  string-append res (
                                                    to-str-space (
                                                      list-ref bits i
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
                                                loop28
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
                                      loop28
                                    )
                                  )
                                )
                              )
                               (
                                ret23 res
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
            estimated_error_rate b
          )
           (
            call/cc (
              lambda (
                ret30
              )
               (
                let (
                  (
                    ones 0
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
                            break32
                          )
                           (
                            letrec (
                              (
                                loop31 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        hash-table-ref b "size"
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          equal? (
                                            list-ref (
                                              hash-table-ref b "bits"
                                            )
                                             i
                                          )
                                           1
                                        )
                                         (
                                          begin (
                                            set! ones (
                                              + ones 1
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
                                        loop31
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
                              loop31
                            )
                          )
                        )
                      )
                       (
                        let (
                          (
                            frac (
                              _div (
                                + 0.0 ones
                              )
                               (
                                + 0.0 (
                                  hash-table-ref b "size"
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            ret30 (
                              * frac frac
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
            any_in b items
          )
           (
            call/cc (
              lambda (
                ret33
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
                        break35
                      )
                       (
                        letrec (
                          (
                            loop34 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len items
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      bloom_exists b (
                                        list-ref items i
                                      )
                                    )
                                     (
                                      begin (
                                        ret33 #t
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
                                    loop34
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
                          loop34
                        )
                      )
                    )
                  )
                   (
                    ret33 #f
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
                ret36
              )
               (
                let (
                  (
                    bloom (
                      new_bloom 8
                    )
                  )
                )
                 (
                  begin (
                    _display (
                      if (
                        string? (
                          bitstring bloom
                        )
                      )
                       (
                        bitstring bloom
                      )
                       (
                        to-str (
                          bitstring bloom
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
                          bloom_exists bloom "Titanic"
                        )
                      )
                       (
                        bloom_exists bloom "Titanic"
                      )
                       (
                        to-str (
                          bloom_exists bloom "Titanic"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! bloom (
                      bloom_add bloom "Titanic"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          bitstring bloom
                        )
                      )
                       (
                        bitstring bloom
                      )
                       (
                        to-str (
                          bitstring bloom
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
                          bloom_exists bloom "Titanic"
                        )
                      )
                       (
                        bloom_exists bloom "Titanic"
                      )
                       (
                        to-str (
                          bloom_exists bloom "Titanic"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! bloom (
                      bloom_add bloom "Avatar"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          bloom_exists bloom "Avatar"
                        )
                      )
                       (
                        bloom_exists bloom "Avatar"
                      )
                       (
                        to-str (
                          bloom_exists bloom "Avatar"
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
                          format_hash bloom "Avatar"
                        )
                      )
                       (
                        format_hash bloom "Avatar"
                      )
                       (
                        to-str (
                          format_hash bloom "Avatar"
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
                          bitstring bloom
                        )
                      )
                       (
                        bitstring bloom
                      )
                       (
                        to-str (
                          bitstring bloom
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    let (
                      (
                        not_present (
                          _list "The Godfather" "Interstellar" "Parasite" "Pulp Fiction"
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
                                break38
                              )
                               (
                                letrec (
                                  (
                                    loop37 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i (
                                            _len not_present
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                film (
                                                  list-ref not_present i
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                _display (
                                                  if (
                                                    string? (
                                                      string-append (
                                                        string-append film ":"
                                                      )
                                                       (
                                                        format_hash bloom film
                                                      )
                                                    )
                                                  )
                                                   (
                                                    string-append (
                                                      string-append film ":"
                                                    )
                                                     (
                                                      format_hash bloom film
                                                    )
                                                  )
                                                   (
                                                    to-str (
                                                      string-append (
                                                        string-append film ":"
                                                      )
                                                       (
                                                        format_hash bloom film
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                newline
                                              )
                                               (
                                                set! i (
                                                  + i 1
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop37
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
                                  loop37
                                )
                              )
                            )
                          )
                           (
                            _display (
                              if (
                                string? (
                                  any_in bloom not_present
                                )
                              )
                               (
                                any_in bloom not_present
                              )
                               (
                                to-str (
                                  any_in bloom not_present
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
                                  bloom_exists bloom "Ratatouille"
                                )
                              )
                               (
                                bloom_exists bloom "Ratatouille"
                              )
                               (
                                to-str (
                                  bloom_exists bloom "Ratatouille"
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
                                  format_hash bloom "Ratatouille"
                                )
                              )
                               (
                                format_hash bloom "Ratatouille"
                              )
                               (
                                to-str (
                                  format_hash bloom "Ratatouille"
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
                                  to-str-space (
                                    estimated_error_rate bloom
                                  )
                                )
                              )
                               (
                                to-str-space (
                                  estimated_error_rate bloom
                                )
                              )
                               (
                                to-str (
                                  to-str-space (
                                    estimated_error_rate bloom
                                  )
                                )
                              )
                            )
                          )
                           (
                            newline
                          )
                           (
                            set! bloom (
                              bloom_add bloom "The Godfather"
                            )
                          )
                           (
                            _display (
                              if (
                                string? (
                                  to-str-space (
                                    estimated_error_rate bloom
                                  )
                                )
                              )
                               (
                                to-str-space (
                                  estimated_error_rate bloom
                                )
                              )
                               (
                                to-str (
                                  to-str-space (
                                    estimated_error_rate bloom
                                  )
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
                                  bitstring bloom
                                )
                              )
                               (
                                bitstring bloom
                              )
                               (
                                to-str (
                                  bitstring bloom
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
          end40 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur41 (
              quotient (
                * (
                  - end40 start39
                )
                 1000000
              )
               jps42
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur41
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
