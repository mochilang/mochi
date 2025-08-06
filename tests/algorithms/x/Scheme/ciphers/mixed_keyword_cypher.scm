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
      start35 (
        current-jiffy
      )
    )
     (
      jps38 (
        jiffies-per-second
      )
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
          let (
            (
              LOWER "abcdefghijklmnopqrstuvwxyz"
            )
          )
           (
            begin (
              define (
                to_upper s
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
                                                                          string=? ch (
                                                                            _substring LOWER j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! res (
                                                                              string-append res (
                                                                                _substring UPPER j (
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
                                                          eq? found #f
                                                        )
                                                         (
                                                          begin (
                                                            set! res (
                                                              string-append res ch
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
                contains xs x
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
                                            ret6 #t
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
                        ret6 #f
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
                    ret9
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
                                            ret9 #t
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
                        ret9 #f
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                get_value keys values key
              )
               (
                call/cc (
                  lambda (
                    ret12
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
                                      < i (
                                        _len keys
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          string=? (
                                            list-ref keys i
                                          )
                                           key
                                        )
                                         (
                                          begin (
                                            ret12 (
                                              list-ref values i
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
                        ret12 (
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
                print_mapping keys values
              )
               (
                call/cc (
                  lambda (
                    ret15
                  )
                   (
                    let (
                      (
                        s "{"
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
                                          < i (
                                            _len keys
                                          )
                                        )
                                         (
                                          begin (
                                            set! s (
                                              string-append (
                                                string-append (
                                                  string-append (
                                                    string-append (
                                                      string-append s "'"
                                                    )
                                                     (
                                                      list-ref keys i
                                                    )
                                                  )
                                                   "': '"
                                                )
                                                 (
                                                  list-ref values i
                                                )
                                              )
                                               "'"
                                            )
                                          )
                                           (
                                            if (
                                              < (
                                                + i 1
                                              )
                                               (
                                                _len keys
                                              )
                                            )
                                             (
                                              begin (
                                                set! s (
                                                  string-append s ", "
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
                            set! s (
                              string-append s "}"
                            )
                          )
                           (
                            _display (
                              if (
                                string? s
                              )
                               s (
                                to-str s
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
             (
              define (
                mixed_keyword keyword plaintext verbose
              )
               (
                call/cc (
                  lambda (
                    ret18
                  )
                   (
                    let (
                      (
                        alphabet UPPER
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            keyword_u (
                              to_upper keyword
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                plaintext_u (
                                  to_upper plaintext
                                )
                              )
                            )
                             (
                              begin (
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
                                                        _len keyword_u
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            ch (
                                                              cond (
                                                                (
                                                                  string? keyword_u
                                                                )
                                                                 (
                                                                  _substring keyword_u i (
                                                                    + i 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                (
                                                                  hash-table? keyword_u
                                                                )
                                                                 (
                                                                  hash-table-ref keyword_u i
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref keyword_u i
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              and (
                                                                contains_char alphabet ch
                                                              )
                                                               (
                                                                eq? (
                                                                  contains unique ch
                                                                )
                                                                 #f
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! unique (
                                                                  append unique (
                                                                    _list ch
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
                                            num_unique (
                                              _len unique
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                shifted (
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
                                                                _len unique
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! shifted (
                                                                  append shifted (
                                                                    _list (
                                                                      list-ref unique i
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
                                                                _len alphabet
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    ch (
                                                                      _substring alphabet i (
                                                                        + i 1
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      eq? (
                                                                        contains unique ch
                                                                      )
                                                                       #f
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! shifted (
                                                                          append shifted (
                                                                            _list ch
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
                                                    modified (
                                                      _list
                                                    )
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
                                                                      < k (
                                                                        _len shifted
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            row (
                                                                              _list
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                r 0
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
                                                                                              and (
                                                                                                < r num_unique
                                                                                              )
                                                                                               (
                                                                                                < (
                                                                                                  + k r
                                                                                                )
                                                                                                 (
                                                                                                  _len shifted
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! row (
                                                                                                  append row (
                                                                                                    _list (
                                                                                                      list-ref shifted (
                                                                                                        + k r
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                set! r (
                                                                                                  + r 1
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
                                                                                set! modified (
                                                                                  append modified (
                                                                                    _list row
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! k (
                                                                                  + k num_unique
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
                                                        let (
                                                          (
                                                            keys (
                                                              _list
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                values (
                                                                  _list
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    column 0
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        letter_index 0
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
                                                                                      < column num_unique
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            row_idx 0
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
                                                                                                          < row_idx (
                                                                                                            _len modified
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            let (
                                                                                                              (
                                                                                                                row (
                                                                                                                  list-ref modified row_idx
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                if (
                                                                                                                  <= (
                                                                                                                    _len row
                                                                                                                  )
                                                                                                                   column
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    break32 (
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
                                                                                                                set! keys (
                                                                                                                  append keys (
                                                                                                                    _list (
                                                                                                                      _substring alphabet letter_index (
                                                                                                                        + letter_index 1
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                set! values (
                                                                                                                  append values (
                                                                                                                    _list (
                                                                                                                      list-ref row column
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                set! letter_index (
                                                                                                                  + letter_index 1
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                set! row_idx (
                                                                                                                  + row_idx 1
                                                                                                                )
                                                                                                              )
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
                                                                                            set! column (
                                                                                              + column 1
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
                                                                        if verbose (
                                                                          begin (
                                                                            print_mapping keys values
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
                                                                            result ""
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! i 0
                                                                          )
                                                                           (
                                                                            call/cc (
                                                                              lambda (
                                                                                break34
                                                                              )
                                                                               (
                                                                                letrec (
                                                                                  (
                                                                                    loop33 (
                                                                                      lambda (
                                                                                        
                                                                                      )
                                                                                       (
                                                                                        if (
                                                                                          < i (
                                                                                            _len plaintext_u
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                ch (
                                                                                                  cond (
                                                                                                    (
                                                                                                      string? plaintext_u
                                                                                                    )
                                                                                                     (
                                                                                                      _substring plaintext_u i (
                                                                                                        + i 1
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    (
                                                                                                      hash-table? plaintext_u
                                                                                                    )
                                                                                                     (
                                                                                                      hash-table-ref plaintext_u i
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    else (
                                                                                                      list-ref plaintext_u i
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                let (
                                                                                                  (
                                                                                                    mapped (
                                                                                                      get_value keys values ch
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    if (
                                                                                                      equal? mapped (
                                                                                                        quote (
                                                                                                          
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        set! result (
                                                                                                          string-append result ch
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        set! result (
                                                                                                          string-append result mapped
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
                                                                                            loop33
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
                                                                                  loop33
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            ret18 result
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
              _display (
                if (
                  string? (
                    mixed_keyword "college" "UNIVERSITY" #t
                  )
                )
                 (
                  mixed_keyword "college" "UNIVERSITY" #t
                )
                 (
                  to-str (
                    mixed_keyword "college" "UNIVERSITY" #t
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
          end36 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur37 (
              quotient (
                * (
                  - end36 start35
                )
                 1000000
              )
               jps38
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur37
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
