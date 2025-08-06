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
      start76 (
        current-jiffy
      )
    )
     (
      jps79 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          KEY_STRING "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        )
      )
       (
        begin (
          define (
            mod36 n
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                let (
                  (
                    r (
                      modulo n 36
                    )
                  )
                )
                 (
                  begin (
                    if (
                      < r 0
                    )
                     (
                      begin (
                        set! r (
                          + r 36
                        )
                      )
                    )
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
                    ret1 r
                  )
                )
              )
            )
          )
        )
         (
          define (
            gcd a b
          )
           (
            call/cc (
              lambda (
                ret2
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
                                      not (
                                        equal? y 0
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            t y
                                          )
                                        )
                                         (
                                          begin (
                                            set! y (
                                              modulo x y
                                            )
                                          )
                                           (
                                            set! x t
                                          )
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
                        if (
                          < x 0
                        )
                         (
                          begin (
                            set! x (
                              - x
                            )
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        ret2 x
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
            replace_letters letter
          )
           (
            call/cc (
              lambda (
                ret5
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
                                  < i (
                                    _len KEY_STRING
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      string=? (
                                        _substring KEY_STRING i (
                                          + i 1
                                        )
                                      )
                                       letter
                                    )
                                     (
                                      begin (
                                        ret5 i
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
                    ret5 0
                  )
                )
              )
            )
          )
        )
         (
          define (
            replace_digits num
          )
           (
            call/cc (
              lambda (
                ret8
              )
               (
                let (
                  (
                    idx (
                      mod36 num
                    )
                  )
                )
                 (
                  begin (
                    ret8 (
                      _substring KEY_STRING idx (
                        + idx 1
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
            to_upper c
          )
           (
            call/cc (
              lambda (
                ret9
              )
               (
                let (
                  (
                    lower "abcdefghijklmnopqrstuvwxyz"
                  )
                )
                 (
                  begin (
                    let (
                      (
                        upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
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
                                            _len lower
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              string=? c (
                                                _substring lower i (
                                                  + i 1
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                ret9 (
                                                  _substring upper i (
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
                            ret9 c
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
            process_text text break_key
          )
           (
            call/cc (
              lambda (
                ret12
              )
               (
                let (
                  (
                    chars (
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
                                        _len text
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            c (
                                              to_upper (
                                                _substring text i (
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
                                                j 0
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    ok #f
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
                                                                  < j (
                                                                    _len KEY_STRING
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      string=? (
                                                                        _substring KEY_STRING j (
                                                                          + j 1
                                                                        )
                                                                      )
                                                                       c
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! ok #t
                                                                      )
                                                                       (
                                                                        break16 (
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
                                                    if ok (
                                                      begin (
                                                        set! chars (
                                                          append chars (
                                                            _list c
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
                                            )
                                          )
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
                        if (
                          equal? (
                            _len chars
                          )
                           0
                        )
                         (
                          begin (
                            ret12 ""
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
                            last (
                              list-ref chars (
                                - (
                                  _len chars
                                )
                                 1
                              )
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
                                          not (
                                            equal? (
                                              modulo (
                                                _len chars
                                              )
                                               break_key
                                            )
                                             0
                                          )
                                        )
                                         (
                                          begin (
                                            set! chars (
                                              append chars (
                                                _list last
                                              )
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
                            let (
                              (
                                res ""
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
                                                  < k (
                                                    _len chars
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! res (
                                                      string-append res (
                                                        list-ref chars k
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! k (
                                                      + k 1
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
                                    ret12 res
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
            matrix_minor m row col
          )
           (
            call/cc (
              lambda (
                ret21
              )
               (
                let (
                  (
                    res (
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
                                        _len m
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          not (
                                            equal? i row
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                r (
                                                  _list
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
                                                                  < j (
                                                                    _len (
                                                                      list-ref m i
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      not (
                                                                        equal? j col
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! r (
                                                                          append r (
                                                                            _list (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref m i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref m i
                                                                                  )
                                                                                   j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref m i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref m i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref m i
                                                                                  )
                                                                                   j
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
                                                                    set! j (
                                                                      + j 1
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
                                                    set! res (
                                                      append res (
                                                        _list r
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
                                        set! i (
                                          + i 1
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
                        ret21 res
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
            determinant m
          )
           (
            call/cc (
              lambda (
                ret26
              )
               (
                let (
                  (
                    n (
                      _len m
                    )
                  )
                )
                 (
                  begin (
                    if (
                      equal? n 1
                    )
                     (
                      begin (
                        ret26 (
                          cond (
                            (
                              string? (
                                list-ref m 0
                              )
                            )
                             (
                              _substring (
                                list-ref m 0
                              )
                               0 (
                                + 0 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? (
                                list-ref m 0
                              )
                            )
                             (
                              hash-table-ref (
                                list-ref m 0
                              )
                               0
                            )
                          )
                           (
                            else (
                              list-ref (
                                list-ref m 0
                              )
                               0
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
                      equal? n 2
                    )
                     (
                      begin (
                        ret26 (
                          - (
                            * (
                              cond (
                                (
                                  string? (
                                    list-ref m 0
                                  )
                                )
                                 (
                                  _substring (
                                    list-ref m 0
                                  )
                                   0 (
                                    + 0 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? (
                                    list-ref m 0
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    list-ref m 0
                                  )
                                   0
                                )
                              )
                               (
                                else (
                                  list-ref (
                                    list-ref m 0
                                  )
                                   0
                                )
                              )
                            )
                             (
                              cond (
                                (
                                  string? (
                                    list-ref m 1
                                  )
                                )
                                 (
                                  _substring (
                                    list-ref m 1
                                  )
                                   1 (
                                    + 1 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? (
                                    list-ref m 1
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    list-ref m 1
                                  )
                                   1
                                )
                              )
                               (
                                else (
                                  list-ref (
                                    list-ref m 1
                                  )
                                   1
                                )
                              )
                            )
                          )
                           (
                            * (
                              cond (
                                (
                                  string? (
                                    list-ref m 0
                                  )
                                )
                                 (
                                  _substring (
                                    list-ref m 0
                                  )
                                   1 (
                                    + 1 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? (
                                    list-ref m 0
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    list-ref m 0
                                  )
                                   1
                                )
                              )
                               (
                                else (
                                  list-ref (
                                    list-ref m 0
                                  )
                                   1
                                )
                              )
                            )
                             (
                              cond (
                                (
                                  string? (
                                    list-ref m 1
                                  )
                                )
                                 (
                                  _substring (
                                    list-ref m 1
                                  )
                                   0 (
                                    + 0 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? (
                                    list-ref m 1
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    list-ref m 1
                                  )
                                   0
                                )
                              )
                               (
                                else (
                                  list-ref (
                                    list-ref m 1
                                  )
                                   0
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
                    let (
                      (
                        det 0
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            col 0
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
                                          < col n
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                minor_mat (
                                                  matrix_minor m 0 col
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    sign 1
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      equal? (
                                                        modulo col 2
                                                      )
                                                       1
                                                    )
                                                     (
                                                      begin (
                                                        set! sign (
                                                          - 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! det (
                                                      _add det (
                                                        * (
                                                          * sign (
                                                            cond (
                                                              (
                                                                string? (
                                                                  list-ref m 0
                                                                )
                                                              )
                                                               (
                                                                _substring (
                                                                  list-ref m 0
                                                                )
                                                                 col (
                                                                  + col 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? (
                                                                  list-ref m 0
                                                                )
                                                              )
                                                               (
                                                                hash-table-ref (
                                                                  list-ref m 0
                                                                )
                                                                 col
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref (
                                                                  list-ref m 0
                                                                )
                                                                 col
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          determinant minor_mat
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! col (
                                                      + col 1
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
                            ret26 det
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
            cofactor_matrix m
          )
           (
            call/cc (
              lambda (
                ret29
              )
               (
                let (
                  (
                    n (
                      _len m
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
                        let (
                          (
                            i 0
                          )
                        )
                         (
                          begin (
                            call/cc (
                              lambda (
                                break31
                              )
                               (
                                letrec (
                                  (
                                    loop30 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i n
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
                                                    j 0
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
                                                                  < j n
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        minor_mat (
                                                                          matrix_minor m i j
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            det_minor (
                                                                              determinant minor_mat
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                sign 1
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                if (
                                                                                  equal? (
                                                                                    modulo (
                                                                                      + i j
                                                                                    )
                                                                                     2
                                                                                  )
                                                                                   1
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! sign (
                                                                                      - 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  quote (
                                                                                    
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! row (
                                                                                  append row (
                                                                                    _list (
                                                                                      * sign det_minor
                                                                                    )
                                                                                  )
                                                                                )
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
                                                    set! res (
                                                      append res (
                                                        _list row
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
                                            loop30
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
                                  loop30
                                )
                              )
                            )
                          )
                           (
                            ret29 res
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
            transpose m
          )
           (
            call/cc (
              lambda (
                ret34
              )
               (
                let (
                  (
                    rows (
                      _len m
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        cols (
                          _len (
                            list-ref m 0
                          )
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
                            let (
                              (
                                j 0
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
                                              < j cols
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
                                                                      < i rows
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! row (
                                                                          append row (
                                                                            _list (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref m i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref m i
                                                                                  )
                                                                                   j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref m i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref m i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref m i
                                                                                  )
                                                                                   j
                                                                                )
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
                                                        set! res (
                                                          append res (
                                                            _list row
                                                          )
                                                        )
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
         (
          define (
            matrix_mod m
          )
           (
            call/cc (
              lambda (
                ret39
              )
               (
                let (
                  (
                    res (
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
                            break41
                          )
                           (
                            letrec (
                              (
                                loop40 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len m
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
                                                j 0
                                              )
                                            )
                                             (
                                              begin (
                                                call/cc (
                                                  lambda (
                                                    break43
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop42 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              < j (
                                                                _len (
                                                                  list-ref m i
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list (
                                                                      mod36 (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref m i
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref m i
                                                                            )
                                                                             j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref m i
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref m i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref m i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! j (
                                                                  + j 1
                                                                )
                                                              )
                                                               (
                                                                loop42
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
                                                      loop42
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! res (
                                                  append res (
                                                    _list row
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
                                        loop40
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
                              loop40
                            )
                          )
                        )
                      )
                       (
                        ret39 res
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
            scalar_matrix_mult s m
          )
           (
            call/cc (
              lambda (
                ret44
              )
               (
                let (
                  (
                    res (
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
                            break46
                          )
                           (
                            letrec (
                              (
                                loop45 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len m
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
                                                j 0
                                              )
                                            )
                                             (
                                              begin (
                                                call/cc (
                                                  lambda (
                                                    break48
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop47 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              < j (
                                                                _len (
                                                                  list-ref m i
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list (
                                                                      mod36 (
                                                                        * s (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref m i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref m i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref m i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref m i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref m i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! j (
                                                                  + j 1
                                                                )
                                                              )
                                                               (
                                                                loop47
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
                                                      loop47
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! res (
                                                  append res (
                                                    _list row
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
                                        loop45
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
                              loop45
                            )
                          )
                        )
                      )
                       (
                        ret44 res
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
            adjugate m
          )
           (
            call/cc (
              lambda (
                ret49
              )
               (
                let (
                  (
                    cof (
                      cofactor_matrix m
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        n (
                          _len cof
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
                            let (
                              (
                                i 0
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
                                              < i n
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
                                                        j 0
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
                                                                      < j n
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! row (
                                                                          append row (
                                                                            _list (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    cond (
                                                                                      (
                                                                                        string? cof
                                                                                      )
                                                                                       (
                                                                                        _substring cof j (
                                                                                          + j 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? cof
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref cof j
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        list-ref cof j
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    cond (
                                                                                      (
                                                                                        string? cof
                                                                                      )
                                                                                       (
                                                                                        _substring cof j (
                                                                                          + j 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? cof
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref cof j
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        list-ref cof j
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   i (
                                                                                    + i 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    cond (
                                                                                      (
                                                                                        string? cof
                                                                                      )
                                                                                       (
                                                                                        _substring cof j (
                                                                                          + j 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? cof
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref cof j
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        list-ref cof j
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    cond (
                                                                                      (
                                                                                        string? cof
                                                                                      )
                                                                                       (
                                                                                        _substring cof j (
                                                                                          + j 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? cof
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref cof j
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        list-ref cof j
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   i
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    cond (
                                                                                      (
                                                                                        string? cof
                                                                                      )
                                                                                       (
                                                                                        _substring cof j (
                                                                                          + j 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? cof
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref cof j
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        list-ref cof j
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   i
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        set! j (
                                                                          + j 1
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
                                                        set! res (
                                                          append res (
                                                            _list row
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
                                ret49 res
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
            multiply_matrix_vector m v
          )
           (
            call/cc (
              lambda (
                ret54
              )
               (
                let (
                  (
                    n (
                      _len m
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
                        let (
                          (
                            i 0
                          )
                        )
                         (
                          begin (
                            call/cc (
                              lambda (
                                break56
                              )
                               (
                                letrec (
                                  (
                                    loop55 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i n
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                sum 0
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
                                                        break58
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop57 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < j n
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! sum (
                                                                      + sum (
                                                                        * (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref m i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref m i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref m i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref m i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref m i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          list-ref v j
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! j (
                                                                      + j 1
                                                                    )
                                                                  )
                                                                   (
                                                                    loop57
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
                                                          loop57
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! res (
                                                      append res (
                                                        _list (
                                                          mod36 sum
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
                                            loop55
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
                                  loop55
                                )
                              )
                            )
                          )
                           (
                            ret54 res
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
            inverse_key key
          )
           (
            call/cc (
              lambda (
                ret59
              )
               (
                let (
                  (
                    det_val (
                      determinant key
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        det_mod (
                          mod36 det_val
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            det_inv 0
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
                                    break61
                                  )
                                   (
                                    letrec (
                                      (
                                        loop60 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i 36
                                            )
                                             (
                                              begin (
                                                if (
                                                  equal? (
                                                    fmod (
                                                      * det_mod i
                                                    )
                                                     36
                                                  )
                                                   1
                                                )
                                                 (
                                                  begin (
                                                    set! det_inv i
                                                  )
                                                   (
                                                    break61 (
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
                                                set! i (
                                                  + i 1
                                                )
                                              )
                                               (
                                                loop60
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
                                      loop60
                                    )
                                  )
                                )
                              )
                               (
                                let (
                                  (
                                    adj (
                                      adjugate key
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        tmp (
                                          scalar_matrix_mult det_inv adj
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            res (
                                              matrix_mod tmp
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            ret59 res
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
            hill_encrypt key text
          )
           (
            call/cc (
              lambda (
                ret62
              )
               (
                let (
                  (
                    break_key (
                      _len key
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        processed (
                          process_text text break_key
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            encrypted ""
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
                                    break64
                                  )
                                   (
                                    letrec (
                                      (
                                        loop63 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i (
                                                _len processed
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    vec (
                                                      _list
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
                                                            break66
                                                          )
                                                           (
                                                            letrec (
                                                              (
                                                                loop65 (
                                                                  lambda (
                                                                    
                                                                  )
                                                                   (
                                                                    if (
                                                                      < j break_key
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! vec (
                                                                          append vec (
                                                                            _list (
                                                                              replace_letters (
                                                                                cond (
                                                                                  (
                                                                                    string? processed
                                                                                  )
                                                                                   (
                                                                                    _substring processed (
                                                                                      + i j
                                                                                    )
                                                                                     (
                                                                                      + (
                                                                                        + i j
                                                                                      )
                                                                                       1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? processed
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref processed (
                                                                                      + i j
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref processed (
                                                                                      + i j
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        set! j (
                                                                          + j 1
                                                                        )
                                                                      )
                                                                       (
                                                                        loop65
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
                                                              loop65
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        let (
                                                          (
                                                            enc_vec (
                                                              multiply_matrix_vector key vec
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
                                                                    break68
                                                                  )
                                                                   (
                                                                    letrec (
                                                                      (
                                                                        loop67 (
                                                                          lambda (
                                                                            
                                                                          )
                                                                           (
                                                                            if (
                                                                              < k break_key
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! encrypted (
                                                                                  string-append encrypted (
                                                                                    replace_digits (
                                                                                      cond (
                                                                                        (
                                                                                          string? enc_vec
                                                                                        )
                                                                                         (
                                                                                          _substring enc_vec k (
                                                                                            + k 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? enc_vec
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref enc_vec k
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref enc_vec k
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! k (
                                                                                  + k 1
                                                                                )
                                                                              )
                                                                               (
                                                                                loop67
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
                                                                      loop67
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! i (
                                                                  + i break_key
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
                                                loop63
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
                                      loop63
                                    )
                                  )
                                )
                              )
                               (
                                ret62 encrypted
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
            hill_decrypt key text
          )
           (
            call/cc (
              lambda (
                ret69
              )
               (
                let (
                  (
                    break_key (
                      _len key
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        decrypt_key (
                          inverse_key key
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            processed (
                              process_text text break_key
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                decrypted ""
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
                                        break71
                                      )
                                       (
                                        letrec (
                                          (
                                            loop70 (
                                              lambda (
                                                
                                              )
                                               (
                                                if (
                                                  < i (
                                                    _len processed
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        vec (
                                                          _list
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
                                                                break73
                                                              )
                                                               (
                                                                letrec (
                                                                  (
                                                                    loop72 (
                                                                      lambda (
                                                                        
                                                                      )
                                                                       (
                                                                        if (
                                                                          < j break_key
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! vec (
                                                                              append vec (
                                                                                _list (
                                                                                  replace_letters (
                                                                                    cond (
                                                                                      (
                                                                                        string? processed
                                                                                      )
                                                                                       (
                                                                                        _substring processed (
                                                                                          + i j
                                                                                        )
                                                                                         (
                                                                                          + (
                                                                                            + i j
                                                                                          )
                                                                                           1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? processed
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref processed (
                                                                                          + i j
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        list-ref processed (
                                                                                          + i j
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                           (
                                                                            loop72
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
                                                                  loop72
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            let (
                                                              (
                                                                dec_vec (
                                                                  multiply_matrix_vector decrypt_key vec
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
                                                                        break75
                                                                      )
                                                                       (
                                                                        letrec (
                                                                          (
                                                                            loop74 (
                                                                              lambda (
                                                                                
                                                                              )
                                                                               (
                                                                                if (
                                                                                  < k break_key
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! decrypted (
                                                                                      string-append decrypted (
                                                                                        replace_digits (
                                                                                          cond (
                                                                                            (
                                                                                              string? dec_vec
                                                                                            )
                                                                                             (
                                                                                              _substring dec_vec k (
                                                                                                + k 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? dec_vec
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref dec_vec k
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref dec_vec k
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! k (
                                                                                      + k 1
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    loop74
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
                                                                          loop74
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! i (
                                                                      + i break_key
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
                                                    loop70
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
                                          loop70
                                        )
                                      )
                                    )
                                  )
                                   (
                                    ret69 decrypted
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
              key (
                _list (
                  _list 2 5
                )
                 (
                  _list 1 6
                )
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    hill_encrypt key "testing hill cipher"
                  )
                )
                 (
                  hill_encrypt key "testing hill cipher"
                )
                 (
                  to-str (
                    hill_encrypt key "testing hill cipher"
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
                    hill_encrypt key "hello"
                  )
                )
                 (
                  hill_encrypt key "hello"
                )
                 (
                  to-str (
                    hill_encrypt key "hello"
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
                    hill_decrypt key "WHXYJOLM9C6XT085LL"
                  )
                )
                 (
                  hill_decrypt key "WHXYJOLM9C6XT085LL"
                )
                 (
                  to-str (
                    hill_decrypt key "WHXYJOLM9C6XT085LL"
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
                    hill_decrypt key "85FF00"
                  )
                )
                 (
                  hill_decrypt key "85FF00"
                )
                 (
                  to-str (
                    hill_decrypt key "85FF00"
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
          end77 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur78 (
              quotient (
                * (
                  - end77 start76
                )
                 1000000
              )
               jps79
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur78
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
