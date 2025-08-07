;; Generated on 2025-08-07 16:11 +0700
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
        ((number? x)
         (if (integer? x)
             (number->string (inexact->exact x))
             (number->string x)))
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
      define (
        bubble_sort nums
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                arr (
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
                                    _len nums
                                  )
                                )
                                 (
                                  begin (
                                    set! arr (
                                      append arr (
                                        _list (
                                          list-ref-safe nums i
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
                                 '(
                                  
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
                    let (
                      (
                        n (
                          _len arr
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            a 0
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
                                          < a n
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                b 0
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
                                                              < b (
                                                                - (
                                                                  - n a
                                                                )
                                                                 1
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  > (
                                                                    list-ref-safe arr b
                                                                  )
                                                                   (
                                                                    list-ref-safe arr (
                                                                      + b 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        tmp (
                                                                          list-ref-safe arr b
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        list-set! arr b (
                                                                          list-ref-safe arr (
                                                                            + b 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        list-set! arr (
                                                                          + b 1
                                                                        )
                                                                         tmp
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 '(
                                                                  
                                                                )
                                                              )
                                                               (
                                                                set! b (
                                                                  + b 1
                                                                )
                                                              )
                                                               (
                                                                loop6
                                                              )
                                                            )
                                                             '(
                                                              
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
                                                set! a (
                                                  + a 1
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop4
                                          )
                                        )
                                         '(
                                          
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
                            ret1 arr
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
        sort3 xs
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                arr (
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
                                    _len xs
                                  )
                                )
                                 (
                                  begin (
                                    set! arr (
                                      append arr (
                                        _list (
                                          list-ref-safe xs i
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
                                    loop9
                                  )
                                )
                                 '(
                                  
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
                    let (
                      (
                        n (
                          _len arr
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            a 0
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
                                          < a n
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                b 0
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
                                                              < b (
                                                                - (
                                                                  - n a
                                                                )
                                                                 1
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  > (
                                                                    list-ref-safe arr b
                                                                  )
                                                                   (
                                                                    list-ref-safe arr (
                                                                      + b 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        tmp (
                                                                          list-ref-safe arr b
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        list-set! arr b (
                                                                          list-ref-safe arr (
                                                                            + b 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        list-set! arr (
                                                                          + b 1
                                                                        )
                                                                         tmp
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 '(
                                                                  
                                                                )
                                                              )
                                                               (
                                                                set! b (
                                                                  + b 1
                                                                )
                                                              )
                                                               (
                                                                loop13
                                                              )
                                                            )
                                                             '(
                                                              
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
                                                set! a (
                                                  + a 1
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop11
                                          )
                                        )
                                         '(
                                          
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
                            ret8 arr
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
        triplet_sum1 arr target
      )
       (
        call/cc (
          lambda (
            ret15
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
                                - (
                                  _len arr
                                )
                                 2
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    j (
                                      + i 1
                                    )
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
                                                  < j (
                                                    - (
                                                      _len arr
                                                    )
                                                     1
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        k (
                                                          + j 1
                                                        )
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
                                                                      < k (
                                                                        _len arr
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          equal? (
                                                                            + (
                                                                              + (
                                                                                list-ref-safe arr i
                                                                              )
                                                                               (
                                                                                list-ref-safe arr j
                                                                              )
                                                                            )
                                                                             (
                                                                              list-ref-safe arr k
                                                                            )
                                                                          )
                                                                           target
                                                                        )
                                                                         (
                                                                          begin (
                                                                            ret15 (
                                                                              sort3 (
                                                                                _list (
                                                                                  list-ref-safe arr i
                                                                                )
                                                                                 (
                                                                                  list-ref-safe arr j
                                                                                )
                                                                                 (
                                                                                  list-ref-safe arr k
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         '(
                                                                          
                                                                        )
                                                                      )
                                                                       (
                                                                        set! k (
                                                                          + k 1
                                                                        )
                                                                      )
                                                                       (
                                                                        loop20
                                                                      )
                                                                    )
                                                                     '(
                                                                      
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
                                                        set! j (
                                                          + j 1
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    loop18
                                                  )
                                                )
                                                 '(
                                                  
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
                                    set! i (
                                      + i 1
                                    )
                                  )
                                )
                              )
                               (
                                loop16
                              )
                            )
                             '(
                              
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
                ret15 (
                  _list 0 0 0
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        triplet_sum2 arr target
      )
       (
        call/cc (
          lambda (
            ret22
          )
           (
            let (
              (
                sorted (
                  bubble_sort arr
                )
              )
            )
             (
              begin (
                let (
                  (
                    n (
                      _len sorted
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
                                        - n 2
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            left (
                                              + i 1
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                right (
                                                  - n 1
                                                )
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
                                                              < left right
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    s (
                                                                      _add (
                                                                        _add (
                                                                          cond (
                                                                            (
                                                                              string? sorted
                                                                            )
                                                                             (
                                                                              _substring sorted i (
                                                                                + i 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? sorted
                                                                            )
                                                                             (
                                                                              hash-table-ref sorted i
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref-safe sorted i
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          cond (
                                                                            (
                                                                              string? sorted
                                                                            )
                                                                             (
                                                                              _substring sorted left (
                                                                                + left 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? sorted
                                                                            )
                                                                             (
                                                                              hash-table-ref sorted left
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref-safe sorted left
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        cond (
                                                                          (
                                                                            string? sorted
                                                                          )
                                                                           (
                                                                            _substring sorted right (
                                                                              + right 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? sorted
                                                                          )
                                                                           (
                                                                            hash-table-ref sorted right
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref-safe sorted right
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      equal? s target
                                                                    )
                                                                     (
                                                                      begin (
                                                                        ret22 (
                                                                          _list (
                                                                            cond (
                                                                              (
                                                                                string? sorted
                                                                              )
                                                                               (
                                                                                _substring sorted i (
                                                                                  + i 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? sorted
                                                                              )
                                                                               (
                                                                                hash-table-ref sorted i
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe sorted i
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            cond (
                                                                              (
                                                                                string? sorted
                                                                              )
                                                                               (
                                                                                _substring sorted left (
                                                                                  + left 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? sorted
                                                                              )
                                                                               (
                                                                                hash-table-ref sorted left
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe sorted left
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            cond (
                                                                              (
                                                                                string? sorted
                                                                              )
                                                                               (
                                                                                _substring sorted right (
                                                                                  + right 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? sorted
                                                                              )
                                                                               (
                                                                                hash-table-ref sorted right
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe sorted right
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     '(
                                                                      
                                                                    )
                                                                  )
                                                                   (
                                                                    if (
                                                                      _lt s target
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! left (
                                                                          + left 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! right (
                                                                          - right 1
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
                                                             '(
                                                              
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
                                                set! i (
                                                  + i 1
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
                                     '(
                                      
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
                        ret22 (
                          _list 0 0 0
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
        list_equal a b
      )
       (
        call/cc (
          lambda (
            ret27
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len a
                  )
                   (
                    _len b
                  )
                )
              )
               (
                begin (
                  ret27 #f
                )
              )
               '(
                
              )
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
                                  _len a
                                )
                              )
                               (
                                begin (
                                  if (
                                    not (
                                      equal? (
                                        list-ref-safe a i
                                      )
                                       (
                                        list-ref-safe b i
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      ret27 #f
                                    )
                                  )
                                   '(
                                    
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
                               '(
                                
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
                  ret27 #t
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        test_triplet_sum
      )
       (
        call/cc (
          lambda (
            ret30
          )
           (
            let (
              (
                arr1 (
                  _list 13 29 7 23 5
                )
              )
            )
             (
              begin (
                if (
                  not (
                    list_equal (
                      triplet_sum1 arr1 35
                    )
                     (
                      _list 5 7 23
                    )
                  )
                )
                 (
                  begin (
                    panic "ts1 case1 failed"
                  )
                )
                 '(
                  
                )
              )
               (
                if (
                  not (
                    list_equal (
                      triplet_sum2 arr1 35
                    )
                     (
                      _list 5 7 23
                    )
                  )
                )
                 (
                  begin (
                    panic "ts2 case1 failed"
                  )
                )
                 '(
                  
                )
              )
               (
                let (
                  (
                    arr2 (
                      _list 37 9 19 50 44
                    )
                  )
                )
                 (
                  begin (
                    if (
                      not (
                        list_equal (
                          triplet_sum1 arr2 65
                        )
                         (
                          _list 9 19 37
                        )
                      )
                    )
                     (
                      begin (
                        panic "ts1 case2 failed"
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    if (
                      not (
                        list_equal (
                          triplet_sum2 arr2 65
                        )
                         (
                          _list 9 19 37
                        )
                      )
                    )
                     (
                      begin (
                        panic "ts2 case2 failed"
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    let (
                      (
                        arr3 (
                          _list 6 47 27 1 15
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          not (
                            list_equal (
                              triplet_sum1 arr3 11
                            )
                             (
                              _list 0 0 0
                            )
                          )
                        )
                         (
                          begin (
                            panic "ts1 case3 failed"
                          )
                        )
                         '(
                          
                        )
                      )
                       (
                        if (
                          not (
                            list_equal (
                              triplet_sum2 arr3 11
                            )
                             (
                              _list 0 0 0
                            )
                          )
                        )
                         (
                          begin (
                            panic "ts2 case3 failed"
                          )
                        )
                         '(
                          
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
            begin (
              test_triplet_sum
            )
             (
              let (
                (
                  sample (
                    _list 13 29 7 23 5
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      res (
                        triplet_sum2 sample 35
                      )
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            string-append (
                              string-append (
                                string-append (
                                  string-append (
                                    to-str-space (
                                      cond (
                                        (
                                          string? res
                                        )
                                         (
                                          _substring res 0 (
                                            + 0 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? res
                                        )
                                         (
                                          hash-table-ref res 0
                                        )
                                      )
                                       (
                                        else (
                                          list-ref-safe res 0
                                        )
                                      )
                                    )
                                  )
                                   " "
                                )
                                 (
                                  to-str-space (
                                    cond (
                                      (
                                        string? res
                                      )
                                       (
                                        _substring res 1 (
                                          + 1 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? res
                                      )
                                       (
                                        hash-table-ref res 1
                                      )
                                    )
                                     (
                                      else (
                                        list-ref-safe res 1
                                      )
                                    )
                                  )
                                )
                              )
                               " "
                            )
                             (
                              to-str-space (
                                cond (
                                  (
                                    string? res
                                  )
                                   (
                                    _substring res 2 (
                                      + 2 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? res
                                  )
                                   (
                                    hash-table-ref res 2
                                  )
                                )
                                 (
                                  else (
                                    list-ref-safe res 2
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          string-append (
                            string-append (
                              string-append (
                                string-append (
                                  to-str-space (
                                    cond (
                                      (
                                        string? res
                                      )
                                       (
                                        _substring res 0 (
                                          + 0 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? res
                                      )
                                       (
                                        hash-table-ref res 0
                                      )
                                    )
                                     (
                                      else (
                                        list-ref-safe res 0
                                      )
                                    )
                                  )
                                )
                                 " "
                              )
                               (
                                to-str-space (
                                  cond (
                                    (
                                      string? res
                                    )
                                     (
                                      _substring res 1 (
                                        + 1 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? res
                                    )
                                     (
                                      hash-table-ref res 1
                                    )
                                  )
                                   (
                                    else (
                                      list-ref-safe res 1
                                    )
                                  )
                                )
                              )
                            )
                             " "
                          )
                           (
                            to-str-space (
                              cond (
                                (
                                  string? res
                                )
                                 (
                                  _substring res 2 (
                                    + 2 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? res
                                )
                                 (
                                  hash-table-ref res 2
                                )
                              )
                               (
                                else (
                                  list-ref-safe res 2
                                )
                              )
                            )
                          )
                        )
                         (
                          to-str (
                            string-append (
                              string-append (
                                string-append (
                                  string-append (
                                    to-str-space (
                                      cond (
                                        (
                                          string? res
                                        )
                                         (
                                          _substring res 0 (
                                            + 0 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? res
                                        )
                                         (
                                          hash-table-ref res 0
                                        )
                                      )
                                       (
                                        else (
                                          list-ref-safe res 0
                                        )
                                      )
                                    )
                                  )
                                   " "
                                )
                                 (
                                  to-str-space (
                                    cond (
                                      (
                                        string? res
                                      )
                                       (
                                        _substring res 1 (
                                          + 1 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? res
                                      )
                                       (
                                        hash-table-ref res 1
                                      )
                                    )
                                     (
                                      else (
                                        list-ref-safe res 1
                                      )
                                    )
                                  )
                                )
                              )
                               " "
                            )
                             (
                              to-str-space (
                                cond (
                                  (
                                    string? res
                                  )
                                   (
                                    _substring res 2 (
                                      + 2 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? res
                                  )
                                   (
                                    hash-table-ref res 2
                                  )
                                )
                                 (
                                  else (
                                    list-ref-safe res 2
                                  )
                                )
                              )
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
     (
      main
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
