;; Generated on 2025-08-07 11:54 +0700
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
(define (_div a b) (/ a b))
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
(
  let (
    (
      start53 (
        current-jiffy
      )
    )
     (
      jps56 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        add matrices
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                rows (
                  _len (
                    list-ref matrices 0
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    cols (
                      _len (
                        cond (
                          (
                            string? (
                              list-ref matrices 0
                            )
                          )
                           (
                            _substring (
                              list-ref matrices 0
                            )
                             0 (
                              + 0 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? (
                              list-ref matrices 0
                            )
                          )
                           (
                            hash-table-ref (
                              list-ref matrices 0
                            )
                             0
                          )
                        )
                         (
                          else (
                            list-ref (
                              list-ref matrices 0
                            )
                             0
                          )
                        )
                      )
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
                        let (
                          (
                            result (
                              _list
                            )
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
                                          < r rows
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
                                                    c 0
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
                                                                  < c cols
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        sum 0.0
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
                                                                                          < m (
                                                                                            _len matrices
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! sum (
                                                                                              + sum (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                           r (
                                                                                                            + r 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                           r
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                           r
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    _substring (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                           r (
                                                                                                            + r 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                           r
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                           r
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     c (
                                                                                                      + c 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  (
                                                                                                    hash-table? (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                           r (
                                                                                                            + r 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                           r
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                           r
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    hash-table-ref (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                           r (
                                                                                                            + r 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                           r
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                           r
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     c
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    list-ref (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                           r (
                                                                                                            + r 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                           r
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref matrices m
                                                                                                          )
                                                                                                           r
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     c
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! m (
                                                                                              + m 1
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
                                                                            set! row (
                                                                              append row (
                                                                                _list sum
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! c (
                                                                              + c 1
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
                                                    set! result (
                                                      append result (
                                                        _list row
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! r (
                                                      + r 1
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
                            ret1 result
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
        subtract a b
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                rows (
                  _len a
                )
              )
            )
             (
              begin (
                let (
                  (
                    cols (
                      _len (
                        list-ref a 0
                      )
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
                        let (
                          (
                            result (
                              _list
                            )
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
                                          < r rows
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
                                                    c 0
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
                                                                  < c cols
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! row (
                                                                      append row (
                                                                        _list (
                                                                          - (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref a r
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref a r
                                                                                )
                                                                                 c (
                                                                                  + c 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref a r
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref a r
                                                                                )
                                                                                 c
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref a r
                                                                                )
                                                                                 c
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref b r
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref b r
                                                                                )
                                                                                 c (
                                                                                  + c 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref b r
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref b r
                                                                                )
                                                                                 c
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref b r
                                                                                )
                                                                                 c
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! c (
                                                                      + c 1
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
                                                    set! result (
                                                      append result (
                                                        _list row
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! r (
                                                      + r 1
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
                            ret8 result
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
        scalar_multiply matrix n
      )
       (
        call/cc (
          lambda (
            ret13
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
                                    _len matrix
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
                                                          < j (
                                                            _len (
                                                              list-ref matrix i
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! row (
                                                              append row (
                                                                _list (
                                                                  * (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref matrix i
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref matrix i
                                                                        )
                                                                         j (
                                                                          + j 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref matrix i
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref matrix i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          list-ref matrix i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                  )
                                                                   n
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
                                            set! result (
                                              append result (
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
                    ret13 result
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
        multiply a b
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            let (
              (
                rowsA (
                  _len a
                )
              )
            )
             (
              begin (
                let (
                  (
                    colsA (
                      _len (
                        list-ref a 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        rowsB (
                          _len b
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            colsB (
                              _len (
                                list-ref b 0
                              )
                            )
                          )
                        )
                         (
                          begin (
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
                                                  < i rowsA
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
                                                                          < j colsB
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                sum 0.0
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
                                                                                                  < k colsA
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    set! sum (
                                                                                                      _add sum (
                                                                                                        * (
                                                                                                          cond (
                                                                                                            (
                                                                                                              string? (
                                                                                                                list-ref a i
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              _substring (
                                                                                                                list-ref a i
                                                                                                              )
                                                                                                               k (
                                                                                                                + k 1
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            (
                                                                                                              hash-table? (
                                                                                                                list-ref a i
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              hash-table-ref (
                                                                                                                list-ref a i
                                                                                                              )
                                                                                                               k
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            else (
                                                                                                              list-ref (
                                                                                                                list-ref a i
                                                                                                              )
                                                                                                               k
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          cond (
                                                                                                            (
                                                                                                              string? (
                                                                                                                list-ref b k
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              _substring (
                                                                                                                list-ref b k
                                                                                                              )
                                                                                                               j (
                                                                                                                + j 1
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            (
                                                                                                              hash-table? (
                                                                                                                list-ref b k
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              hash-table-ref (
                                                                                                                list-ref b k
                                                                                                              )
                                                                                                               j
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            else (
                                                                                                              list-ref (
                                                                                                                list-ref b k
                                                                                                              )
                                                                                                               j
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
                                                                                    set! row (
                                                                                      append row (
                                                                                        _list sum
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
                                                            set! result (
                                                              append result (
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
     (
      define (
        identity n
      )
       (
        call/cc (
          lambda (
            ret25
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
                                                          < j n
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              equal? i j
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list 1.0
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list 0.0
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
                                            set! result (
                                              append result (
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
                    ret25 result
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
        transpose matrix
      )
       (
        call/cc (
          lambda (
            ret30
          )
           (
            let (
              (
                rows (
                  _len matrix
                )
              )
            )
             (
              begin (
                let (
                  (
                    cols (
                      _len (
                        list-ref matrix 0
                      )
                    )
                  )
                )
                 (
                  begin (
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
                            c 0
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
                                          < c cols
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
                                                                  < r rows
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! row (
                                                                      append row (
                                                                        _list (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref matrix r
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref matrix r
                                                                              )
                                                                               c (
                                                                                + c 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref matrix r
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref matrix r
                                                                              )
                                                                               c
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref matrix r
                                                                              )
                                                                               c
                                                                            )
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
                                                    set! result (
                                                      append result (
                                                        _list row
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! c (
                                                      + c 1
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
                            ret30 result
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
        minor matrix row column
      )
       (
        call/cc (
          lambda (
            ret35
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
                        break37
                      )
                       (
                        letrec (
                          (
                            loop36 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len matrix
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
                                            new_row (
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
                                                    break39
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop38 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              < j (
                                                                _len (
                                                                  list-ref matrix i
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  not (
                                                                    equal? j column
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! new_row (
                                                                      append new_row (
                                                                        _list (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref matrix i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref matrix i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref matrix i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref matrix i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref matrix i
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
                                                                loop38
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
                                                      loop38
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! result (
                                                  append result (
                                                    _list new_row
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
                                    loop36
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
                          loop36
                        )
                      )
                    )
                  )
                   (
                    ret35 result
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
        determinant matrix
      )
       (
        call/cc (
          lambda (
            ret40
          )
           (
            begin (
              if (
                equal? (
                  _len matrix
                )
                 1
              )
               (
                begin (
                  ret40 (
                    cond (
                      (
                        string? (
                          list-ref matrix 0
                        )
                      )
                       (
                        _substring (
                          list-ref matrix 0
                        )
                         0 (
                          + 0 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? (
                          list-ref matrix 0
                        )
                      )
                       (
                        hash-table-ref (
                          list-ref matrix 0
                        )
                         0
                      )
                    )
                     (
                      else (
                        list-ref (
                          list-ref matrix 0
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
              let (
                (
                  det 0.0
                )
              )
               (
                begin (
                  let (
                    (
                      c 0
                    )
                  )
                   (
                    begin (
                      call/cc (
                        lambda (
                          break42
                        )
                         (
                          letrec (
                            (
                              loop41 (
                                lambda (
                                  
                                )
                                 (
                                  if (
                                    < c (
                                      _len (
                                        list-ref matrix 0
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          sub (
                                            minor matrix 0 c
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              sign (
                                                if (
                                                  equal? (
                                                    _mod c 2
                                                  )
                                                   0
                                                )
                                                 1.0 (
                                                  - 1.0
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! det (
                                                _add det (
                                                  * (
                                                    * (
                                                      cond (
                                                        (
                                                          string? (
                                                            list-ref matrix 0
                                                          )
                                                        )
                                                         (
                                                          _substring (
                                                            list-ref matrix 0
                                                          )
                                                           c (
                                                            + c 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? (
                                                            list-ref matrix 0
                                                          )
                                                        )
                                                         (
                                                          hash-table-ref (
                                                            list-ref matrix 0
                                                          )
                                                           c
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref (
                                                            list-ref matrix 0
                                                          )
                                                           c
                                                        )
                                                      )
                                                    )
                                                     (
                                                      determinant sub
                                                    )
                                                  )
                                                   sign
                                                )
                                              )
                                            )
                                             (
                                              set! c (
                                                + c 1
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      loop41
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
                            loop41
                          )
                        )
                      )
                    )
                     (
                      ret40 det
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
        inverse matrix
      )
       (
        call/cc (
          lambda (
            ret43
          )
           (
            let (
              (
                det (
                  determinant matrix
                )
              )
            )
             (
              begin (
                if (
                  equal? det 0.0
                )
                 (
                  begin (
                    ret43 (
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
                let (
                  (
                    size (
                      _len matrix
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        matrix_minor (
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
                                break45
                              )
                               (
                                letrec (
                                  (
                                    loop44 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i size
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
                                                        break47
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop46 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < j size
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        m (
                                                                          minor matrix i j
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! row (
                                                                          append row (
                                                                            _list (
                                                                              determinant m
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
                                                                    loop46
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
                                                          loop46
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! matrix_minor (
                                                      append matrix_minor (
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
                                            loop44
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
                                  loop44
                                )
                              )
                            )
                          )
                           (
                            let (
                              (
                                cofactors (
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
                                    break49
                                  )
                                   (
                                    letrec (
                                      (
                                        loop48 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i size
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
                                                                      < j size
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            sign (
                                                                              if (
                                                                                equal? (
                                                                                  _mod (
                                                                                    + i j
                                                                                  )
                                                                                   2
                                                                                )
                                                                                 0
                                                                              )
                                                                               1.0 (
                                                                                - 1.0
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! row (
                                                                              append row (
                                                                                _list (
                                                                                  * (
                                                                                    cond (
                                                                                      (
                                                                                        string? (
                                                                                          list-ref matrix_minor i
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        _substring (
                                                                                          list-ref matrix_minor i
                                                                                        )
                                                                                         j (
                                                                                          + j 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? (
                                                                                          list-ref matrix_minor i
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref (
                                                                                          list-ref matrix_minor i
                                                                                        )
                                                                                         j
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        list-ref (
                                                                                          list-ref matrix_minor i
                                                                                        )
                                                                                         j
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   sign
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
                                                        set! cofactors (
                                                          append cofactors (
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
                                                loop48
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
                                      loop48
                                    )
                                  )
                                )
                              )
                               (
                                let (
                                  (
                                    adjugate (
                                      transpose cofactors
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    ret43 (
                                      scalar_multiply adjugate (
                                        _div 1.0 det
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
            ret52
          )
           (
            let (
              (
                matrix_a (
                  _list (
                    _list 12.0 10.0
                  )
                   (
                    _list 3.0 9.0
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    matrix_b (
                      _list (
                        _list 3.0 4.0
                      )
                       (
                        _list 7.0 4.0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        matrix_c (
                          _list (
                            _list 11.0 12.0 13.0 14.0
                          )
                           (
                            _list 21.0 22.0 23.0 24.0
                          )
                           (
                            _list 31.0 32.0 33.0 34.0
                          )
                           (
                            _list 41.0 42.0 43.0 44.0
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            matrix_d (
                              _list (
                                _list 3.0 0.0 2.0
                              )
                               (
                                _list 2.0 0.0 (
                                  - 2.0
                                )
                              )
                               (
                                _list 0.0 1.0 1.0
                              )
                            )
                          )
                        )
                         (
                          begin (
                            _display (
                              if (
                                string? (
                                  string-append (
                                    string-append "Add Operation, add(matrix_a, matrix_b) = " (
                                      to-str-space (
                                        add (
                                          _list matrix_a matrix_b
                                        )
                                      )
                                    )
                                  )
                                   " \n"
                                )
                              )
                               (
                                string-append (
                                  string-append "Add Operation, add(matrix_a, matrix_b) = " (
                                    to-str-space (
                                      add (
                                        _list matrix_a matrix_b
                                      )
                                    )
                                  )
                                )
                                 " \n"
                              )
                               (
                                to-str (
                                  string-append (
                                    string-append "Add Operation, add(matrix_a, matrix_b) = " (
                                      to-str-space (
                                        add (
                                          _list matrix_a matrix_b
                                        )
                                      )
                                    )
                                  )
                                   " \n"
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
                                  string-append (
                                    string-append "Multiply Operation, multiply(matrix_a, matrix_b) = " (
                                      to-str-space (
                                        multiply matrix_a matrix_b
                                      )
                                    )
                                  )
                                   " \n"
                                )
                              )
                               (
                                string-append (
                                  string-append "Multiply Operation, multiply(matrix_a, matrix_b) = " (
                                    to-str-space (
                                      multiply matrix_a matrix_b
                                    )
                                  )
                                )
                                 " \n"
                              )
                               (
                                to-str (
                                  string-append (
                                    string-append "Multiply Operation, multiply(matrix_a, matrix_b) = " (
                                      to-str-space (
                                        multiply matrix_a matrix_b
                                      )
                                    )
                                  )
                                   " \n"
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
                                  string-append (
                                    string-append "Identity: " (
                                      to-str-space (
                                        identity 5
                                      )
                                    )
                                  )
                                   "\n"
                                )
                              )
                               (
                                string-append (
                                  string-append "Identity: " (
                                    to-str-space (
                                      identity 5
                                    )
                                  )
                                )
                                 "\n"
                              )
                               (
                                to-str (
                                  string-append (
                                    string-append "Identity: " (
                                      to-str-space (
                                        identity 5
                                      )
                                    )
                                  )
                                   "\n"
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
                                  string-append (
                                    string-append (
                                      string-append (
                                        string-append "Minor of " (
                                          to-str-space matrix_c
                                        )
                                      )
                                       " = "
                                    )
                                     (
                                      to-str-space (
                                        minor matrix_c 1 2
                                      )
                                    )
                                  )
                                   " \n"
                                )
                              )
                               (
                                string-append (
                                  string-append (
                                    string-append (
                                      string-append "Minor of " (
                                        to-str-space matrix_c
                                      )
                                    )
                                     " = "
                                  )
                                   (
                                    to-str-space (
                                      minor matrix_c 1 2
                                    )
                                  )
                                )
                                 " \n"
                              )
                               (
                                to-str (
                                  string-append (
                                    string-append (
                                      string-append (
                                        string-append "Minor of " (
                                          to-str-space matrix_c
                                        )
                                      )
                                       " = "
                                    )
                                     (
                                      to-str-space (
                                        minor matrix_c 1 2
                                      )
                                    )
                                  )
                                   " \n"
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
                                  string-append (
                                    string-append (
                                      string-append (
                                        string-append "Determinant of " (
                                          to-str-space matrix_b
                                        )
                                      )
                                       " = "
                                    )
                                     (
                                      to-str-space (
                                        determinant matrix_b
                                      )
                                    )
                                  )
                                   " \n"
                                )
                              )
                               (
                                string-append (
                                  string-append (
                                    string-append (
                                      string-append "Determinant of " (
                                        to-str-space matrix_b
                                      )
                                    )
                                     " = "
                                  )
                                   (
                                    to-str-space (
                                      determinant matrix_b
                                    )
                                  )
                                )
                                 " \n"
                              )
                               (
                                to-str (
                                  string-append (
                                    string-append (
                                      string-append (
                                        string-append "Determinant of " (
                                          to-str-space matrix_b
                                        )
                                      )
                                       " = "
                                    )
                                     (
                                      to-str-space (
                                        determinant matrix_b
                                      )
                                    )
                                  )
                                   " \n"
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
                                  string-append (
                                    string-append (
                                      string-append (
                                        string-append "Inverse of " (
                                          to-str-space matrix_d
                                        )
                                      )
                                       " = "
                                    )
                                     (
                                      to-str-space (
                                        inverse matrix_d
                                      )
                                    )
                                  )
                                   "\n"
                                )
                              )
                               (
                                string-append (
                                  string-append (
                                    string-append (
                                      string-append "Inverse of " (
                                        to-str-space matrix_d
                                      )
                                    )
                                     " = "
                                  )
                                   (
                                    to-str-space (
                                      inverse matrix_d
                                    )
                                  )
                                )
                                 "\n"
                              )
                               (
                                to-str (
                                  string-append (
                                    string-append (
                                      string-append (
                                        string-append "Inverse of " (
                                          to-str-space matrix_d
                                        )
                                      )
                                       " = "
                                    )
                                     (
                                      to-str-space (
                                        inverse matrix_d
                                      )
                                    )
                                  )
                                   "\n"
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
     (
      main
    )
     (
      let (
        (
          end54 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur55 (
              quotient (
                * (
                  - end54 start53
                )
                 1000000
              )
               jps56
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur55
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
