;; Generated on 2025-08-07 08:20 +0700
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
      start42 (
        current-jiffy
      )
    )
     (
      jps45 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        default_matrix_multiplication a b
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              _list (
                _list (
                  + (
                    * (
                      cond (
                        (
                          string? (
                            list-ref a 0
                          )
                        )
                         (
                          _substring (
                            list-ref a 0
                          )
                           0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref a 0
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref a 0
                          )
                           0
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref a 0
                          )
                           0
                        )
                      )
                    )
                     (
                      cond (
                        (
                          string? (
                            list-ref b 0
                          )
                        )
                         (
                          _substring (
                            list-ref b 0
                          )
                           0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref b 0
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref b 0
                          )
                           0
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref b 0
                          )
                           0
                        )
                      )
                    )
                  )
                   (
                    * (
                      cond (
                        (
                          string? (
                            list-ref a 0
                          )
                        )
                         (
                          _substring (
                            list-ref a 0
                          )
                           1 (
                            + 1 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref a 0
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref a 0
                          )
                           1
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref a 0
                          )
                           1
                        )
                      )
                    )
                     (
                      cond (
                        (
                          string? (
                            list-ref b 1
                          )
                        )
                         (
                          _substring (
                            list-ref b 1
                          )
                           0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref b 1
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref b 1
                          )
                           0
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref b 1
                          )
                           0
                        )
                      )
                    )
                  )
                )
                 (
                  + (
                    * (
                      cond (
                        (
                          string? (
                            list-ref a 0
                          )
                        )
                         (
                          _substring (
                            list-ref a 0
                          )
                           0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref a 0
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref a 0
                          )
                           0
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref a 0
                          )
                           0
                        )
                      )
                    )
                     (
                      cond (
                        (
                          string? (
                            list-ref b 0
                          )
                        )
                         (
                          _substring (
                            list-ref b 0
                          )
                           1 (
                            + 1 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref b 0
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref b 0
                          )
                           1
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref b 0
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
                            list-ref a 0
                          )
                        )
                         (
                          _substring (
                            list-ref a 0
                          )
                           1 (
                            + 1 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref a 0
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref a 0
                          )
                           1
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref a 0
                          )
                           1
                        )
                      )
                    )
                     (
                      cond (
                        (
                          string? (
                            list-ref b 1
                          )
                        )
                         (
                          _substring (
                            list-ref b 1
                          )
                           1 (
                            + 1 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref b 1
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref b 1
                          )
                           1
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref b 1
                          )
                           1
                        )
                      )
                    )
                  )
                )
              )
               (
                _list (
                  + (
                    * (
                      cond (
                        (
                          string? (
                            list-ref a 1
                          )
                        )
                         (
                          _substring (
                            list-ref a 1
                          )
                           0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref a 1
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref a 1
                          )
                           0
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref a 1
                          )
                           0
                        )
                      )
                    )
                     (
                      cond (
                        (
                          string? (
                            list-ref b 0
                          )
                        )
                         (
                          _substring (
                            list-ref b 0
                          )
                           0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref b 0
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref b 0
                          )
                           0
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref b 0
                          )
                           0
                        )
                      )
                    )
                  )
                   (
                    * (
                      cond (
                        (
                          string? (
                            list-ref a 1
                          )
                        )
                         (
                          _substring (
                            list-ref a 1
                          )
                           1 (
                            + 1 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref a 1
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref a 1
                          )
                           1
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref a 1
                          )
                           1
                        )
                      )
                    )
                     (
                      cond (
                        (
                          string? (
                            list-ref b 1
                          )
                        )
                         (
                          _substring (
                            list-ref b 1
                          )
                           0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref b 1
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref b 1
                          )
                           0
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref b 1
                          )
                           0
                        )
                      )
                    )
                  )
                )
                 (
                  + (
                    * (
                      cond (
                        (
                          string? (
                            list-ref a 1
                          )
                        )
                         (
                          _substring (
                            list-ref a 1
                          )
                           0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref a 1
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref a 1
                          )
                           0
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref a 1
                          )
                           0
                        )
                      )
                    )
                     (
                      cond (
                        (
                          string? (
                            list-ref b 0
                          )
                        )
                         (
                          _substring (
                            list-ref b 0
                          )
                           1 (
                            + 1 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref b 0
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref b 0
                          )
                           1
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref b 0
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
                            list-ref a 1
                          )
                        )
                         (
                          _substring (
                            list-ref a 1
                          )
                           1 (
                            + 1 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref a 1
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref a 1
                          )
                           1
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref a 1
                          )
                           1
                        )
                      )
                    )
                     (
                      cond (
                        (
                          string? (
                            list-ref b 1
                          )
                        )
                         (
                          _substring (
                            list-ref b 1
                          )
                           1 (
                            + 1 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref b 1
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref b 1
                          )
                           1
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref b 1
                          )
                           1
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
        matrix_addition matrix_a matrix_b
      )
       (
        call/cc (
          lambda (
            ret2
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
                                  < i (
                                    _len matrix_a
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
                                                          < j (
                                                            _len (
                                                              list-ref matrix_a i
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! row (
                                                              append row (
                                                                _list (
                                                                  + (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref matrix_a i
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref matrix_a i
                                                                        )
                                                                         j (
                                                                          + j 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref matrix_a i
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref matrix_a i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          list-ref matrix_a i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref matrix_b i
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref matrix_b i
                                                                        )
                                                                         j (
                                                                          + j 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref matrix_b i
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref matrix_b i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          list-ref matrix_b i
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
                    ret2 result
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
        matrix_subtraction matrix_a matrix_b
      )
       (
        call/cc (
          lambda (
            ret7
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
                                    _len matrix_a
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
                                                          < j (
                                                            _len (
                                                              list-ref matrix_a i
                                                            )
                                                          )
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
                                                                          list-ref matrix_a i
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref matrix_a i
                                                                        )
                                                                         j (
                                                                          + j 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref matrix_a i
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref matrix_a i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          list-ref matrix_a i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref matrix_b i
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref matrix_b i
                                                                        )
                                                                         j (
                                                                          + j 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref matrix_b i
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref matrix_b i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          list-ref matrix_b i
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
                    ret7 result
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
        split_matrix a
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                n (
                  _len a
                )
              )
            )
             (
              begin (
                let (
                  (
                    mid (
                      _div n 2
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        top_left (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            top_right (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                bot_left (
                                  _list
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    bot_right (
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
                                                      < i mid
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            left_row (
                                                              _list
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                right_row (
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
                                                                                  < j mid
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! left_row (
                                                                                      append left_row (
                                                                                        _list (
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
                                                                                               j (
                                                                                                + j 1
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
                                                                                               j
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref a i
                                                                                              )
                                                                                               j
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! right_row (
                                                                                      append right_row (
                                                                                        _list (
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
                                                                                               (
                                                                                                + j mid
                                                                                              )
                                                                                               (
                                                                                                + (
                                                                                                  + j mid
                                                                                                )
                                                                                                 1
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
                                                                                               (
                                                                                                + j mid
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref a i
                                                                                              )
                                                                                               (
                                                                                                + j mid
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
                                                                    set! top_left (
                                                                      append top_left (
                                                                        _list left_row
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! top_right (
                                                                      append top_right (
                                                                        _list right_row
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
                                        set! i mid
                                      )
                                       (
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
                                                      < i n
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            left_row (
                                                              _list
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                right_row (
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
                                                                                  < j mid
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! left_row (
                                                                                      append left_row (
                                                                                        _list (
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
                                                                                               j (
                                                                                                + j 1
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
                                                                                               j
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref a i
                                                                                              )
                                                                                               j
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! right_row (
                                                                                      append right_row (
                                                                                        _list (
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
                                                                                               (
                                                                                                + j mid
                                                                                              )
                                                                                               (
                                                                                                + (
                                                                                                  + j mid
                                                                                                )
                                                                                                 1
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
                                                                                               (
                                                                                                + j mid
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref a i
                                                                                              )
                                                                                               (
                                                                                                + j mid
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
                                                                    set! bot_left (
                                                                      append bot_left (
                                                                        _list left_row
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! bot_right (
                                                                      append bot_right (
                                                                        _list right_row
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
                                        ret12 (
                                          _list top_left top_right bot_left bot_right
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
        matrix_dimensions matrix
      )
       (
        call/cc (
          lambda (
            ret21
          )
           (
            ret21 (
              _list (
                _len matrix
              )
               (
                _len (
                  list-ref matrix 0
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        next_power_of_two n
      )
       (
        call/cc (
          lambda (
            ret22
          )
           (
            let (
              (
                p 1
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
                              < p n
                            )
                             (
                              begin (
                                set! p (
                                  * p 2
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
                ret22 p
              )
            )
          )
        )
      )
    )
     (
      define (
        pad_matrix mat rows cols
      )
       (
        call/cc (
          lambda (
            ret25
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
                                  < i rows
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
                                                          < j cols
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                v 0
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  and (
                                                                    < i (
                                                                      _len mat
                                                                    )
                                                                  )
                                                                   (
                                                                    < j (
                                                                      _len (
                                                                        list-ref mat 0
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! v (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            list-ref mat i
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref mat i
                                                                          )
                                                                           j (
                                                                            + j 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref mat i
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref mat i
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            list-ref mat i
                                                                          )
                                                                           j
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
                                                                set! row (
                                                                  append row (
                                                                    _list v
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
                    ret25 res
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
        actual_strassen matrix_a matrix_b
      )
       (
        call/cc (
          lambda (
            ret30
          )
           (
            begin (
              if (
                equal? (
                  cond (
                    (
                      string? (
                        matrix_dimensions matrix_a
                      )
                    )
                     (
                      _substring (
                        matrix_dimensions matrix_a
                      )
                       0 (
                        + 0 1
                      )
                    )
                  )
                   (
                    (
                      hash-table? (
                        matrix_dimensions matrix_a
                      )
                    )
                     (
                      hash-table-ref (
                        matrix_dimensions matrix_a
                      )
                       0
                    )
                  )
                   (
                    else (
                      list-ref (
                        matrix_dimensions matrix_a
                      )
                       0
                    )
                  )
                )
                 2
              )
               (
                begin (
                  ret30 (
                    default_matrix_multiplication matrix_a matrix_b
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
                  parts_a (
                    split_matrix matrix_a
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      a (
                        cond (
                          (
                            string? parts_a
                          )
                           (
                            _substring parts_a 0 (
                              + 0 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? parts_a
                          )
                           (
                            hash-table-ref parts_a 0
                          )
                        )
                         (
                          else (
                            list-ref parts_a 0
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          b (
                            cond (
                              (
                                string? parts_a
                              )
                               (
                                _substring parts_a 1 (
                                  + 1 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? parts_a
                              )
                               (
                                hash-table-ref parts_a 1
                              )
                            )
                             (
                              else (
                                list-ref parts_a 1
                              )
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              c (
                                cond (
                                  (
                                    string? parts_a
                                  )
                                   (
                                    _substring parts_a 2 (
                                      + 2 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? parts_a
                                  )
                                   (
                                    hash-table-ref parts_a 2
                                  )
                                )
                                 (
                                  else (
                                    list-ref parts_a 2
                                  )
                                )
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  d (
                                    cond (
                                      (
                                        string? parts_a
                                      )
                                       (
                                        _substring parts_a 3 (
                                          + 3 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? parts_a
                                      )
                                       (
                                        hash-table-ref parts_a 3
                                      )
                                    )
                                     (
                                      else (
                                        list-ref parts_a 3
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      parts_b (
                                        split_matrix matrix_b
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          e (
                                            cond (
                                              (
                                                string? parts_b
                                              )
                                               (
                                                _substring parts_b 0 (
                                                  + 0 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? parts_b
                                              )
                                               (
                                                hash-table-ref parts_b 0
                                              )
                                            )
                                             (
                                              else (
                                                list-ref parts_b 0
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              f (
                                                cond (
                                                  (
                                                    string? parts_b
                                                  )
                                                   (
                                                    _substring parts_b 1 (
                                                      + 1 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? parts_b
                                                  )
                                                   (
                                                    hash-table-ref parts_b 1
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref parts_b 1
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  g (
                                                    cond (
                                                      (
                                                        string? parts_b
                                                      )
                                                       (
                                                        _substring parts_b 2 (
                                                          + 2 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        hash-table? parts_b
                                                      )
                                                       (
                                                        hash-table-ref parts_b 2
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        list-ref parts_b 2
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      h (
                                                        cond (
                                                          (
                                                            string? parts_b
                                                          )
                                                           (
                                                            _substring parts_b 3 (
                                                              + 3 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          (
                                                            hash-table? parts_b
                                                          )
                                                           (
                                                            hash-table-ref parts_b 3
                                                          )
                                                        )
                                                         (
                                                          else (
                                                            list-ref parts_b 3
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          t1 (
                                                            actual_strassen a (
                                                              matrix_subtraction f h
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              t2 (
                                                                actual_strassen (
                                                                  matrix_addition a b
                                                                )
                                                                 h
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  t3 (
                                                                    actual_strassen (
                                                                      matrix_addition c d
                                                                    )
                                                                     e
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      t4 (
                                                                        actual_strassen d (
                                                                          matrix_subtraction g e
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      let (
                                                                        (
                                                                          t5 (
                                                                            actual_strassen (
                                                                              matrix_addition a d
                                                                            )
                                                                             (
                                                                              matrix_addition e h
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          let (
                                                                            (
                                                                              t6 (
                                                                                actual_strassen (
                                                                                  matrix_subtraction b d
                                                                                )
                                                                                 (
                                                                                  matrix_addition g h
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              let (
                                                                                (
                                                                                  t7 (
                                                                                    actual_strassen (
                                                                                      matrix_subtraction a c
                                                                                    )
                                                                                     (
                                                                                      matrix_addition e f
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  let (
                                                                                    (
                                                                                      top_left (
                                                                                        matrix_addition (
                                                                                          matrix_subtraction (
                                                                                            matrix_addition t5 t4
                                                                                          )
                                                                                           t2
                                                                                        )
                                                                                         t6
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      let (
                                                                                        (
                                                                                          top_right (
                                                                                            matrix_addition t1 t2
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        begin (
                                                                                          let (
                                                                                            (
                                                                                              bot_left (
                                                                                                matrix_addition t3 t4
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            begin (
                                                                                              let (
                                                                                                (
                                                                                                  bot_right (
                                                                                                    matrix_subtraction (
                                                                                                      matrix_subtraction (
                                                                                                        matrix_addition t1 t5
                                                                                                      )
                                                                                                       t3
                                                                                                    )
                                                                                                     t7
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                begin (
                                                                                                  let (
                                                                                                    (
                                                                                                      new_matrix (
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
                                                                                                                          _len top_right
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        begin (
                                                                                                                          set! new_matrix (
                                                                                                                            append new_matrix (
                                                                                                                              _list (
                                                                                                                                append (
                                                                                                                                  cond (
                                                                                                                                    (
                                                                                                                                      string? top_left
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      _substring top_left i (
                                                                                                                                        + i 1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    (
                                                                                                                                      hash-table? top_left
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      hash-table-ref top_left i
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    else (
                                                                                                                                      list-ref top_left i
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  cond (
                                                                                                                                    (
                                                                                                                                      string? top_right
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      _substring top_right i (
                                                                                                                                        + i 1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    (
                                                                                                                                      hash-table? top_right
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      hash-table-ref top_right i
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    else (
                                                                                                                                      list-ref top_right i
                                                                                                                                    )
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
                                                                                                                          _len bot_right
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        begin (
                                                                                                                          set! new_matrix (
                                                                                                                            append new_matrix (
                                                                                                                              _list (
                                                                                                                                append (
                                                                                                                                  cond (
                                                                                                                                    (
                                                                                                                                      string? bot_left
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      _substring bot_left i (
                                                                                                                                        + i 1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    (
                                                                                                                                      hash-table? bot_left
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      hash-table-ref bot_left i
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    else (
                                                                                                                                      list-ref bot_left i
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  cond (
                                                                                                                                    (
                                                                                                                                      string? bot_right
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      _substring bot_right i (
                                                                                                                                        + i 1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    (
                                                                                                                                      hash-table? bot_right
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      hash-table-ref bot_right i
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    else (
                                                                                                                                      list-ref bot_right i
                                                                                                                                    )
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
                                                                                                          ret30 new_matrix
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
        strassen matrix1 matrix2
      )
       (
        call/cc (
          lambda (
            ret35
          )
           (
            let (
              (
                dims1 (
                  matrix_dimensions matrix1
                )
              )
            )
             (
              begin (
                let (
                  (
                    dims2 (
                      matrix_dimensions matrix2
                    )
                  )
                )
                 (
                  begin (
                    if (
                      not (
                        equal? (
                          cond (
                            (
                              string? dims1
                            )
                             (
                              _substring dims1 1 (
                                + 1 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? dims1
                            )
                             (
                              hash-table-ref dims1 1
                            )
                          )
                           (
                            else (
                              list-ref dims1 1
                            )
                          )
                        )
                         (
                          cond (
                            (
                              string? dims2
                            )
                             (
                              _substring dims2 0 (
                                + 0 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? dims2
                            )
                             (
                              hash-table-ref dims2 0
                            )
                          )
                           (
                            else (
                              list-ref dims2 0
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        ret35 (
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
                        maximum (
                          let (
                            (
                              v36 (
                                apply max (
                                  _list (
                                    cond (
                                      (
                                        string? dims1
                                      )
                                       (
                                        _substring dims1 0 (
                                          + 0 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? dims1
                                      )
                                       (
                                        hash-table-ref dims1 0
                                      )
                                    )
                                     (
                                      else (
                                        list-ref dims1 0
                                      )
                                    )
                                  )
                                   (
                                    cond (
                                      (
                                        string? dims1
                                      )
                                       (
                                        _substring dims1 1 (
                                          + 1 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? dims1
                                      )
                                       (
                                        hash-table-ref dims1 1
                                      )
                                    )
                                     (
                                      else (
                                        list-ref dims1 1
                                      )
                                    )
                                  )
                                   (
                                    cond (
                                      (
                                        string? dims2
                                      )
                                       (
                                        _substring dims2 0 (
                                          + 0 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? dims2
                                      )
                                       (
                                        hash-table-ref dims2 0
                                      )
                                    )
                                     (
                                      else (
                                        list-ref dims2 0
                                      )
                                    )
                                  )
                                   (
                                    cond (
                                      (
                                        string? dims2
                                      )
                                       (
                                        _substring dims2 1 (
                                          + 1 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? dims2
                                      )
                                       (
                                        hash-table-ref dims2 1
                                      )
                                    )
                                     (
                                      else (
                                        list-ref dims2 1
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                           (
                            cond (
                              (
                                string? v36
                              )
                               (
                                inexact->exact (
                                  floor (
                                    string->number v36
                                  )
                                )
                              )
                            )
                             (
                              (
                                boolean? v36
                              )
                               (
                                if v36 1 0
                              )
                            )
                             (
                              else (
                                inexact->exact (
                                  floor v36
                                )
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
                            size (
                              next_power_of_two maximum
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                new_matrix1 (
                                  pad_matrix matrix1 size size
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    new_matrix2 (
                                      pad_matrix matrix2 size size
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        result_padded (
                                          actual_strassen new_matrix1 new_matrix2
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            final_matrix (
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
                                                              _lt i (
                                                                cond (
                                                                  (
                                                                    string? dims1
                                                                  )
                                                                   (
                                                                    _substring dims1 0 (
                                                                      + 0 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? dims1
                                                                  )
                                                                   (
                                                                    hash-table-ref dims1 0
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref dims1 0
                                                                  )
                                                                )
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
                                                                            break40
                                                                          )
                                                                           (
                                                                            letrec (
                                                                              (
                                                                                loop39 (
                                                                                  lambda (
                                                                                    
                                                                                  )
                                                                                   (
                                                                                    if (
                                                                                      _lt j (
                                                                                        cond (
                                                                                          (
                                                                                            string? dims2
                                                                                          )
                                                                                           (
                                                                                            _substring dims2 1 (
                                                                                              + 1 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? dims2
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref dims2 1
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref dims2 1
                                                                                          )
                                                                                        )
                                                                                      )
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
                                                                                                        string? result_padded
                                                                                                      )
                                                                                                       (
                                                                                                        _substring result_padded i (
                                                                                                          + i 1
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      (
                                                                                                        hash-table? result_padded
                                                                                                      )
                                                                                                       (
                                                                                                        hash-table-ref result_padded i
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      else (
                                                                                                        list-ref result_padded i
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  _substring (
                                                                                                    cond (
                                                                                                      (
                                                                                                        string? result_padded
                                                                                                      )
                                                                                                       (
                                                                                                        _substring result_padded i (
                                                                                                          + i 1
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      (
                                                                                                        hash-table? result_padded
                                                                                                      )
                                                                                                       (
                                                                                                        hash-table-ref result_padded i
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      else (
                                                                                                        list-ref result_padded i
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   j (
                                                                                                    + j 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                (
                                                                                                  hash-table? (
                                                                                                    cond (
                                                                                                      (
                                                                                                        string? result_padded
                                                                                                      )
                                                                                                       (
                                                                                                        _substring result_padded i (
                                                                                                          + i 1
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      (
                                                                                                        hash-table? result_padded
                                                                                                      )
                                                                                                       (
                                                                                                        hash-table-ref result_padded i
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      else (
                                                                                                        list-ref result_padded i
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-ref (
                                                                                                    cond (
                                                                                                      (
                                                                                                        string? result_padded
                                                                                                      )
                                                                                                       (
                                                                                                        _substring result_padded i (
                                                                                                          + i 1
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      (
                                                                                                        hash-table? result_padded
                                                                                                      )
                                                                                                       (
                                                                                                        hash-table-ref result_padded i
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      else (
                                                                                                        list-ref result_padded i
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   j
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  list-ref (
                                                                                                    cond (
                                                                                                      (
                                                                                                        string? result_padded
                                                                                                      )
                                                                                                       (
                                                                                                        _substring result_padded i (
                                                                                                          + i 1
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      (
                                                                                                        hash-table? result_padded
                                                                                                      )
                                                                                                       (
                                                                                                        hash-table-ref result_padded i
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      else (
                                                                                                        list-ref result_padded i
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   j
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
                                                                                        loop39
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
                                                                              loop39
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        set! final_matrix (
                                                                          append final_matrix (
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
                                                ret35 final_matrix
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
            ret41
          )
           (
            let (
              (
                matrix1 (
                  _list (
                    _list 2 3 4 5
                  )
                   (
                    _list 6 4 3 1
                  )
                   (
                    _list 2 3 6 7
                  )
                   (
                    _list 3 1 2 4
                  )
                   (
                    _list 2 3 4 5
                  )
                   (
                    _list 6 4 3 1
                  )
                   (
                    _list 2 3 6 7
                  )
                   (
                    _list 3 1 2 4
                  )
                   (
                    _list 2 3 4 5
                  )
                   (
                    _list 6 2 3 1
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    matrix2 (
                      _list (
                        _list 0 2 1 1
                      )
                       (
                        _list 16 2 3 3
                      )
                       (
                        _list 2 2 7 7
                      )
                       (
                        _list 13 11 22 4
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        res (
                          strassen matrix1 matrix2
                        )
                      )
                    )
                     (
                      begin (
                        _display (
                          if (
                            string? res
                          )
                           res (
                            to-str res
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
     (
      let (
        (
          end43 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur44 (
              quotient (
                * (
                  - end43 start42
                )
                 1000000
              )
               jps45
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur44
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
