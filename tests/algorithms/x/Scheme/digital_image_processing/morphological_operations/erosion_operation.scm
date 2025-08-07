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
      start28 (
        current-jiffy
      )
    )
     (
      jps31 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        rgb_to_gray rgb
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                gray (
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
                                    _len rgb
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
                                                          < j (
                                                            _len (
                                                              list-ref rgb i
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                r (
                                                                  + 0.0 (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref rgb i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref rgb i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref rgb i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref rgb i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref rgb i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref rgb i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref rgb i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref rgb i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref rgb i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref rgb i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                        )
                                                                         0 (
                                                                          + 0 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref rgb i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref rgb i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref rgb i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref rgb i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref rgb i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref rgb i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref rgb i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref rgb i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref rgb i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref rgb i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                        )
                                                                         0
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref rgb i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref rgb i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref rgb i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref rgb i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref rgb i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
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
                                                                    g (
                                                                      + 0.0 (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                   j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                   j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                            )
                                                                             1 (
                                                                              + 1 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                   j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                   j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                            )
                                                                             1
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                   j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref rgb i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                            )
                                                                             1
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
                                                                        b (
                                                                          + 0.0 (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                       j (
                                                                                        + j 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                       j
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                       j
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                       j (
                                                                                        + j 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                       j
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                       j
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 2 (
                                                                                  + 2 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                       j (
                                                                                        + j 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                       j
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                       j
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                       j (
                                                                                        + j 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                       j
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                       j
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 2
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                       j (
                                                                                        + j 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                       j
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref rgb i
                                                                                      )
                                                                                       j
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 2
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
                                                                            value (
                                                                              _add (
                                                                                _add (
                                                                                  * 0.2989 r
                                                                                )
                                                                                 (
                                                                                  * 0.587 g
                                                                                )
                                                                              )
                                                                               (
                                                                                * 0.114 b
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! row (
                                                                              append row (
                                                                                _list value
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
                                            set! gray (
                                              append gray (
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
                    ret1 gray
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
        gray_to_binary gray
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                binary (
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
                                    _len gray
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
                                                          < j (
                                                            _len (
                                                              list-ref gray i
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! row (
                                                              append row (
                                                                _list (
                                                                  and (
                                                                    > (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            list-ref gray i
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref gray i
                                                                          )
                                                                           j (
                                                                            + j 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref gray i
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref gray i
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            list-ref gray i
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                    )
                                                                     127.0
                                                                  )
                                                                   (
                                                                    <= (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            list-ref gray i
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref gray i
                                                                          )
                                                                           j (
                                                                            + j 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref gray i
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref gray i
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            list-ref gray i
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                    )
                                                                     255.0
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
                                            set! binary (
                                              append binary (
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
                    ret6 binary
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
        erosion image kernel
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            let (
              (
                h (
                  _len image
                )
              )
            )
             (
              begin (
                let (
                  (
                    w (
                      _len (
                        list-ref image 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        k_h (
                          _len kernel
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            k_w (
                              _len (
                                list-ref kernel 0
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                pad_y (
                                  _div k_h 2
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    pad_x (
                                      _div k_w 2
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        padded (
                                          _list
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            y 0
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
                                                          < y (
                                                            + h (
                                                              * 2 pad_y
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
                                                                    x 0
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
                                                                                  < x (
                                                                                    + w (
                                                                                      * 2 pad_x
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! row (
                                                                                      append row (
                                                                                        _list #f
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! x (
                                                                                      + x 1
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
                                                                    set! padded (
                                                                      append padded (
                                                                        _list row
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! y (
                                                                      + y 1
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
                                            set! y 0
                                          )
                                           (
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
                                                          < y h
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                x 0
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
                                                                              < x w
                                                                            )
                                                                             (
                                                                              begin (
                                                                                list-set! (
                                                                                  list-ref padded (
                                                                                    + pad_y y
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  + pad_x x
                                                                                )
                                                                                 (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref image y
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref image y
                                                                                      )
                                                                                       x (
                                                                                        + x 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref image y
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref image y
                                                                                      )
                                                                                       x
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref image y
                                                                                      )
                                                                                       x
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! x (
                                                                                  + x 1
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
                                                                set! y (
                                                                  + y 1
                                                                )
                                                              )
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
                                            let (
                                              (
                                                output (
                                                  _list
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! y 0
                                              )
                                               (
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
                                                              < y h
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    row_out (
                                                                      _list
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        x 0
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
                                                                                      < x w
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
                                                                                                ky 0
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
                                                                                                              < ky k_h
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                let (
                                                                                                                  (
                                                                                                                    kx 0
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
                                                                                                                                  < kx k_w
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  begin (
                                                                                                                                    if (
                                                                                                                                      and (
                                                                                                                                        equal? (
                                                                                                                                          cond (
                                                                                                                                            (
                                                                                                                                              string? (
                                                                                                                                                list-ref kernel ky
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              _substring (
                                                                                                                                                list-ref kernel ky
                                                                                                                                              )
                                                                                                                                               kx (
                                                                                                                                                + kx 1
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            (
                                                                                                                                              hash-table? (
                                                                                                                                                list-ref kernel ky
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              hash-table-ref (
                                                                                                                                                list-ref kernel ky
                                                                                                                                              )
                                                                                                                                               kx
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            else (
                                                                                                                                              list-ref (
                                                                                                                                                list-ref kernel ky
                                                                                                                                              )
                                                                                                                                               kx
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         1
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        cond (
                                                                                                                                          (
                                                                                                                                            string? (
                                                                                                                                              list-ref padded (
                                                                                                                                                + y ky
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            _substring (
                                                                                                                                              list-ref padded (
                                                                                                                                                + y ky
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              + x kx
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              + (
                                                                                                                                                + x kx
                                                                                                                                              )
                                                                                                                                               1
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          (
                                                                                                                                            hash-table? (
                                                                                                                                              list-ref padded (
                                                                                                                                                + y ky
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            hash-table-ref (
                                                                                                                                              list-ref padded (
                                                                                                                                                + y ky
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              + x kx
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          else (
                                                                                                                                            list-ref (
                                                                                                                                              list-ref padded (
                                                                                                                                                + y ky
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              + x kx
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      begin (
                                                                                                                                        set! sum (
                                                                                                                                          + sum 1
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      quote (
                                                                                                                                        
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    set! kx (
                                                                                                                                      + kx 1
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
                                                                                                                    set! ky (
                                                                                                                      + ky 1
                                                                                                                    )
                                                                                                                  )
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
                                                                                                set! row_out (
                                                                                                  append row_out (
                                                                                                    _list (
                                                                                                      equal? sum 5
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                set! x (
                                                                                                  + x 1
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
                                                                        set! output (
                                                                          append output (
                                                                            _list row_out
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        set! y (
                                                                          + y 1
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                loop20
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
                                                      loop20
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                ret11 output
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
          rgb_img (
            _list (
              _list (
                _list 127 255 0
              )
            )
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                to-str-space (
                  rgb_to_gray rgb_img
                )
              )
            )
             (
              to-str-space (
                rgb_to_gray rgb_img
              )
            )
             (
              to-str (
                to-str-space (
                  rgb_to_gray rgb_img
                )
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
              gray_img (
                _list (
                  _list 127.0 255.0 0.0
                )
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    to-str-space (
                      gray_to_binary gray_img
                    )
                  )
                )
                 (
                  to-str-space (
                    gray_to_binary gray_img
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      gray_to_binary gray_img
                    )
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
                  img1 (
                    _list (
                      _list #t #t #f
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      kernel1 (
                        _list (
                          _list 0 1 0
                        )
                      )
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            to-str-space (
                              erosion img1 kernel1
                            )
                          )
                        )
                         (
                          to-str-space (
                            erosion img1 kernel1
                          )
                        )
                         (
                          to-str (
                            to-str-space (
                              erosion img1 kernel1
                            )
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
                          img2 (
                            _list (
                              _list #t #f #f
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              kernel2 (
                                _list (
                                  _list 1 1 0
                                )
                              )
                            )
                          )
                           (
                            begin (
                              _display (
                                if (
                                  string? (
                                    to-str-space (
                                      erosion img2 kernel2
                                    )
                                  )
                                )
                                 (
                                  to-str-space (
                                    erosion img2 kernel2
                                  )
                                )
                                 (
                                  to-str (
                                    to-str-space (
                                      erosion img2 kernel2
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
     (
      let (
        (
          end29 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur30 (
              quotient (
                * (
                  - end29 start28
                )
                 1000000
              )
               jps31
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur30
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
