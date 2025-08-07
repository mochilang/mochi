;; Generated on 2025-08-07 08:56 +0700
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
      start18 (
        current-jiffy
      )
    )
     (
      jps21 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          INF 1000000000.0
        )
      )
       (
        begin (
          define (
            floyd_warshall graph
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                let (
                  (
                    v (
                      _len graph
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        dist (
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
                                          < i v
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
                                                                  < j v
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! row (
                                                                      append row (
                                                                        _list (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref graph i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref graph i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref graph i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref graph i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref graph i
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
                                                    set! dist (
                                                      append dist (
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
                            let (
                              (
                                k 0
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
                                              < k v
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
                                                                  < i v
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
                                                                                      < j v
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        if (
                                                                                          and (
                                                                                            and (
                                                                                              < (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? (
                                                                                                      list-ref dist i
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    _substring (
                                                                                                      list-ref dist i
                                                                                                    )
                                                                                                     k (
                                                                                                      + k 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  (
                                                                                                    hash-table? (
                                                                                                      list-ref dist i
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    hash-table-ref (
                                                                                                      list-ref dist i
                                                                                                    )
                                                                                                     k
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    list-ref (
                                                                                                      list-ref dist i
                                                                                                    )
                                                                                                     k
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               INF
                                                                                            )
                                                                                             (
                                                                                              < (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? (
                                                                                                      list-ref dist k
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    _substring (
                                                                                                      list-ref dist k
                                                                                                    )
                                                                                                     j (
                                                                                                      + j 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  (
                                                                                                    hash-table? (
                                                                                                      list-ref dist k
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    hash-table-ref (
                                                                                                      list-ref dist k
                                                                                                    )
                                                                                                     j
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    list-ref (
                                                                                                      list-ref dist k
                                                                                                    )
                                                                                                     j
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               INF
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            _lt (
                                                                                              + (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? (
                                                                                                      list-ref dist i
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    _substring (
                                                                                                      list-ref dist i
                                                                                                    )
                                                                                                     k (
                                                                                                      + k 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  (
                                                                                                    hash-table? (
                                                                                                      list-ref dist i
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    hash-table-ref (
                                                                                                      list-ref dist i
                                                                                                    )
                                                                                                     k
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    list-ref (
                                                                                                      list-ref dist i
                                                                                                    )
                                                                                                     k
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? (
                                                                                                      list-ref dist k
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    _substring (
                                                                                                      list-ref dist k
                                                                                                    )
                                                                                                     j (
                                                                                                      + j 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  (
                                                                                                    hash-table? (
                                                                                                      list-ref dist k
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    hash-table-ref (
                                                                                                      list-ref dist k
                                                                                                    )
                                                                                                     j
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    list-ref (
                                                                                                      list-ref dist k
                                                                                                    )
                                                                                                     j
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              cond (
                                                                                                (
                                                                                                  string? (
                                                                                                    list-ref dist i
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  _substring (
                                                                                                    list-ref dist i
                                                                                                  )
                                                                                                   j (
                                                                                                    + j 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                (
                                                                                                  hash-table? (
                                                                                                    list-ref dist i
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-ref (
                                                                                                    list-ref dist i
                                                                                                  )
                                                                                                   j
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  list-ref (
                                                                                                    list-ref dist i
                                                                                                  )
                                                                                                   j
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            list-set! (
                                                                                              list-ref dist i
                                                                                            )
                                                                                             j (
                                                                                              + (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? (
                                                                                                      list-ref dist i
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    _substring (
                                                                                                      list-ref dist i
                                                                                                    )
                                                                                                     k (
                                                                                                      + k 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  (
                                                                                                    hash-table? (
                                                                                                      list-ref dist i
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    hash-table-ref (
                                                                                                      list-ref dist i
                                                                                                    )
                                                                                                     k
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    list-ref (
                                                                                                      list-ref dist i
                                                                                                    )
                                                                                                     k
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? (
                                                                                                      list-ref dist k
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    _substring (
                                                                                                      list-ref dist k
                                                                                                    )
                                                                                                     j (
                                                                                                      + j 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  (
                                                                                                    hash-table? (
                                                                                                      list-ref dist k
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    hash-table-ref (
                                                                                                      list-ref dist k
                                                                                                    )
                                                                                                     j
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    list-ref (
                                                                                                      list-ref dist k
                                                                                                    )
                                                                                                     j
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
                                                                        set! i (
                                                                          + i 1
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
                                                    set! k (
                                                      + k 1
                                                    )
                                                  )
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
                                ret1 dist
                              )
                            )
                          )
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
            print_dist dist
          )
           (
            call/cc (
              lambda (
                ret12
              )
               (
                begin (
                  _display (
                    if (
                      string? "\nThe shortest path matrix using Floyd Warshall algorithm\n"
                    )
                     "\nThe shortest path matrix using Floyd Warshall algorithm\n" (
                      to-str "\nThe shortest path matrix using Floyd Warshall algorithm\n"
                    )
                  )
                )
                 (
                  newline
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
                                      _len dist
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
                                              line ""
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
                                                              _len (
                                                                list-ref dist i
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              if (
                                                                _ge (
                                                                  cond (
                                                                    (
                                                                      string? (
                                                                        list-ref dist i
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref dist i
                                                                      )
                                                                       j (
                                                                        + j 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        list-ref dist i
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref dist i
                                                                      )
                                                                       j
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref (
                                                                        list-ref dist i
                                                                      )
                                                                       j
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  _div INF 2.0
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  set! line (
                                                                    string-append line "INF\t"
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  set! line (
                                                                    string-append (
                                                                      string-append line (
                                                                        to-str-space (
                                                                          let (
                                                                            (
                                                                              v17 (
                                                                                cond (
                                                                                  (
                                                                                    string? (
                                                                                      list-ref dist i
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref dist i
                                                                                    )
                                                                                     j (
                                                                                      + j 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref dist i
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref dist i
                                                                                    )
                                                                                     j
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref (
                                                                                      list-ref dist i
                                                                                    )
                                                                                     j
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            cond (
                                                                              (
                                                                                string? v17
                                                                              )
                                                                               (
                                                                                exact (
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
                                                                                exact (
                                                                                  floor v17
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     "\t"
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
                                              _display (
                                                if (
                                                  string? line
                                                )
                                                 line (
                                                  to-str line
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
                  )
                )
              )
            )
          )
        )
         (
          let (
            (
              graph (
                _list (
                  _list 0.0 5.0 INF 10.0
                )
                 (
                  _list INF 0.0 3.0 INF
                )
                 (
                  _list INF INF 0.0 1.0
                )
                 (
                  _list INF INF INF 0.0
                )
              )
            )
          )
           (
            begin (
              let (
                (
                  result (
                    floyd_warshall graph
                  )
                )
              )
               (
                begin (
                  print_dist result
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
          end19 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur20 (
              quotient (
                * (
                  - end19 start18
                )
                 1000000
              )
               jps21
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur20
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
