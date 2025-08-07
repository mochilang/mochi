;; Generated on 2025-08-07 08:40 +0700
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
      start25 (
        current-jiffy
      )
    )
     (
      jps28 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        distance_sq a b
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                sum 0.0
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
                                    _len a
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        diff (
                                          - (
                                            list-ref a i
                                          )
                                           (
                                            list-ref b i
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! sum (
                                          _add sum (
                                            * diff diff
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
                    ret1 sum
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
        mean vectors
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                dim (
                  _len (
                    list-ref vectors 0
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
                                      < i dim
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            total 0.0
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
                                                              < j (
                                                                _len vectors
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! total (
                                                                  + total (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref vectors j
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref vectors j
                                                                        )
                                                                         i (
                                                                          + i 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref vectors j
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref vectors j
                                                                        )
                                                                         i
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          list-ref vectors j
                                                                        )
                                                                         i
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
                                                set! res (
                                                  append res (
                                                    _list (
                                                      _div total (
                                                        _len vectors
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
                                            )
                                          )
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
     (
      define (
        k_means vectors k iterations
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                centroids (
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
                                  < i k
                                )
                                 (
                                  begin (
                                    set! centroids (
                                      append centroids (
                                        _list (
                                          list-ref vectors i
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
                    let (
                      (
                        assignments (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            n (
                              _len vectors
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
                                          < i n
                                        )
                                         (
                                          begin (
                                            set! assignments (
                                              append assignments (
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
                            let (
                              (
                                it 0
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
                                              < it iterations
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
                                                                  < v n
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        best 0
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            bestDist (
                                                                              distance_sq (
                                                                                list-ref vectors v
                                                                              )
                                                                               (
                                                                                list-ref centroids 0
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                c 1
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
                                                                                              < c k
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                let (
                                                                                                  (
                                                                                                    d (
                                                                                                      distance_sq (
                                                                                                        list-ref vectors v
                                                                                                      )
                                                                                                       (
                                                                                                        list-ref centroids c
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    if (
                                                                                                      < d bestDist
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        set! bestDist d
                                                                                                      )
                                                                                                       (
                                                                                                        set! best c
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      quote (
                                                                                                        
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
                                                                                list-set! assignments v best
                                                                              )
                                                                               (
                                                                                set! v (
                                                                                  + v 1
                                                                                )
                                                                              )
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
                                                        cIdx 0
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
                                                                      < cIdx k
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            cluster (
                                                                              _list
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                v2 0
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
                                                                                              < v2 n
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                if (
                                                                                                  equal? (
                                                                                                    list-ref assignments v2
                                                                                                  )
                                                                                                   cIdx
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    set! cluster (
                                                                                                      append cluster (
                                                                                                        _list (
                                                                                                          list-ref vectors v2
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
                                                                                                set! v2 (
                                                                                                  + v2 1
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
                                                                                if (
                                                                                  > (
                                                                                    _len cluster
                                                                                  )
                                                                                   0
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    list-set! centroids cIdx (
                                                                                      mean cluster
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  quote (
                                                                                    
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! cIdx (
                                                                                  + cIdx 1
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
                                                        set! it (
                                                          + it 1
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
                                ret9 (
                                  alist->hash-table (
                                    _list (
                                      cons "centroids" centroids
                                    )
                                     (
                                      cons "assignments" assignments
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
            ret24
          )
           (
            let (
              (
                vectors (
                  _list (
                    _list 1.0 2.0
                  )
                   (
                    _list 1.5 1.8
                  )
                   (
                    _list 5.0 8.0
                  )
                   (
                    _list 8.0 8.0
                  )
                   (
                    _list 1.0 0.6
                  )
                   (
                    _list 9.0 11.0
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    result (
                      k_means vectors 2 5
                    )
                  )
                )
                 (
                  begin (
                    _display (
                      if (
                        string? (
                          to-str-space (
                            hash-table-ref result "centroids"
                          )
                        )
                      )
                       (
                        to-str-space (
                          hash-table-ref result "centroids"
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            hash-table-ref result "centroids"
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
                          to-str-space (
                            hash-table-ref result "assignments"
                          )
                        )
                      )
                       (
                        to-str-space (
                          hash-table-ref result "assignments"
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            hash-table-ref result "assignments"
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
     (
      main
    )
     (
      let (
        (
          end26 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur27 (
              quotient (
                * (
                  - end26 start25
                )
                 1000000
              )
               jps28
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur27
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
