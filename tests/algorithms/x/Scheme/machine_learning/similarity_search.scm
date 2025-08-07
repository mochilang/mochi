;; Generated on 2025-08-07 10:06 +0700
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
      start17 (
        current-jiffy
      )
    )
     (
      jps20 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        sqrt x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                <= x 0.0
              )
               (
                begin (
                  ret1 0.0
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
                  guess x
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
                                    < i 10
                                  )
                                   (
                                    begin (
                                      set! guess (
                                        _div (
                                          _add guess (
                                            _div x guess
                                          )
                                        )
                                         2.0
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
                      ret1 guess
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
        euclidean a b
      )
       (
        call/cc (
          lambda (
            ret4
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
                    let (
                      (
                        res (
                          sqrt sum
                        )
                      )
                    )
                     (
                      begin (
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
        similarity_search dataset value_array
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                dim (
                  _len (
                    list-ref dataset 0
                  )
                )
              )
            )
             (
              begin (
                if (
                  not (
                    equal? dim (
                      _len (
                        list-ref value_array 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    ret7 (
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
                                        _len value_array
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            value (
                                              list-ref value_array i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                dist (
                                                  euclidean value (
                                                    list-ref dataset 0
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    vec (
                                                      list-ref dataset 0
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        j 1
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
                                                                        _len dataset
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            d (
                                                                              euclidean value (
                                                                                list-ref dataset j
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            if (
                                                                              < d dist
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! dist d
                                                                              )
                                                                               (
                                                                                set! vec (
                                                                                  list-ref dataset j
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
                                                            nb (
                                                              alist->hash-table (
                                                                _list (
                                                                  cons "vector" vec
                                                                )
                                                                 (
                                                                  cons "distance" dist
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! result (
                                                              append result (
                                                                _list nb
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
      )
    )
     (
      define (
        cosine_similarity a b
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                dot 0.0
              )
            )
             (
              begin (
                let (
                  (
                    norm_a 0.0
                  )
                )
                 (
                  begin (
                    let (
                      (
                        norm_b 0.0
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
                                            _len a
                                          )
                                        )
                                         (
                                          begin (
                                            set! dot (
                                              _add dot (
                                                * (
                                                  list-ref a i
                                                )
                                                 (
                                                  list-ref b i
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! norm_a (
                                              _add norm_a (
                                                * (
                                                  list-ref a i
                                                )
                                                 (
                                                  list-ref a i
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! norm_b (
                                              _add norm_b (
                                                * (
                                                  list-ref b i
                                                )
                                                 (
                                                  list-ref b i
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
                              or (
                                equal? norm_a 0.0
                              )
                               (
                                equal? norm_b 0.0
                              )
                            )
                             (
                              begin (
                                ret12 0.0
                              )
                            )
                             (
                              quote (
                                
                              )
                            )
                          )
                           (
                            ret12 (
                              _div dot (
                                * (
                                  sqrt norm_a
                                )
                                 (
                                  sqrt norm_b
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
          dataset (
            _list (
              _list 0.0 0.0 0.0
            )
             (
              _list 1.0 1.0 1.0
            )
             (
              _list 2.0 2.0 2.0
            )
          )
        )
      )
       (
        begin (
          let (
            (
              value_array (
                _list (
                  _list 0.0 0.0 0.0
                )
                 (
                  _list 0.0 0.0 1.0
                )
              )
            )
          )
           (
            begin (
              let (
                (
                  neighbors (
                    similarity_search dataset value_array
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
                                    < k (
                                      _len neighbors
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          n (
                                            cond (
                                              (
                                                string? neighbors
                                              )
                                               (
                                                _substring neighbors k (
                                                  + k 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? neighbors
                                              )
                                               (
                                                hash-table-ref neighbors k
                                              )
                                            )
                                             (
                                              else (
                                                list-ref neighbors k
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
                                                string-append (
                                                  string-append (
                                                    string-append (
                                                      string-append "[" (
                                                        to-str-space (
                                                          hash-table-ref n "vector"
                                                        )
                                                      )
                                                    )
                                                     ", "
                                                  )
                                                   (
                                                    to-str-space (
                                                      hash-table-ref n "distance"
                                                    )
                                                  )
                                                )
                                                 "]"
                                              )
                                            )
                                             (
                                              string-append (
                                                string-append (
                                                  string-append (
                                                    string-append "[" (
                                                      to-str-space (
                                                        hash-table-ref n "vector"
                                                      )
                                                    )
                                                  )
                                                   ", "
                                                )
                                                 (
                                                  to-str-space (
                                                    hash-table-ref n "distance"
                                                  )
                                                )
                                              )
                                               "]"
                                            )
                                             (
                                              to-str (
                                                string-append (
                                                  string-append (
                                                    string-append (
                                                      string-append "[" (
                                                        to-str-space (
                                                          hash-table-ref n "vector"
                                                        )
                                                      )
                                                    )
                                                     ", "
                                                  )
                                                   (
                                                    to-str-space (
                                                      hash-table-ref n "distance"
                                                    )
                                                  )
                                                )
                                                 "]"
                                              )
                                            )
                                          )
                                        )
                                         (
                                          newline
                                        )
                                         (
                                          set! k (
                                            + k 1
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
                      _display (
                        if (
                          string? (
                            to-str-space (
                              cosine_similarity (
                                _list 1.0 2.0
                              )
                               (
                                _list 6.0 32.0
                              )
                            )
                          )
                        )
                         (
                          to-str-space (
                            cosine_similarity (
                              _list 1.0 2.0
                            )
                             (
                              _list 6.0 32.0
                            )
                          )
                        )
                         (
                          to-str (
                            to-str-space (
                              cosine_similarity (
                                _list 1.0 2.0
                              )
                               (
                                _list 6.0 32.0
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
      let (
        (
          end18 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur19 (
              quotient (
                * (
                  - end18 start17
                )
                 1000000
              )
               jps20
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur19
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
