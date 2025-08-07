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
      start23 (
        current-jiffy
      )
    )
     (
      jps26 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        sqrtApprox x
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
                                    < i 20
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
        make_knn train_data train_target class_labels
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                items (
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
                                  < i (
                                    _len train_data
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        pl (
                                          alist->hash-table (
                                            _list (
                                              cons "point" (
                                                list-ref train_data i
                                              )
                                            )
                                             (
                                              cons "label" (
                                                list-ref train_target i
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! items (
                                          append items (
                                            _list pl
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
                    ret4 (
                      alist->hash-table (
                        _list (
                          cons "data" items
                        )
                         (
                          cons "labels" class_labels
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
        euclidean_distance a b
      )
       (
        call/cc (
          lambda (
            ret7
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
                    ret7 (
                      sqrtApprox sum
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
        classify knn pred_point k
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                distances (
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
                                    _len (
                                      hash-table-ref knn "data"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        d (
                                          euclidean_distance (
                                            hash-table-ref (
                                              list-ref (
                                                hash-table-ref knn "data"
                                              )
                                               i
                                            )
                                             "point"
                                          )
                                           pred_point
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! distances (
                                          append distances (
                                            _list (
                                              alist->hash-table (
                                                _list (
                                                  cons "dist" d
                                                )
                                                 (
                                                  cons "label" (
                                                    hash-table-ref (
                                                      list-ref (
                                                        hash-table-ref knn "data"
                                                      )
                                                       i
                                                    )
                                                     "label"
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
                    let (
                      (
                        votes (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            count 0
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
                                          < count k
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                min_index 0
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
                                                                    _len distances
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      < (
                                                                        hash-table-ref (
                                                                          list-ref distances j
                                                                        )
                                                                         "dist"
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref distances min_index
                                                                        )
                                                                         "dist"
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! min_index j
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
                                                    set! votes (
                                                      append votes (
                                                        _list (
                                                          hash-table-ref (
                                                            list-ref distances min_index
                                                          )
                                                           "label"
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    hash-table-set! (
                                                      list-ref distances min_index
                                                    )
                                                     "dist" 1
                                                  )
                                                   "e18" (
                                                    set! count (
                                                      + count 1
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
                            let (
                              (
                                tally (
                                  _list
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    t 0
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
                                                  < t (
                                                    _len (
                                                      hash-table-ref knn "labels"
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! tally (
                                                      append tally (
                                                        _list 0
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! t (
                                                      + t 1
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
                                        v 0
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
                                                      < v (
                                                        _len votes
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            lbl (
                                                              list-ref votes v
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            list-set! tally lbl (
                                                              + (
                                                                list-ref tally lbl
                                                              )
                                                               1
                                                            )
                                                          )
                                                           (
                                                            set! v (
                                                              + v 1
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
                                            max_idx 0
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                m 1
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
                                                              < m (
                                                                _len tally
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  > (
                                                                    list-ref tally m
                                                                  )
                                                                   (
                                                                    list-ref tally max_idx
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! max_idx m
                                                                  )
                                                                )
                                                                 (
                                                                  quote (
                                                                    
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! m (
                                                                  + m 1
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
                                                ret10 (
                                                  list-ref (
                                                    hash-table-ref knn "labels"
                                                  )
                                                   max_idx
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
      let (
        (
          train_X (
            _list (
              _list 0.0 0.0
            )
             (
              _list 1.0 0.0
            )
             (
              _list 0.0 1.0
            )
             (
              _list 0.5 0.5
            )
             (
              _list 3.0 3.0
            )
             (
              _list 2.0 3.0
            )
             (
              _list 3.0 2.0
            )
          )
        )
      )
       (
        begin (
          let (
            (
              train_y (
                _list 0 0 0 0 1 1 1
              )
            )
          )
           (
            begin (
              let (
                (
                  classes (
                    _list "A" "B"
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      knn (
                        make_knn train_X train_y classes
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          point (
                            _list 1.2 1.2
                          )
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? (
                                classify knn point 5
                              )
                            )
                             (
                              classify knn point 5
                            )
                             (
                              to-str (
                                classify knn point 5
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
     (
      let (
        (
          end24 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur25 (
              quotient (
                * (
                  - end24 start23
                )
                 1000000
              )
               jps26
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur25
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
