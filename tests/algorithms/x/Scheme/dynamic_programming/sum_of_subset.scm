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
      start15 (
        current-jiffy
      )
    )
     (
      jps18 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        create_bool_matrix rows cols
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                matrix (
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
                                  <= i rows
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
                                                          <= j cols
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
                                            set! matrix (
                                              append matrix (
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
                    ret1 matrix
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
        is_sum_subset arr required_sum
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                arr_len (
                  _len arr
                )
              )
            )
             (
              begin (
                let (
                  (
                    subset (
                      create_bool_matrix arr_len required_sum
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
                                      <= i arr_len
                                    )
                                     (
                                      begin (
                                        list-set! (
                                          list-ref subset i
                                        )
                                         0 #t
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
                        let (
                          (
                            j 1
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
                                          <= j required_sum
                                        )
                                         (
                                          begin (
                                            list-set! (
                                              list-ref subset 0
                                            )
                                             j #f
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
                            set! i 1
                          )
                           (
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
                                          <= i arr_len
                                        )
                                         (
                                          begin (
                                            set! j 1
                                          )
                                           (
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
                                                          <= j required_sum
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              > (
                                                                list-ref arr (
                                                                  - i 1
                                                                )
                                                              )
                                                               j
                                                            )
                                                             (
                                                              begin (
                                                                list-set! (
                                                                  list-ref subset i
                                                                )
                                                                 j (
                                                                  cond (
                                                                    (
                                                                      string? (
                                                                        list-ref subset (
                                                                          - i 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref subset (
                                                                          - i 1
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
                                                                        list-ref subset (
                                                                          - i 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref subset (
                                                                          - i 1
                                                                        )
                                                                      )
                                                                       j
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref (
                                                                        list-ref subset (
                                                                          - i 1
                                                                        )
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
                                                            if (
                                                              <= (
                                                                list-ref arr (
                                                                  - i 1
                                                                )
                                                              )
                                                               j
                                                            )
                                                             (
                                                              begin (
                                                                list-set! (
                                                                  list-ref subset i
                                                                )
                                                                 j (
                                                                  or (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref subset (
                                                                            - i 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref subset (
                                                                            - i 1
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
                                                                          list-ref subset (
                                                                            - i 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref subset (
                                                                            - i 1
                                                                          )
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          list-ref subset (
                                                                            - i 1
                                                                          )
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref subset (
                                                                            - i 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref subset (
                                                                            - i 1
                                                                          )
                                                                        )
                                                                         (
                                                                          - j (
                                                                            list-ref arr (
                                                                              - i 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          + (
                                                                            - j (
                                                                              list-ref arr (
                                                                                - i 1
                                                                              )
                                                                            )
                                                                          )
                                                                           1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref subset (
                                                                            - i 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref subset (
                                                                            - i 1
                                                                          )
                                                                        )
                                                                         (
                                                                          - j (
                                                                            list-ref arr (
                                                                              - i 1
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          list-ref subset (
                                                                            - i 1
                                                                          )
                                                                        )
                                                                         (
                                                                          - j (
                                                                            list-ref arr (
                                                                              - i 1
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
                                            set! i (
                                              + i 1
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
                            ret6 (
                              cond (
                                (
                                  string? (
                                    list-ref subset arr_len
                                  )
                                )
                                 (
                                  _substring (
                                    list-ref subset arr_len
                                  )
                                   required_sum (
                                    + required_sum 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? (
                                    list-ref subset arr_len
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    list-ref subset arr_len
                                  )
                                   required_sum
                                )
                              )
                               (
                                else (
                                  list-ref (
                                    list-ref subset arr_len
                                  )
                                   required_sum
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
            is_sum_subset (
              _list 2 4 6 8
            )
             5
          )
        )
         (
          is_sum_subset (
            _list 2 4 6 8
          )
           5
        )
         (
          to-str (
            is_sum_subset (
              _list 2 4 6 8
            )
             5
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
            is_sum_subset (
              _list 2 4 6 8
            )
             14
          )
        )
         (
          is_sum_subset (
            _list 2 4 6 8
          )
           14
        )
         (
          to-str (
            is_sum_subset (
              _list 2 4 6 8
            )
             14
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
          end16 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur17 (
              quotient (
                * (
                  - end16 start15
                )
                 1000000
              )
               jps18
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur17
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
