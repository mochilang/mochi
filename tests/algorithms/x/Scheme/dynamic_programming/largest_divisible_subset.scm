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
      define (
        sort_list nums
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                arr nums
              )
            )
             (
              begin (
                let (
                  (
                    i 1
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
                                    _len arr
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        key (
                                          list-ref arr i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            j (
                                              - i 1
                                            )
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
                                                          and (
                                                            >= j 0
                                                          )
                                                           (
                                                            > (
                                                              list-ref arr j
                                                            )
                                                             key
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            list-set! arr (
                                                              + j 1
                                                            )
                                                             (
                                                              list-ref arr j
                                                            )
                                                          )
                                                           (
                                                            set! j (
                                                              - j 1
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
                                            list-set! arr (
                                              + j 1
                                            )
                                             key
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
                    ret1 arr
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
        largest_divisible_subset items
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            begin (
              if (
                equal? (
                  _len items
                )
                 0
              )
               (
                begin (
                  ret6 (
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
                  nums (
                    sort_list items
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      n (
                        _len nums
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          memo (
                            _list
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              prev (
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
                                                < i n
                                              )
                                               (
                                                begin (
                                                  set! memo (
                                                    append memo (
                                                      _list 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  set! prev (
                                                    append prev (
                                                      _list i
                                                    )
                                                  )
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
                                  set! i 0
                                )
                                 (
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
                                                < i n
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
                                                                    < j i
                                                                  )
                                                                   (
                                                                    begin (
                                                                      if (
                                                                        and (
                                                                          or (
                                                                            equal? (
                                                                              cond (
                                                                                (
                                                                                  string? nums
                                                                                )
                                                                                 (
                                                                                  _substring nums j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? nums
                                                                                )
                                                                                 (
                                                                                  hash-table-ref nums j
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref nums j
                                                                                )
                                                                              )
                                                                            )
                                                                             0
                                                                          )
                                                                           (
                                                                            equal? (
                                                                              _mod (
                                                                                cond (
                                                                                  (
                                                                                    string? nums
                                                                                  )
                                                                                   (
                                                                                    _substring nums i (
                                                                                      + i 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? nums
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref nums i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref nums i
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                cond (
                                                                                  (
                                                                                    string? nums
                                                                                  )
                                                                                   (
                                                                                    _substring nums j (
                                                                                      + j 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? nums
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref nums j
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref nums j
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             0
                                                                          )
                                                                        )
                                                                         (
                                                                          > (
                                                                            + (
                                                                              list-ref memo j
                                                                            )
                                                                             1
                                                                          )
                                                                           (
                                                                            list-ref memo i
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          list-set! memo i (
                                                                            + (
                                                                              list-ref memo j
                                                                            )
                                                                             1
                                                                          )
                                                                        )
                                                                         (
                                                                          list-set! prev i j
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
                                                      set! i (
                                                        + i 1
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
                                  let (
                                    (
                                      ans (
                                        - 0 1
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          last_index (
                                            - 0 1
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
                                                        < i n
                                                      )
                                                       (
                                                        begin (
                                                          if (
                                                            > (
                                                              list-ref memo i
                                                            )
                                                             ans
                                                          )
                                                           (
                                                            begin (
                                                              set! ans (
                                                                list-ref memo i
                                                              )
                                                            )
                                                             (
                                                              set! last_index i
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
                                            equal? last_index (
                                              - 0 1
                                            )
                                          )
                                           (
                                            begin (
                                              ret6 (
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
                                                _list (
                                                  cond (
                                                    (
                                                      string? nums
                                                    )
                                                     (
                                                      _substring nums last_index (
                                                        + last_index 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? nums
                                                    )
                                                     (
                                                      hash-table-ref nums last_index
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref nums last_index
                                                    )
                                                  )
                                                )
                                              )
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
                                                            not (
                                                              equal? (
                                                                list-ref prev last_index
                                                              )
                                                               last_index
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              set! last_index (
                                                                list-ref prev last_index
                                                              )
                                                            )
                                                             (
                                                              set! result (
                                                                append result (
                                                                  _list (
                                                                    cond (
                                                                      (
                                                                        string? nums
                                                                      )
                                                                       (
                                                                        _substring nums last_index (
                                                                          + last_index 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? nums
                                                                      )
                                                                       (
                                                                        hash-table-ref nums last_index
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref nums last_index
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
                                              ret6 result
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
            ret17
          )
           (
            let (
              (
                items (
                  _list 1 16 7 8 4
                )
              )
            )
             (
              begin (
                let (
                  (
                    subset (
                      largest_divisible_subset items
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
                                string-append "The longest divisible subset of " (
                                  to-str-space items
                                )
                              )
                               " is "
                            )
                             (
                              to-str-space subset
                            )
                          )
                           "."
                        )
                      )
                       (
                        string-append (
                          string-append (
                            string-append (
                              string-append "The longest divisible subset of " (
                                to-str-space items
                              )
                            )
                             " is "
                          )
                           (
                            to-str-space subset
                          )
                        )
                         "."
                      )
                       (
                        to-str (
                          string-append (
                            string-append (
                              string-append (
                                string-append "The longest divisible subset of " (
                                  to-str-space items
                                )
                              )
                               " is "
                            )
                             (
                              to-str-space subset
                            )
                          )
                           "."
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
