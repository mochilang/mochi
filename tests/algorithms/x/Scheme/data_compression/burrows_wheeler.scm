;; Generated on 2025-08-06 23:57 +0700
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
        all_rotations s
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                n (
                  _len s
                )
              )
            )
             (
              begin (
                let (
                  (
                    rotations (
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
                                      < i n
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            rotation (
                                              string-append (
                                                _substring s i n
                                              )
                                               (
                                                _substring s 0 i
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! rotations (
                                              append rotations (
                                                _list rotation
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
                        ret1 rotations
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
        sort_strings arr
      )
       (
        call/cc (
          lambda (
            ret4
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
                    i 1
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
                                  < i n
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
                                                          and (
                                                            >= j 0
                                                          )
                                                           (
                                                            string>? (
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
                    ret4 arr
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
        join_strings arr
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                res ""
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
                                  < i (
                                    _len arr
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      string-append res (
                                        list-ref arr i
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
                    ret9 res
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
        bwt_transform s
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            begin (
              if (
                string=? s ""
              )
               (
                begin (
                  panic "input string must not be empty"
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
                  rotations (
                    all_rotations s
                  )
                )
              )
               (
                begin (
                  set! rotations (
                    sort_strings rotations
                  )
                )
                 (
                  let (
                    (
                      last_col (
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
                                        < i (
                                          _len rotations
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              word (
                                                cond (
                                                  (
                                                    string? rotations
                                                  )
                                                   (
                                                    _substring rotations i (
                                                      + i 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? rotations
                                                  )
                                                   (
                                                    hash-table-ref rotations i
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref rotations i
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! last_col (
                                                append last_col (
                                                  _list (
                                                    _substring word (
                                                      - (
                                                        _len word
                                                      )
                                                       1
                                                    )
                                                     (
                                                      _len word
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
                              bwt_string (
                                join_strings last_col
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  idx (
                                    index_of rotations s
                                  )
                                )
                              )
                               (
                                begin (
                                  ret12 (
                                    alist->hash-table (
                                      _list (
                                        cons "bwt_string" bwt_string
                                      )
                                       (
                                        cons "idx_original_string" idx
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
        index_of arr target
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
                                _len arr
                              )
                            )
                             (
                              begin (
                                if (
                                  string=? (
                                    list-ref arr i
                                  )
                                   target
                                )
                                 (
                                  begin (
                                    ret15 i
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
                ret15 (
                  - 1
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        reverse_bwt bwt_string idx_original_string
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            begin (
              if (
                string=? bwt_string ""
              )
               (
                begin (
                  panic "bwt string must not be empty"
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
                  n (
                    _len bwt_string
                  )
                )
              )
               (
                begin (
                  if (
                    or (
                      < idx_original_string 0
                    )
                     (
                      >= idx_original_string n
                    )
                  )
                   (
                    begin (
                      panic "index out of range"
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
                      ordered_rotations (
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
                                        < i n
                                      )
                                       (
                                        begin (
                                          set! ordered_rotations (
                                            append ordered_rotations (
                                              _list ""
                                            )
                                          )
                                        )
                                         (
                                          set! i (
                                            + i 1
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
                              iter 0
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
                                            < iter n
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
                                                                < j n
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      ch (
                                                                        _substring bwt_string j (
                                                                          + j 1
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      list-set! ordered_rotations j (
                                                                        string-append ch (
                                                                          list-ref ordered_rotations j
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
                                                  set! ordered_rotations (
                                                    sort_strings ordered_rotations
                                                  )
                                                )
                                                 (
                                                  set! iter (
                                                    + iter 1
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
                              ret18 (
                                list-ref ordered_rotations idx_original_string
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
          s "^BANANA"
        )
      )
       (
        begin (
          let (
            (
              result (
                bwt_transform s
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    hash-table-ref result "bwt_string"
                  )
                )
                 (
                  hash-table-ref result "bwt_string"
                )
                 (
                  to-str (
                    hash-table-ref result "bwt_string"
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
                    hash-table-ref result "idx_original_string"
                  )
                )
                 (
                  hash-table-ref result "idx_original_string"
                )
                 (
                  to-str (
                    hash-table-ref result "idx_original_string"
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
                    reverse_bwt (
                      hash-table-ref result "bwt_string"
                    )
                     (
                      hash-table-ref result "idx_original_string"
                    )
                  )
                )
                 (
                  reverse_bwt (
                    hash-table-ref result "bwt_string"
                  )
                   (
                    hash-table-ref result "idx_original_string"
                  )
                )
                 (
                  to-str (
                    reverse_bwt (
                      hash-table-ref result "bwt_string"
                    )
                     (
                      hash-table-ref result "idx_original_string"
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
