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
      define (
        index_of xs x
      )
       (
        call/cc (
          lambda (
            ret1
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
                                _len xs
                              )
                            )
                             (
                              begin (
                                if (
                                  equal? (
                                    list-ref xs i
                                  )
                                   x
                                )
                                 (
                                  begin (
                                    ret1 i
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
                ret1 (
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
        remove_item xs x
      )
       (
        call/cc (
          lambda (
            ret4
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
                    removed #f
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
                                        _len xs
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          and (
                                            not removed
                                          )
                                           (
                                            equal? (
                                              list-ref xs i
                                            )
                                             x
                                          )
                                        )
                                         (
                                          begin (
                                            set! removed #t
                                          )
                                        )
                                         (
                                          begin (
                                            set! res (
                                              append res (
                                                _list (
                                                  list-ref xs i
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
        stable_matching donor_pref recipient_pref
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len donor_pref
                  )
                   (
                    _len recipient_pref
                  )
                )
              )
               (
                begin (
                  panic "unequal groups"
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
                    _len donor_pref
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      unmatched (
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
                                        < i n
                                      )
                                       (
                                        begin (
                                          set! unmatched (
                                            append unmatched (
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
                          let (
                            (
                              donor_record (
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
                                            < i n
                                          )
                                           (
                                            begin (
                                              set! donor_record (
                                                append donor_record (
                                                  _list (
                                                    - 1
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
                                  rec_record (
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
                                                  set! rec_record (
                                                    append rec_record (
                                                      _list (
                                                        - 1
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
                                      num_donations (
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
                                                    < i n
                                                  )
                                                   (
                                                    begin (
                                                      set! num_donations (
                                                        append num_donations (
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
                                                    > (
                                                      _len unmatched
                                                    )
                                                     0
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          donor (
                                                            list-ref unmatched 0
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              donor_preference (
                                                                list-ref donor_pref donor
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  recipient (
                                                                    list-ref donor_preference (
                                                                      list-ref num_donations donor
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  list-set! num_donations donor (
                                                                    + (
                                                                      list-ref num_donations donor
                                                                    )
                                                                     1
                                                                  )
                                                                )
                                                                 (
                                                                  let (
                                                                    (
                                                                      rec_preference (
                                                                        list-ref recipient_pref recipient
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      let (
                                                                        (
                                                                          prev_donor (
                                                                            list-ref rec_record recipient
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          if (
                                                                            not (
                                                                              equal? prev_donor (
                                                                                - 0 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              let (
                                                                                (
                                                                                  prev_index (
                                                                                    index_of rec_preference prev_donor
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  let (
                                                                                    (
                                                                                      new_index (
                                                                                        index_of rec_preference donor
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      if (
                                                                                        _gt prev_index new_index
                                                                                      )
                                                                                       (
                                                                                        begin (
                                                                                          list-set! rec_record recipient donor
                                                                                        )
                                                                                         (
                                                                                          list-set! donor_record donor recipient
                                                                                        )
                                                                                         (
                                                                                          set! unmatched (
                                                                                            append unmatched (
                                                                                              _list prev_donor
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          set! unmatched (
                                                                                            remove_item unmatched donor
                                                                                          )
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
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              list-set! rec_record recipient donor
                                                                            )
                                                                             (
                                                                              list-set! donor_record donor recipient
                                                                            )
                                                                             (
                                                                              set! unmatched (
                                                                                remove_item unmatched donor
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
                                      ret7 donor_record
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
          donor_pref (
            _list (
              _list 0 1 3 2
            )
             (
              _list 0 2 3 1
            )
             (
              _list 1 0 2 3
            )
             (
              _list 0 3 1 2
            )
          )
        )
      )
       (
        begin (
          let (
            (
              recipient_pref (
                _list (
                  _list 3 1 2 0
                )
                 (
                  _list 3 1 0 2
                )
                 (
                  _list 0 3 1 2
                )
                 (
                  _list 1 0 3 2
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
                      stable_matching donor_pref recipient_pref
                    )
                  )
                )
                 (
                  to-str-space (
                    stable_matching donor_pref recipient_pref
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      stable_matching donor_pref recipient_pref
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
