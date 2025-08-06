;; Generated on 2025-08-06 22:04 +0700
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
      start29 (
        current-jiffy
      )
    )
     (
      jps32 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          SQUARE (
            _list (
              _list "a" "b" "c" "d" "e"
            )
             (
              _list "f" "g" "h" "i" "k"
            )
             (
              _list "l" "m" "n" "o" "p"
            )
             (
              _list "q" "r" "s" "t" "u"
            )
             (
              _list "v" "w" "x" "y" "z"
            )
          )
        )
      )
       (
        begin (
          define (
            index_of s ch
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
                                    _len s
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      string=? (
                                        _substring s i (
                                          + i 1
                                        )
                                      )
                                       ch
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
            to_lower_without_spaces message replace_j
          )
           (
            call/cc (
              lambda (
                ret4
              )
               (
                let (
                  (
                    upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                  )
                )
                 (
                  begin (
                    let (
                      (
                        lower "abcdefghijklmnopqrstuvwxyz"
                      )
                    )
                     (
                      begin (
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
                                                _len message
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    ch (
                                                      _substring message i (
                                                        + i 1
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        pos (
                                                          index_of upper ch
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          _ge pos 0
                                                        )
                                                         (
                                                          begin (
                                                            set! ch (
                                                              _substring lower pos (
                                                                + pos 1
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
                                                          not (
                                                            string=? ch " "
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              and replace_j (
                                                                string=? ch "j"
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! ch "i"
                                                              )
                                                            )
                                                             (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! res (
                                                              string-append res ch
                                                            )
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
          )
        )
         (
          define (
            letter_to_numbers letter
          )
           (
            call/cc (
              lambda (
                ret7
              )
               (
                let (
                  (
                    r 0
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
                                  < r (
                                    _len SQUARE
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        c 0
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
                                                      < c (
                                                        _len (
                                                          list-ref SQUARE r
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          string=? (
                                                            cond (
                                                              (
                                                                string? (
                                                                  list-ref SQUARE r
                                                                )
                                                              )
                                                               (
                                                                _substring (
                                                                  list-ref SQUARE r
                                                                )
                                                                 c (
                                                                  + c 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? (
                                                                  list-ref SQUARE r
                                                                )
                                                              )
                                                               (
                                                                hash-table-ref (
                                                                  list-ref SQUARE r
                                                                )
                                                                 c
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref (
                                                                  list-ref SQUARE r
                                                                )
                                                                 c
                                                              )
                                                            )
                                                          )
                                                           letter
                                                        )
                                                         (
                                                          begin (
                                                            ret7 (
                                                              _list (
                                                                + r 1
                                                              )
                                                               (
                                                                + c 1
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
                                                        set! c (
                                                          + c 1
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
                                        set! r (
                                          + r 1
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
                      _list 0 0
                    )
                  )
                )
              )
            )
          )
        )
         (
          define (
            numbers_to_letter row col
          )
           (
            call/cc (
              lambda (
                ret12
              )
               (
                ret12 (
                  cond (
                    (
                      string? (
                        list-ref SQUARE (
                          - row 1
                        )
                      )
                    )
                     (
                      _substring (
                        list-ref SQUARE (
                          - row 1
                        )
                      )
                       (
                        - col 1
                      )
                       (
                        + (
                          - col 1
                        )
                         1
                      )
                    )
                  )
                   (
                    (
                      hash-table? (
                        list-ref SQUARE (
                          - row 1
                        )
                      )
                    )
                     (
                      hash-table-ref (
                        list-ref SQUARE (
                          - row 1
                        )
                      )
                       (
                        - col 1
                      )
                    )
                  )
                   (
                    else (
                      list-ref (
                        list-ref SQUARE (
                          - row 1
                        )
                      )
                       (
                        - col 1
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
            encode message
          )
           (
            call/cc (
              lambda (
                ret13
              )
               (
                let (
                  (
                    clean (
                      to_lower_without_spaces message #t
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        l (
                          _len clean
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            rows (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                cols (
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
                                                  < i l
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        nums (
                                                          letter_to_numbers (
                                                            cond (
                                                              (
                                                                string? clean
                                                              )
                                                               (
                                                                _substring clean i (
                                                                  + i 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? clean
                                                              )
                                                               (
                                                                hash-table-ref clean i
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref clean i
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! rows (
                                                          append rows (
                                                            _list (
                                                              cond (
                                                                (
                                                                  string? nums
                                                                )
                                                                 (
                                                                  _substring nums 0 (
                                                                    + 0 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                (
                                                                  hash-table? nums
                                                                )
                                                                 (
                                                                  hash-table-ref nums 0
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref nums 0
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! cols (
                                                          append cols (
                                                            _list (
                                                              cond (
                                                                (
                                                                  string? nums
                                                                )
                                                                 (
                                                                  _substring nums 1 (
                                                                    + 1 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                (
                                                                  hash-table? nums
                                                                )
                                                                 (
                                                                  hash-table-ref nums 1
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref nums 1
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
                                    let (
                                      (
                                        seq (
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
                                                      < i l
                                                    )
                                                     (
                                                      begin (
                                                        set! seq (
                                                          append seq (
                                                            _list (
                                                              list-ref rows i
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
                                        set! i 0
                                      )
                                       (
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
                                                      < i l
                                                    )
                                                     (
                                                      begin (
                                                        set! seq (
                                                          append seq (
                                                            _list (
                                                              list-ref cols i
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
                                        let (
                                          (
                                            encoded ""
                                          )
                                        )
                                         (
                                          begin (
                                            set! i 0
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
                                                          < i l
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                r (
                                                                  list-ref seq (
                                                                    * 2 i
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    c (
                                                                      list-ref seq (
                                                                        + (
                                                                          * 2 i
                                                                        )
                                                                         1
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! encoded (
                                                                      string-append encoded (
                                                                        numbers_to_letter r c
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
                                            ret13 encoded
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
            decode message
          )
           (
            call/cc (
              lambda (
                ret22
              )
               (
                let (
                  (
                    clean (
                      to_lower_without_spaces message #f
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        l (
                          _len clean
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            first (
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
                                              < i l
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    nums (
                                                      letter_to_numbers (
                                                        cond (
                                                          (
                                                            string? clean
                                                          )
                                                           (
                                                            _substring clean i (
                                                              + i 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          (
                                                            hash-table? clean
                                                          )
                                                           (
                                                            hash-table-ref clean i
                                                          )
                                                        )
                                                         (
                                                          else (
                                                            list-ref clean i
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! first (
                                                      append first (
                                                        _list (
                                                          cond (
                                                            (
                                                              string? nums
                                                            )
                                                             (
                                                              _substring nums 0 (
                                                                + 0 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? nums
                                                            )
                                                             (
                                                              hash-table-ref nums 0
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref nums 0
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! first (
                                                      append first (
                                                        _list (
                                                          cond (
                                                            (
                                                              string? nums
                                                            )
                                                             (
                                                              _substring nums 1 (
                                                                + 1 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? nums
                                                            )
                                                             (
                                                              hash-table-ref nums 1
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref nums 1
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
                                let (
                                  (
                                    top (
                                      _list
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        bottom (
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
                                            break26
                                          )
                                           (
                                            letrec (
                                              (
                                                loop25 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < i l
                                                    )
                                                     (
                                                      begin (
                                                        set! top (
                                                          append top (
                                                            _list (
                                                              list-ref first i
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! bottom (
                                                          append bottom (
                                                            _list (
                                                              list-ref first (
                                                                + i l
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
                                                        loop25
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
                                              loop25
                                            )
                                          )
                                        )
                                      )
                                       (
                                        let (
                                          (
                                            decoded ""
                                          )
                                        )
                                         (
                                          begin (
                                            set! i 0
                                          )
                                           (
                                            call/cc (
                                              lambda (
                                                break28
                                              )
                                               (
                                                letrec (
                                                  (
                                                    loop27 (
                                                      lambda (
                                                        
                                                      )
                                                       (
                                                        if (
                                                          < i l
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                r (
                                                                  list-ref top i
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    c (
                                                                      list-ref bottom i
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! decoded (
                                                                      string-append decoded (
                                                                        numbers_to_letter r c
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
                                                            loop27
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
                                                  loop27
                                                )
                                              )
                                            )
                                          )
                                           (
                                            ret22 decoded
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
          _display (
            if (
              string? (
                encode "testmessage"
              )
            )
             (
              encode "testmessage"
            )
             (
              to-str (
                encode "testmessage"
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
                encode "Test Message"
              )
            )
             (
              encode "Test Message"
            )
             (
              to-str (
                encode "Test Message"
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
                encode "test j"
              )
            )
             (
              encode "test j"
            )
             (
              to-str (
                encode "test j"
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
                encode "test i"
              )
            )
             (
              encode "test i"
            )
             (
              to-str (
                encode "test i"
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
                decode "qtltbdxrxlk"
              )
            )
             (
              decode "qtltbdxrxlk"
            )
             (
              to-str (
                decode "qtltbdxrxlk"
              )
            )
          )
        )
         (
          newline
        )
      )
    )
     (
      let (
        (
          end30 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur31 (
              quotient (
                * (
                  - end30 start29
                )
                 1000000
              )
               jps32
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur31
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
