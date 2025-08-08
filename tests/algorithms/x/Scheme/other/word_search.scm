;; Generated on 2025-08-08 15:49 +0700
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
        ((number? x)
         (if (integer? x)
             (number->string (inexact->exact x))
             (number->string x)))
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
(define (_div a b) (if (and (integer? a) (integer? b) (exact? a) (exact? b)) (quotient a b) (/ a b)))
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
(define (list-ref-safe lst idx) (if (and (integer? idx) (>= idx 0) (< idx (length lst))) (list-ref lst idx) '()))
(
  let (
    (
      start34 (
        current-jiffy
      )
    )
     (
      jps37 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          seed 123456789
        )
      )
       (
        begin (
          define (
            rand
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                begin (
                  set! seed (
                    _mod (
                      + (
                        * seed 1103515245
                      )
                       12345
                    )
                     2147483648
                  )
                )
                 (
                  ret1 seed
                )
              )
            )
          )
        )
         (
          define (
            rand_range max
          )
           (
            call/cc (
              lambda (
                ret2
              )
               (
                ret2 (
                  _mod (
                    rand
                  )
                   max
                )
              )
            )
          )
        )
         (
          define (
            shuffle list_int
          )
           (
            call/cc (
              lambda (
                ret3
              )
               (
                let (
                  (
                    i (
                      - (
                        _len list_int
                      )
                       1
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
                                  > i 0
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        j (
                                          rand_range (
                                            + i 1
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            tmp (
                                              list-ref-safe list_int i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            list-set! list_int i (
                                              list-ref-safe list_int j
                                            )
                                          )
                                           (
                                            list-set! list_int j tmp
                                          )
                                           (
                                            set! i (
                                              - i 1
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
                                 '(
                                  
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
                    ret3 list_int
                  )
                )
              )
            )
          )
        )
         (
          define (
            rand_letter
          )
           (
            call/cc (
              lambda (
                ret6
              )
               (
                let (
                  (
                    letters "abcdefghijklmnopqrstuvwxyz"
                  )
                )
                 (
                  begin (
                    let (
                      (
                        i (
                          rand_range 26
                        )
                      )
                    )
                     (
                      begin (
                        ret6 (
                          _substring letters i (
                            _add i 1
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
            make_word_search words width height
          )
           (
            call/cc (
              lambda (
                ret7
              )
               (
                let (
                  (
                    board (
                      _list
                    )
                  )
                )
                 (
                  begin (
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
                                      < r height
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
                                                              < c width
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list ""
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
                                                             '(
                                                              
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
                                                set! board (
                                                  append board (
                                                    _list row
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
                                        )
                                      )
                                       (
                                        loop8
                                      )
                                    )
                                     '(
                                      
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
                          alist->hash-table (
                            _list (
                              cons "words" words
                            )
                             (
                              cons "width" width
                            )
                             (
                              cons "height" height
                            )
                             (
                              cons "board" board
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
            insert_dir ws word dr dc rows cols
          )
           (
            call/cc (
              lambda (
                ret12
              )
               (
                let (
                  (
                    word_len (
                      _len word
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        ri 0
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
                                      < ri (
                                        _len rows
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            row (
                                              list-ref-safe rows ri
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                ci 0
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
                                                              < ci (
                                                                _len cols
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    col (
                                                                      list-ref-safe cols ci
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        end_r (
                                                                          + row (
                                                                            * dr (
                                                                              - word_len 1
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            end_c (
                                                                              + col (
                                                                                * dc (
                                                                                  - word_len 1
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            if (
                                                                              or (
                                                                                or (
                                                                                  or (
                                                                                    < end_r 0
                                                                                  )
                                                                                   (
                                                                                    >= end_r (
                                                                                      hash-table-ref ws "height"
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  < end_c 0
                                                                                )
                                                                              )
                                                                               (
                                                                                >= end_c (
                                                                                  hash-table-ref ws "width"
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! ci (
                                                                                  + ci 1
                                                                                )
                                                                              )
                                                                               (
                                                                                loop15
                                                                              )
                                                                            )
                                                                             '(
                                                                              
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
                                                                                let (
                                                                                  (
                                                                                    ok #t
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
                                                                                                  < k word_len
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    let (
                                                                                                      (
                                                                                                        rr (
                                                                                                          + row (
                                                                                                            * dr k
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        let (
                                                                                                          (
                                                                                                            cc (
                                                                                                              + col (
                                                                                                                * dc k
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            if (
                                                                                                              not (
                                                                                                                equal? (
                                                                                                                  cond (
                                                                                                                    (
                                                                                                                      string? (
                                                                                                                        list-ref-safe (
                                                                                                                          hash-table-ref ws "board"
                                                                                                                        )
                                                                                                                         rr
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      _substring (
                                                                                                                        list-ref-safe (
                                                                                                                          hash-table-ref ws "board"
                                                                                                                        )
                                                                                                                         rr
                                                                                                                      )
                                                                                                                       cc (
                                                                                                                        + cc 1
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    (
                                                                                                                      hash-table? (
                                                                                                                        list-ref-safe (
                                                                                                                          hash-table-ref ws "board"
                                                                                                                        )
                                                                                                                         rr
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      hash-table-ref (
                                                                                                                        list-ref-safe (
                                                                                                                          hash-table-ref ws "board"
                                                                                                                        )
                                                                                                                         rr
                                                                                                                      )
                                                                                                                       cc
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    else (
                                                                                                                      list-ref-safe (
                                                                                                                        list-ref-safe (
                                                                                                                          hash-table-ref ws "board"
                                                                                                                        )
                                                                                                                         rr
                                                                                                                      )
                                                                                                                       cc
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 ""
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                set! ok #f
                                                                                                              )
                                                                                                               (
                                                                                                                break18 '(
                                                                                                                  
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             '(
                                                                                                              
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            set! k (
                                                                                                              + k 1
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
                                                                                                 '(
                                                                                                  
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
                                                                                    if ok (
                                                                                      begin (
                                                                                        set! k 0
                                                                                      )
                                                                                       (
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
                                                                                                      < k word_len
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        let (
                                                                                                          (
                                                                                                            rr2 (
                                                                                                              + row (
                                                                                                                * dr k
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            let (
                                                                                                              (
                                                                                                                cc2 (
                                                                                                                  + col (
                                                                                                                    * dc k
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                let (
                                                                                                                  (
                                                                                                                    row_list (
                                                                                                                      list-ref-safe (
                                                                                                                        hash-table-ref ws "board"
                                                                                                                      )
                                                                                                                       rr2
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    list-set! row_list cc2 (
                                                                                                                      _substring word k (
                                                                                                                        + k 1
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
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        loop19
                                                                                                      )
                                                                                                    )
                                                                                                     '(
                                                                                                      
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
                                                                                        ret12 #t
                                                                                      )
                                                                                    )
                                                                                     '(
                                                                                      
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! ci (
                                                                                      + ci 1
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
                                                                loop15
                                                              )
                                                            )
                                                             '(
                                                              
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
                                                set! ri (
                                                  + ri 1
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
                                     '(
                                      
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
                        ret12 #f
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
            generate_board ws
          )
           (
            call/cc (
              lambda (
                ret21
              )
               (
                let (
                  (
                    dirs_r (
                      _list (
                        - 1
                      )
                       (
                        - 1
                      )
                       0 1 1 1 0 (
                        - 1
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        dirs_c (
                          _list 0 1 1 1 0 (
                            - 1
                          )
                           (
                            - 1
                          )
                           (
                            - 1
                          )
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
                                          < i (
                                            _len (
                                              hash-table-ref ws "words"
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                word (
                                                  list-ref-safe (
                                                    hash-table-ref ws "words"
                                                  )
                                                   i
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
                                                        r 0
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
                                                                      < r (
                                                                        hash-table-ref ws "height"
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! rows (
                                                                          append rows (
                                                                            _list r
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        set! r (
                                                                          + r 1
                                                                        )
                                                                      )
                                                                       (
                                                                        loop24
                                                                      )
                                                                    )
                                                                     '(
                                                                      
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
                                                                c 0
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
                                                                              < c (
                                                                                hash-table-ref ws "width"
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! cols (
                                                                                  append cols (
                                                                                    _list c
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! c (
                                                                                  + c 1
                                                                                )
                                                                              )
                                                                               (
                                                                                loop26
                                                                              )
                                                                            )
                                                                             '(
                                                                              
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
                                                                set! rows (
                                                                  shuffle rows
                                                                )
                                                              )
                                                               (
                                                                set! cols (
                                                                  shuffle cols
                                                                )
                                                              )
                                                               (
                                                                let (
                                                                  (
                                                                    d (
                                                                      rand_range 8
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    insert_dir ws word (
                                                                      list-ref-safe dirs_r d
                                                                    )
                                                                     (
                                                                      list-ref-safe dirs_c d
                                                                    )
                                                                     rows cols
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
                                            )
                                          )
                                           (
                                            loop22
                                          )
                                        )
                                         '(
                                          
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
            visualise ws add_fake_chars
          )
           (
            call/cc (
              lambda (
                ret28
              )
               (
                let (
                  (
                    result ""
                  )
                )
                 (
                  begin (
                    let (
                      (
                        r 0
                      )
                    )
                     (
                      begin (
                        call/cc (
                          lambda (
                            break30
                          )
                           (
                            letrec (
                              (
                                loop29 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < r (
                                        hash-table-ref ws "height"
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
                                                break32
                                              )
                                               (
                                                letrec (
                                                  (
                                                    loop31 (
                                                      lambda (
                                                        
                                                      )
                                                       (
                                                        if (
                                                          < c (
                                                            hash-table-ref ws "width"
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                ch (
                                                                  cond (
                                                                    (
                                                                      string? (
                                                                        list-ref-safe (
                                                                          hash-table-ref ws "board"
                                                                        )
                                                                         r
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref-safe (
                                                                          hash-table-ref ws "board"
                                                                        )
                                                                         r
                                                                      )
                                                                       c (
                                                                        + c 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        list-ref-safe (
                                                                          hash-table-ref ws "board"
                                                                        )
                                                                         r
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref-safe (
                                                                          hash-table-ref ws "board"
                                                                        )
                                                                         r
                                                                      )
                                                                       c
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe (
                                                                        list-ref-safe (
                                                                          hash-table-ref ws "board"
                                                                        )
                                                                         r
                                                                      )
                                                                       c
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  equal? ch ""
                                                                )
                                                                 (
                                                                  begin (
                                                                    if add_fake_chars (
                                                                      begin (
                                                                        set! ch (
                                                                          rand_letter
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! ch "#"
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 '(
                                                                  
                                                                )
                                                              )
                                                               (
                                                                set! result (
                                                                  string-append (
                                                                    string-append result ch
                                                                  )
                                                                   " "
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
                                                            loop31
                                                          )
                                                        )
                                                         '(
                                                          
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  loop31
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! result (
                                              string-append result "\n"
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
                                        loop29
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop29
                            )
                          )
                        )
                      )
                       (
                        ret28 result
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
                ret33
              )
               (
                let (
                  (
                    words (
                      _list "cat" "dog" "snake" "fish"
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        ws (
                          make_word_search words 10 10
                        )
                      )
                    )
                     (
                      begin (
                        generate_board ws
                      )
                       (
                        _display (
                          if (
                            string? (
                              visualise ws #t
                            )
                          )
                           (
                            visualise ws #t
                          )
                           (
                            to-str (
                              visualise ws #t
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
      )
    )
     (
      let (
        (
          end35 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur36 (
              quotient (
                * (
                  - end35 start34
                )
                 1000000
              )
               jps37
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur36
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
