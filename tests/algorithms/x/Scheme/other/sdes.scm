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
      start20 (
        current-jiffy
      )
    )
     (
      jps23 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        apply_table inp table
      )
       (
        call/cc (
          lambda (
            ret1
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
                                    _len table
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        idx (
                                          - (
                                            list-ref-safe table i
                                          )
                                           1
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          < idx 0
                                        )
                                         (
                                          begin (
                                            set! idx (
                                              - (
                                                _len inp
                                              )
                                               1
                                            )
                                          )
                                        )
                                         '(
                                          
                                        )
                                      )
                                       (
                                        set! res (
                                          string-append res (
                                            _substring inp idx (
                                              + idx 1
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
                                    loop2
                                  )
                                )
                                 '(
                                  
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
                    ret1 res
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
        left_shift data
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            ret4 (
              string-append (
                _substring data 1 (
                  _len data
                )
              )
               (
                _substring data 0 1
              )
            )
          )
        )
      )
    )
     (
      define (
        xor a b
      )
       (
        call/cc (
          lambda (
            ret5
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
                                  and (
                                    < i (
                                      _len a
                                    )
                                  )
                                   (
                                    < i (
                                      _len b
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? (
                                        _substring a i (
                                          + i 1
                                        )
                                      )
                                       (
                                        _substring b i (
                                          + i 1
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          string-append res "0"
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          string-append res "1"
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
                                    loop6
                                  )
                                )
                                 '(
                                  
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
                    ret5 res
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
        int_to_binary n
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            begin (
              if (
                equal? n 0
              )
               (
                begin (
                  ret8 "0"
                )
              )
               '(
                
              )
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
                      num n
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
                                    > num 0
                                  )
                                   (
                                    begin (
                                      set! res (
                                        string-append (
                                          to-str-space (
                                            _mod num 2
                                          )
                                        )
                                         res
                                      )
                                    )
                                     (
                                      set! num (
                                        _div num 2
                                      )
                                    )
                                     (
                                      loop9
                                    )
                                  )
                                   '(
                                    
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
                      ret8 res
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
        pad_left s width
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            let (
              (
                res s
              )
            )
             (
              begin (
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
                              < (
                                _len res
                              )
                               width
                            )
                             (
                              begin (
                                set! res (
                                  string-append "0" res
                                )
                              )
                               (
                                loop12
                              )
                            )
                             '(
                              
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
                ret11 res
              )
            )
          )
        )
      )
    )
     (
      define (
        bin_to_int s
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            let (
              (
                result 0
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
                                  < i (
                                    _len s
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        digit (
                                          let (
                                            (
                                              v17 (
                                                _substring s i (
                                                  + i 1
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
                                                inexact->exact (
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
                                                inexact->exact (
                                                  floor v17
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! result (
                                          + (
                                            * result 2
                                          )
                                           digit
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
                    ret14 result
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
        apply_sbox s data
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            let (
              (
                row_bits (
                  string-append (
                    _substring data 0 1
                  )
                   (
                    _substring data (
                      - (
                        _len data
                      )
                       1
                    )
                     (
                      _len data
                    )
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    col_bits (
                      _substring data 1 3
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        row (
                          bin_to_int row_bits
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            col (
                              bin_to_int col_bits
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                val (
                                  cond (
                                    (
                                      string? (
                                        list-ref-safe s row
                                      )
                                    )
                                     (
                                      _substring (
                                        list-ref-safe s row
                                      )
                                       col (
                                        + col 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        list-ref-safe s row
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        list-ref-safe s row
                                      )
                                       col
                                    )
                                  )
                                   (
                                    else (
                                      list-ref-safe (
                                        list-ref-safe s row
                                      )
                                       col
                                    )
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    out (
                                      int_to_binary val
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    ret18 out
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
          p4_table (
            _list 2 4 3 1
          )
        )
      )
       (
        begin (
          define (
            f expansion s0 s1 key message
          )
           (
            call/cc (
              lambda (
                ret19
              )
               (
                let (
                  (
                    left (
                      _substring message 0 4
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        right (
                          _substring message 4 8
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            temp (
                              apply_table right expansion
                            )
                          )
                        )
                         (
                          begin (
                            set! temp (
                              xor temp key
                            )
                          )
                           (
                            let (
                              (
                                left_bin_str (
                                  apply_sbox s0 (
                                    if (
                                      string? temp
                                    )
                                     (
                                      _substring temp 0 4
                                    )
                                     (
                                      take (
                                        drop temp 0
                                      )
                                       (
                                        - 4 0
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    right_bin_str (
                                      apply_sbox s1 (
                                        if (
                                          string? temp
                                        )
                                         (
                                          _substring temp 4 8
                                        )
                                         (
                                          take (
                                            drop temp 4
                                          )
                                           (
                                            - 8 4
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! left_bin_str (
                                      pad_left left_bin_str 2
                                    )
                                  )
                                   (
                                    set! right_bin_str (
                                      pad_left right_bin_str 2
                                    )
                                  )
                                   (
                                    set! temp (
                                      apply_table (
                                        _add left_bin_str right_bin_str
                                      )
                                       p4_table
                                    )
                                  )
                                   (
                                    set! temp (
                                      xor left temp
                                    )
                                  )
                                   (
                                    ret19 (
                                      string-append temp right
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
              key "1010000010"
            )
          )
           (
            begin (
              let (
                (
                  message "11010111"
                )
              )
               (
                begin (
                  let (
                    (
                      p8_table (
                        _list 6 3 7 4 8 5 10 9
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          p10_table (
                            _list 3 5 2 7 4 10 1 9 8 6
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              IP (
                                _list 2 6 3 1 4 8 5 7
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  IP_inv (
                                    _list 4 1 3 5 7 2 8 6
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      expansion (
                                        _list 4 1 2 3 2 3 4 1
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          s0 (
                                            _list (
                                              _list 1 0 3 2
                                            )
                                             (
                                              _list 3 2 1 0
                                            )
                                             (
                                              _list 0 2 1 3
                                            )
                                             (
                                              _list 3 1 3 2
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              s1 (
                                                _list (
                                                  _list 0 1 2 3
                                                )
                                                 (
                                                  _list 2 0 1 3
                                                )
                                                 (
                                                  _list 3 0 1 0
                                                )
                                                 (
                                                  _list 2 1 0 3
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  temp (
                                                    apply_table key p10_table
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      left (
                                                        if (
                                                          string? temp
                                                        )
                                                         (
                                                          _substring temp 0 5
                                                        )
                                                         (
                                                          take (
                                                            drop temp 0
                                                          )
                                                           (
                                                            - 5 0
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          right (
                                                            if (
                                                              string? temp
                                                            )
                                                             (
                                                              _substring temp 5 10
                                                            )
                                                             (
                                                              take (
                                                                drop temp 5
                                                              )
                                                               (
                                                                - 10 5
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          set! left (
                                                            left_shift left
                                                          )
                                                        )
                                                         (
                                                          set! right (
                                                            left_shift right
                                                          )
                                                        )
                                                         (
                                                          let (
                                                            (
                                                              key1 (
                                                                apply_table (
                                                                  _add left right
                                                                )
                                                                 p8_table
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              set! left (
                                                                left_shift left
                                                              )
                                                            )
                                                             (
                                                              set! right (
                                                                left_shift right
                                                              )
                                                            )
                                                             (
                                                              set! left (
                                                                left_shift left
                                                              )
                                                            )
                                                             (
                                                              set! right (
                                                                left_shift right
                                                              )
                                                            )
                                                             (
                                                              let (
                                                                (
                                                                  key2 (
                                                                    apply_table (
                                                                      _add left right
                                                                    )
                                                                     p8_table
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  set! temp (
                                                                    apply_table message IP
                                                                  )
                                                                )
                                                                 (
                                                                  set! temp (
                                                                    f expansion s0 s1 key1 temp
                                                                  )
                                                                )
                                                                 (
                                                                  set! temp (
                                                                    _add (
                                                                      if (
                                                                        string? temp
                                                                      )
                                                                       (
                                                                        _substring temp 4 8
                                                                      )
                                                                       (
                                                                        take (
                                                                          drop temp 4
                                                                        )
                                                                         (
                                                                          - 8 4
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      if (
                                                                        string? temp
                                                                      )
                                                                       (
                                                                        _substring temp 0 4
                                                                      )
                                                                       (
                                                                        take (
                                                                          drop temp 0
                                                                        )
                                                                         (
                                                                          - 4 0
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  set! temp (
                                                                    f expansion s0 s1 key2 temp
                                                                  )
                                                                )
                                                                 (
                                                                  let (
                                                                    (
                                                                      CT (
                                                                        apply_table temp IP_inv
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      _display (
                                                                        if (
                                                                          string? (
                                                                            string-append "Cipher text is: " CT
                                                                          )
                                                                        )
                                                                         (
                                                                          string-append "Cipher text is: " CT
                                                                        )
                                                                         (
                                                                          to-str (
                                                                            string-append "Cipher text is: " CT
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      newline
                                                                    )
                                                                     (
                                                                      set! temp (
                                                                        apply_table CT IP
                                                                      )
                                                                    )
                                                                     (
                                                                      set! temp (
                                                                        f expansion s0 s1 key2 temp
                                                                      )
                                                                    )
                                                                     (
                                                                      set! temp (
                                                                        _add (
                                                                          if (
                                                                            string? temp
                                                                          )
                                                                           (
                                                                            _substring temp 4 8
                                                                          )
                                                                           (
                                                                            take (
                                                                              drop temp 4
                                                                            )
                                                                             (
                                                                              - 8 4
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          if (
                                                                            string? temp
                                                                          )
                                                                           (
                                                                            _substring temp 0 4
                                                                          )
                                                                           (
                                                                            take (
                                                                              drop temp 0
                                                                            )
                                                                             (
                                                                              - 4 0
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      set! temp (
                                                                        f expansion s0 s1 key1 temp
                                                                      )
                                                                    )
                                                                     (
                                                                      let (
                                                                        (
                                                                          PT (
                                                                            apply_table temp IP_inv
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          _display (
                                                                            if (
                                                                              string? (
                                                                                string-append "Plain text after decypting is: " PT
                                                                              )
                                                                            )
                                                                             (
                                                                              string-append "Plain text after decypting is: " PT
                                                                            )
                                                                             (
                                                                              to-str (
                                                                                string-append "Plain text after decypting is: " PT
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
    )
     (
      let (
        (
          end21 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur22 (
              quotient (
                * (
                  - end21 start20
                )
                 1000000
              )
               jps23
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur22
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
