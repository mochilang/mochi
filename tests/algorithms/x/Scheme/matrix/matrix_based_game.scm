;; Generated on 2025-08-07 16:11 +0700
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
      start62 (
        current-jiffy
      )
    )
     (
      jps65 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        is_alnum ch
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              or (
                or (
                  and (
                    string>=? ch "0"
                  )
                   (
                    string<=? ch "9"
                  )
                )
                 (
                  and (
                    string>=? ch "A"
                  )
                   (
                    string<=? ch "Z"
                  )
                )
              )
               (
                and (
                  string>=? ch "a"
                )
                 (
                  string<=? ch "z"
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        to_int token
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                res 0
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
                        break4
                      )
                       (
                        letrec (
                          (
                            loop3 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len token
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      + (
                                        * res 10
                                      )
                                       (
                                        let (
                                          (
                                            v5 (
                                              _substring token i (
                                                + i 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          cond (
                                            (
                                              string? v5
                                            )
                                             (
                                              inexact->exact (
                                                floor (
                                                  string->number v5
                                                )
                                              )
                                            )
                                          )
                                           (
                                            (
                                              boolean? v5
                                            )
                                             (
                                              if v5 1 0
                                            )
                                          )
                                           (
                                            else (
                                              inexact->exact (
                                                floor v5
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
                                   (
                                    loop3
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop3
                        )
                      )
                    )
                  )
                   (
                    ret2 res
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
        split s sep
      )
       (
        call/cc (
          lambda (
            ret6
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
                    current ""
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
                                      < i (
                                        _len s
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            ch (
                                              _substring s i (
                                                + i 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              string=? ch sep
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  append res (
                                                    _list current
                                                  )
                                                )
                                              )
                                               (
                                                set! current ""
                                              )
                                            )
                                             (
                                              begin (
                                                set! current (
                                                  string-append current ch
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
                                        loop7
                                      )
                                    )
                                     '(
                                      
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
                            _list current
                          )
                        )
                      )
                       (
                        ret6 res
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
        parse_moves input_str
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                pairs (
                  split input_str ","
                )
              )
            )
             (
              begin (
                let (
                  (
                    moves (
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
                                      < i (
                                        _len pairs
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            pair (
                                              cond (
                                                (
                                                  string? pairs
                                                )
                                                 (
                                                  _substring pairs i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? pairs
                                                )
                                                 (
                                                  hash-table-ref pairs i
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref-safe pairs i
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                numbers (
                                                  _list
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    num ""
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
                                                                      < j (
                                                                        _len pair
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            ch (
                                                                              _substring pair j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            if (
                                                                              string=? ch " "
                                                                            )
                                                                             (
                                                                              begin (
                                                                                if (
                                                                                  not (
                                                                                    string=? num ""
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! numbers (
                                                                                      append numbers (
                                                                                        _list num
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! num ""
                                                                                  )
                                                                                )
                                                                                 '(
                                                                                  
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! num (
                                                                                  string-append num ch
                                                                                )
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
                                                        if (
                                                          not (
                                                            string=? num ""
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! numbers (
                                                              append numbers (
                                                                _list num
                                                              )
                                                            )
                                                          )
                                                        )
                                                         '(
                                                          
                                                        )
                                                      )
                                                       (
                                                        if (
                                                          not (
                                                            equal? (
                                                              _len numbers
                                                            )
                                                             2
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            panic "Each move must have exactly two numbers."
                                                          )
                                                        )
                                                         '(
                                                          
                                                        )
                                                      )
                                                       (
                                                        let (
                                                          (
                                                            x (
                                                              to_int (
                                                                list-ref-safe numbers 0
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                y (
                                                                  to_int (
                                                                    list-ref-safe numbers 1
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! moves (
                                                                  append moves (
                                                                    _list (
                                                                      alist->hash-table (
                                                                        _list (
                                                                          cons "x" x
                                                                        )
                                                                         (
                                                                          cons "y" y
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
                        ret9 moves
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
        validate_matrix_size size
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            if (
              <= size 0
            )
             (
              begin (
                panic "Matrix size must be a positive integer."
              )
            )
             '(
              
            )
          )
        )
      )
    )
     (
      define (
        validate_matrix_content matrix size
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len matrix
                  )
                   size
                )
              )
               (
                begin (
                  panic "The matrix dont match with size."
                )
              )
               '(
                
              )
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
                                < i size
                              )
                               (
                                begin (
                                  let (
                                    (
                                      row (
                                        list-ref-safe matrix i
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      if (
                                        not (
                                          equal? (
                                            _len row
                                          )
                                           size
                                        )
                                      )
                                       (
                                        begin (
                                          panic (
                                            string-append (
                                              string-append "Each row in the matrix must have exactly " (
                                                to-str-space size
                                              )
                                            )
                                             " characters."
                                          )
                                        )
                                      )
                                       '(
                                        
                                      )
                                    )
                                     (
                                      let (
                                        (
                                          j 0
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
                                                        < j size
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              ch (
                                                                _substring row j (
                                                                  + j 1
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              if (
                                                                not (
                                                                  is_alnum ch
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  panic "Matrix rows can only contain letters and numbers."
                                                                )
                                                              )
                                                               '(
                                                                
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
                                                          loop18
                                                        )
                                                      )
                                                       '(
                                                        
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
                                          set! i (
                                            + i 1
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
                               '(
                                
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
              )
            )
          )
        )
      )
    )
     (
      define (
        validate_moves moves size
      )
       (
        call/cc (
          lambda (
            ret20
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
                              < i (
                                _len moves
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    mv (
                                      list-ref-safe moves i
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      or (
                                        or (
                                          or (
                                            < (
                                              hash-table-ref mv "x"
                                            )
                                             0
                                          )
                                           (
                                            >= (
                                              hash-table-ref mv "x"
                                            )
                                             size
                                          )
                                        )
                                         (
                                          < (
                                            hash-table-ref mv "y"
                                          )
                                           0
                                        )
                                      )
                                       (
                                        >= (
                                          hash-table-ref mv "y"
                                        )
                                         size
                                      )
                                    )
                                     (
                                      begin (
                                        panic "Move is out of bounds for a matrix."
                                      )
                                    )
                                     '(
                                      
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
                                loop21
                              )
                            )
                             '(
                              
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
            )
          )
        )
      )
    )
     (
      define (
        contains pos r c
      )
       (
        call/cc (
          lambda (
            ret23
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
                              < i (
                                _len pos
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    p (
                                      list-ref-safe pos i
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      and (
                                        equal? (
                                          hash-table-ref p "x"
                                        )
                                         r
                                      )
                                       (
                                        equal? (
                                          hash-table-ref p "y"
                                        )
                                         c
                                      )
                                    )
                                     (
                                      begin (
                                        ret23 #t
                                      )
                                    )
                                     '(
                                      
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
                ret23 #f
              )
            )
          )
        )
      )
    )
     (
      define (
        find_repeat matrix_g row column size
      )
       (
        call/cc (
          lambda (
            ret26
          )
           (
            begin (
              set! column (
                - (
                  - size 1
                )
                 column
              )
            )
             (
              let (
                (
                  visited (
                    _list
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      repeated (
                        _list
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          color (
                            cond (
                              (
                                string? (
                                  list-ref-safe matrix_g column
                                )
                              )
                               (
                                _substring (
                                  list-ref-safe matrix_g column
                                )
                                 row (
                                  + row 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? (
                                  list-ref-safe matrix_g column
                                )
                              )
                               (
                                hash-table-ref (
                                  list-ref-safe matrix_g column
                                )
                                 row
                              )
                            )
                             (
                              else (
                                list-ref-safe (
                                  list-ref-safe matrix_g column
                                )
                                 row
                              )
                            )
                          )
                        )
                      )
                       (
                        begin (
                          if (
                            string=? color "-"
                          )
                           (
                            begin (
                              ret26 repeated
                            )
                          )
                           '(
                            
                          )
                        )
                         (
                          let (
                            (
                              stack (
                                _list (
                                  alist->hash-table (
                                    _list (
                                      cons "x" column
                                    )
                                     (
                                      cons "y" row
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
                                            > (
                                              _len stack
                                            )
                                             0
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  idx (
                                                    - (
                                                      _len stack
                                                    )
                                                     1
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      pos (
                                                        list-ref-safe stack idx
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! stack (
                                                        take (
                                                          drop stack 0
                                                        )
                                                         (
                                                          - idx 0
                                                        )
                                                      )
                                                    )
                                                     (
                                                      if (
                                                        or (
                                                          or (
                                                            or (
                                                              < (
                                                                hash-table-ref pos "x"
                                                              )
                                                               0
                                                            )
                                                             (
                                                              >= (
                                                                hash-table-ref pos "x"
                                                              )
                                                               size
                                                            )
                                                          )
                                                           (
                                                            < (
                                                              hash-table-ref pos "y"
                                                            )
                                                             0
                                                          )
                                                        )
                                                         (
                                                          >= (
                                                            hash-table-ref pos "y"
                                                          )
                                                           size
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          loop27
                                                        )
                                                      )
                                                       '(
                                                        
                                                      )
                                                    )
                                                     (
                                                      if (
                                                        contains visited (
                                                          hash-table-ref pos "x"
                                                        )
                                                         (
                                                          hash-table-ref pos "y"
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          loop27
                                                        )
                                                      )
                                                       '(
                                                        
                                                      )
                                                    )
                                                     (
                                                      set! visited (
                                                        append visited (
                                                          _list pos
                                                        )
                                                      )
                                                    )
                                                     (
                                                      if (
                                                        string=? (
                                                          cond (
                                                            (
                                                              string? (
                                                                list-ref-safe matrix_g (
                                                                  hash-table-ref pos "x"
                                                                )
                                                              )
                                                            )
                                                             (
                                                              _substring (
                                                                list-ref-safe matrix_g (
                                                                  hash-table-ref pos "x"
                                                                )
                                                              )
                                                               (
                                                                hash-table-ref pos "y"
                                                              )
                                                               (
                                                                + (
                                                                  hash-table-ref pos "y"
                                                                )
                                                                 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? (
                                                                list-ref-safe matrix_g (
                                                                  hash-table-ref pos "x"
                                                                )
                                                              )
                                                            )
                                                             (
                                                              hash-table-ref (
                                                                list-ref-safe matrix_g (
                                                                  hash-table-ref pos "x"
                                                                )
                                                              )
                                                               (
                                                                hash-table-ref pos "y"
                                                              )
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref-safe (
                                                                list-ref-safe matrix_g (
                                                                  hash-table-ref pos "x"
                                                                )
                                                              )
                                                               (
                                                                hash-table-ref pos "y"
                                                              )
                                                            )
                                                          )
                                                        )
                                                         color
                                                      )
                                                       (
                                                        begin (
                                                          set! repeated (
                                                            append repeated (
                                                              _list pos
                                                            )
                                                          )
                                                        )
                                                         (
                                                          set! stack (
                                                            append stack (
                                                              _list (
                                                                alist->hash-table (
                                                                  _list (
                                                                    cons "x" (
                                                                      - (
                                                                        hash-table-ref pos "x"
                                                                      )
                                                                       1
                                                                    )
                                                                  )
                                                                   (
                                                                    cons "y" (
                                                                      hash-table-ref pos "y"
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          set! stack (
                                                            append stack (
                                                              _list (
                                                                alist->hash-table (
                                                                  _list (
                                                                    cons "x" (
                                                                      + (
                                                                        hash-table-ref pos "x"
                                                                      )
                                                                       1
                                                                    )
                                                                  )
                                                                   (
                                                                    cons "y" (
                                                                      hash-table-ref pos "y"
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          set! stack (
                                                            append stack (
                                                              _list (
                                                                alist->hash-table (
                                                                  _list (
                                                                    cons "x" (
                                                                      hash-table-ref pos "x"
                                                                    )
                                                                  )
                                                                   (
                                                                    cons "y" (
                                                                      - (
                                                                        hash-table-ref pos "y"
                                                                      )
                                                                       1
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          set! stack (
                                                            append stack (
                                                              _list (
                                                                alist->hash-table (
                                                                  _list (
                                                                    cons "x" (
                                                                      hash-table-ref pos "x"
                                                                    )
                                                                  )
                                                                   (
                                                                    cons "y" (
                                                                      + (
                                                                        hash-table-ref pos "y"
                                                                      )
                                                                       1
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       '(
                                                        
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
                                           '(
                                            
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
                              ret26 repeated
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
        increment_score count
      )
       (
        call/cc (
          lambda (
            ret29
          )
           (
            ret29 (
              _div (
                * count (
                  + count 1
                )
              )
               2
            )
          )
        )
      )
    )
     (
      define (
        move_x matrix_g column size
      )
       (
        call/cc (
          lambda (
            ret30
          )
           (
            let (
              (
                new_list (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    row 0
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
                                  < row size
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        val (
                                          cond (
                                            (
                                              string? (
                                                list-ref-safe matrix_g row
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref-safe matrix_g row
                                              )
                                               column (
                                                + column 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref-safe matrix_g row
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref-safe matrix_g row
                                              )
                                               column
                                            )
                                          )
                                           (
                                            else (
                                              list-ref-safe (
                                                list-ref-safe matrix_g row
                                              )
                                               column
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          not (
                                            string=? val "-"
                                          )
                                        )
                                         (
                                          begin (
                                            set! new_list (
                                              append new_list (
                                                _list val
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! new_list (
                                              append (
                                                _list val
                                              )
                                               new_list
                                            )
                                          )
                                        )
                                      )
                                       (
                                        set! row (
                                          + row 1
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
                    set! row 0
                  )
                   (
                    call/cc (
                      lambda (
                        break34
                      )
                       (
                        letrec (
                          (
                            loop33 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < row size
                                )
                                 (
                                  begin (
                                    list-set! (
                                      list-ref-safe matrix_g row
                                    )
                                     column (
                                      list-ref-safe new_list row
                                    )
                                  )
                                   (
                                    set! row (
                                      + row 1
                                    )
                                  )
                                   (
                                    loop33
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop33
                        )
                      )
                    )
                  )
                   (
                    ret30 matrix_g
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
        move_y matrix_g size
      )
       (
        call/cc (
          lambda (
            ret35
          )
           (
            let (
              (
                empty_cols (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    column (
                      - size 1
                    )
                  )
                )
                 (
                  begin (
                    call/cc (
                      lambda (
                        break37
                      )
                       (
                        letrec (
                          (
                            loop36 (
                              lambda (
                                
                              )
                               (
                                if (
                                  >= column 0
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        row 0
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            all_empty #t
                                          )
                                        )
                                         (
                                          begin (
                                            call/cc (
                                              lambda (
                                                break39
                                              )
                                               (
                                                letrec (
                                                  (
                                                    loop38 (
                                                      lambda (
                                                        
                                                      )
                                                       (
                                                        if (
                                                          < row size
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              not (
                                                                string=? (
                                                                  cond (
                                                                    (
                                                                      string? (
                                                                        list-ref-safe matrix_g row
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref-safe matrix_g row
                                                                      )
                                                                       column (
                                                                        + column 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        list-ref-safe matrix_g row
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref-safe matrix_g row
                                                                      )
                                                                       column
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe (
                                                                        list-ref-safe matrix_g row
                                                                      )
                                                                       column
                                                                    )
                                                                  )
                                                                )
                                                                 "-"
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! all_empty #f
                                                              )
                                                               (
                                                                break39 '(
                                                                  
                                                                )
                                                              )
                                                            )
                                                             '(
                                                              
                                                            )
                                                          )
                                                           (
                                                            set! row (
                                                              + row 1
                                                            )
                                                          )
                                                           (
                                                            loop38
                                                          )
                                                        )
                                                         '(
                                                          
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  loop38
                                                )
                                              )
                                            )
                                          )
                                           (
                                            if all_empty (
                                              begin (
                                                set! empty_cols (
                                                  append empty_cols (
                                                    _list column
                                                  )
                                                )
                                              )
                                            )
                                             '(
                                              
                                            )
                                          )
                                           (
                                            set! column (
                                              - column 1
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop36
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop36
                        )
                      )
                    )
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
                            break41
                          )
                           (
                            letrec (
                              (
                                loop40 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len empty_cols
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            col (
                                              list-ref-safe empty_cols i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                c (
                                                  + col 1
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                call/cc (
                                                  lambda (
                                                    break43
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop42 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              < c size
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
                                                                        break45
                                                                      )
                                                                       (
                                                                        letrec (
                                                                          (
                                                                            loop44 (
                                                                              lambda (
                                                                                
                                                                              )
                                                                               (
                                                                                if (
                                                                                  < r size
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    list-set! (
                                                                                      list-ref-safe matrix_g r
                                                                                    )
                                                                                     (
                                                                                      - c 1
                                                                                    )
                                                                                     (
                                                                                      cond (
                                                                                        (
                                                                                          string? (
                                                                                            list-ref-safe matrix_g r
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          _substring (
                                                                                            list-ref-safe matrix_g r
                                                                                          )
                                                                                           c (
                                                                                            + c 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? (
                                                                                            list-ref-safe matrix_g r
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref (
                                                                                            list-ref-safe matrix_g r
                                                                                          )
                                                                                           c
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref-safe (
                                                                                            list-ref-safe matrix_g r
                                                                                          )
                                                                                           c
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! r (
                                                                                      + r 1
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    loop44
                                                                                  )
                                                                                )
                                                                                 '(
                                                                                  
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          loop44
                                                                        )
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
                                                                loop42
                                                              )
                                                            )
                                                             '(
                                                              
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      loop42
                                                    )
                                                  )
                                                )
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
                                                        break47
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop46 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < r size
                                                                )
                                                                 (
                                                                  begin (
                                                                    list-set! (
                                                                      list-ref-safe matrix_g r
                                                                    )
                                                                     (
                                                                      - size 1
                                                                    )
                                                                     "-"
                                                                  )
                                                                   (
                                                                    set! r (
                                                                      + r 1
                                                                    )
                                                                  )
                                                                   (
                                                                    loop46
                                                                  )
                                                                )
                                                                 '(
                                                                  
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          loop46
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
                                        )
                                      )
                                       (
                                        loop40
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop40
                            )
                          )
                        )
                      )
                       (
                        ret35 matrix_g
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
        play matrix_g pos_x pos_y size
      )
       (
        call/cc (
          lambda (
            ret48
          )
           (
            let (
              (
                same_colors (
                  find_repeat matrix_g pos_x pos_y size
                )
              )
            )
             (
              begin (
                if (
                  not (
                    equal? (
                      _len same_colors
                    )
                     0
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
                            break50
                          )
                           (
                            letrec (
                              (
                                loop49 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len same_colors
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            p (
                                              cond (
                                                (
                                                  string? same_colors
                                                )
                                                 (
                                                  _substring same_colors i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? same_colors
                                                )
                                                 (
                                                  hash-table-ref same_colors i
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref-safe same_colors i
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            list-set! (
                                              list-ref-safe matrix_g (
                                                hash-table-ref p "x"
                                              )
                                            )
                                             (
                                              hash-table-ref p "y"
                                            )
                                             "-"
                                          )
                                           (
                                            set! i (
                                              + i 1
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop49
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop49
                            )
                          )
                        )
                      )
                       (
                        let (
                          (
                            column 0
                          )
                        )
                         (
                          begin (
                            call/cc (
                              lambda (
                                break52
                              )
                               (
                                letrec (
                                  (
                                    loop51 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < column size
                                        )
                                         (
                                          begin (
                                            set! matrix_g (
                                              move_x matrix_g column size
                                            )
                                          )
                                           (
                                            set! column (
                                              + column 1
                                            )
                                          )
                                           (
                                            loop51
                                          )
                                        )
                                         '(
                                          
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  loop51
                                )
                              )
                            )
                          )
                           (
                            set! matrix_g (
                              move_y matrix_g size
                            )
                          )
                        )
                      )
                    )
                  )
                )
                 '(
                  
                )
              )
               (
                let (
                  (
                    sc (
                      increment_score (
                        _len same_colors
                      )
                    )
                  )
                )
                 (
                  begin (
                    ret48 (
                      alist->hash-table (
                        _list (
                          cons "matrix" matrix_g
                        )
                         (
                          cons "score" sc
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
        build_matrix matrix
      )
       (
        call/cc (
          lambda (
            ret53
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
                    i 0
                  )
                )
                 (
                  begin (
                    call/cc (
                      lambda (
                        break55
                      )
                       (
                        letrec (
                          (
                            loop54 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len matrix
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        row (
                                          list-ref-safe matrix i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            row_list (
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
                                                    break57
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop56 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              < j (
                                                                _len row
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! row_list (
                                                                  append row_list (
                                                                    _list (
                                                                      _substring row j (
                                                                        + j 1
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
                                                                loop56
                                                              )
                                                            )
                                                             '(
                                                              
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      loop56
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! res (
                                                  append res (
                                                    _list row_list
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
                                   (
                                    loop54
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop54
                        )
                      )
                    )
                  )
                   (
                    ret53 res
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
        process_game size matrix moves
      )
       (
        call/cc (
          lambda (
            ret58
          )
           (
            let (
              (
                game_matrix (
                  build_matrix matrix
                )
              )
            )
             (
              begin (
                let (
                  (
                    total 0
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
                            break60
                          )
                           (
                            letrec (
                              (
                                loop59 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len moves
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            mv (
                                              list-ref-safe moves i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                res (
                                                  play game_matrix (
                                                    hash-table-ref mv "x"
                                                  )
                                                   (
                                                    hash-table-ref mv "y"
                                                  )
                                                   size
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! game_matrix (
                                                  hash-table-ref res "matrix"
                                                )
                                              )
                                               (
                                                set! total (
                                                  _add total (
                                                    hash-table-ref res "score"
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
                                        loop59
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop59
                            )
                          )
                        )
                      )
                       (
                        ret58 total
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
            ret61
          )
           (
            let (
              (
                size 4
              )
            )
             (
              begin (
                let (
                  (
                    matrix (
                      _list "RRBG" "RBBG" "YYGG" "XYGG"
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        moves (
                          parse_moves "0 1,1 1"
                        )
                      )
                    )
                     (
                      begin (
                        validate_matrix_size size
                      )
                       (
                        validate_matrix_content matrix size
                      )
                       (
                        validate_moves moves size
                      )
                       (
                        let (
                          (
                            score (
                              process_game size matrix moves
                            )
                          )
                        )
                         (
                          begin (
                            _display (
                              if (
                                string? (
                                  to-str-space score
                                )
                              )
                               (
                                to-str-space score
                              )
                               (
                                to-str (
                                  to-str-space score
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
     (
      main
    )
     (
      let (
        (
          end63 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur64 (
              quotient (
                * (
                  - end63 start62
                )
                 1000000
              )
               jps65
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur64
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
