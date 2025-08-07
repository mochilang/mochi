;; Generated on 2025-08-07 11:54 +0700
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
(define (_div a b) (/ a b))
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
      define (
        abs_int n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                < n 0
              )
               (
                begin (
                  ret1 (
                    - n
                  )
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret1 n
            )
          )
        )
      )
    )
     (
      define (
        make_matrix row_size
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                size (
                  abs_int row_size
                )
              )
            )
             (
              begin (
                if (
                  equal? size 0
                )
                 (
                  begin (
                    set! size 4
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
                    mat (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        y 0
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
                                      _lt y size
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
                                                x 0
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
                                                              _lt x size
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list (
                                                                      _add (
                                                                        + 1 x
                                                                      )
                                                                       (
                                                                        * y size
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! x (
                                                                  + x 1
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
                                                set! mat (
                                                  append mat (
                                                    _list row
                                                  )
                                                )
                                              )
                                               (
                                                set! y (
                                                  + y 1
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop3
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
                              loop3
                            )
                          )
                        )
                      )
                       (
                        ret2 mat
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
        transpose mat
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                n (
                  _len mat
                )
              )
            )
             (
              begin (
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
                                      < i n
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
                                                              < j n
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            list-ref mat j
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref mat j
                                                                          )
                                                                           i (
                                                                            + i 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref mat j
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref mat j
                                                                          )
                                                                           i
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            list-ref mat j
                                                                          )
                                                                           i
                                                                        )
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
                                                set! result (
                                                  append result (
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
        reverse_row mat
      )
       (
        call/cc (
          lambda (
            ret12
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
                    i (
                      - (
                        _len mat
                      )
                       1
                    )
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
                                  >= i 0
                                )
                                 (
                                  begin (
                                    set! result (
                                      append result (
                                        _list (
                                          list-ref mat i
                                        )
                                      )
                                    )
                                  )
                                   (
                                    set! i (
                                      - i 1
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
                    ret12 result
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
        reverse_column mat
      )
       (
        call/cc (
          lambda (
            ret15
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
                                    _len mat
                                  )
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
                                            j (
                                              - (
                                                _len (
                                                  list-ref mat i
                                                )
                                              )
                                               1
                                            )
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
                                                          >= j 0
                                                        )
                                                         (
                                                          begin (
                                                            set! row (
                                                              append row (
                                                                _list (
                                                                  cond (
                                                                    (
                                                                      string? (
                                                                        list-ref mat i
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref mat i
                                                                      )
                                                                       j (
                                                                        + j 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        list-ref mat i
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref mat i
                                                                      )
                                                                       j
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref (
                                                                        list-ref mat i
                                                                      )
                                                                       j
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! j (
                                                              - j 1
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
                                            set! result (
                                              append result (
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
                    ret15 result
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
        rotate_90 mat
      )
       (
        call/cc (
          lambda (
            ret20
          )
           (
            let (
              (
                t (
                  transpose mat
                )
              )
            )
             (
              begin (
                let (
                  (
                    rr (
                      reverse_row t
                    )
                  )
                )
                 (
                  begin (
                    ret20 rr
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
        rotate_180 mat
      )
       (
        call/cc (
          lambda (
            ret21
          )
           (
            let (
              (
                rc (
                  reverse_column mat
                )
              )
            )
             (
              begin (
                let (
                  (
                    rr (
                      reverse_row rc
                    )
                  )
                )
                 (
                  begin (
                    ret21 rr
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
        rotate_270 mat
      )
       (
        call/cc (
          lambda (
            ret22
          )
           (
            let (
              (
                t (
                  transpose mat
                )
              )
            )
             (
              begin (
                let (
                  (
                    rc (
                      reverse_column t
                    )
                  )
                )
                 (
                  begin (
                    ret22 rc
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
        row_to_string row
      )
       (
        call/cc (
          lambda (
            ret23
          )
           (
            let (
              (
                line ""
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
                                    _len row
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? i 0
                                    )
                                     (
                                      begin (
                                        set! line (
                                          to-str-space (
                                            list-ref row i
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! line (
                                          string-append (
                                            string-append line " "
                                          )
                                           (
                                            to-str-space (
                                              list-ref row i
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
                                    loop24
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
                          loop24
                        )
                      )
                    )
                  )
                   (
                    ret23 line
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
        print_matrix mat
      )
       (
        call/cc (
          lambda (
            ret26
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
                              < i (
                                _len mat
                              )
                            )
                             (
                              begin (
                                _display (
                                  if (
                                    string? (
                                      row_to_string (
                                        list-ref mat i
                                      )
                                    )
                                  )
                                   (
                                    row_to_string (
                                      list-ref mat i
                                    )
                                  )
                                   (
                                    to-str (
                                      row_to_string (
                                        list-ref mat i
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                newline
                              )
                               (
                                set! i (
                                  + i 1
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
            )
          )
        )
      )
    )
     (
      let (
        (
          mat (
            make_matrix 4
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? "\norigin:\n"
            )
             "\norigin:\n" (
              to-str "\norigin:\n"
            )
          )
        )
         (
          newline
        )
         (
          print_matrix mat
        )
         (
          _display (
            if (
              string? "\nrotate 90 counterclockwise:\n"
            )
             "\nrotate 90 counterclockwise:\n" (
              to-str "\nrotate 90 counterclockwise:\n"
            )
          )
        )
         (
          newline
        )
         (
          let (
            (
              r90 (
                rotate_90 mat
              )
            )
          )
           (
            begin (
              print_matrix r90
            )
             (
              set! mat (
                make_matrix 4
              )
            )
             (
              _display (
                if (
                  string? "\norigin:\n"
                )
                 "\norigin:\n" (
                  to-str "\norigin:\n"
                )
              )
            )
             (
              newline
            )
             (
              print_matrix mat
            )
             (
              _display (
                if (
                  string? "\nrotate 180:\n"
                )
                 "\nrotate 180:\n" (
                  to-str "\nrotate 180:\n"
                )
              )
            )
             (
              newline
            )
             (
              let (
                (
                  r180 (
                    rotate_180 mat
                  )
                )
              )
               (
                begin (
                  print_matrix r180
                )
                 (
                  set! mat (
                    make_matrix 4
                  )
                )
                 (
                  _display (
                    if (
                      string? "\norigin:\n"
                    )
                     "\norigin:\n" (
                      to-str "\norigin:\n"
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  print_matrix mat
                )
                 (
                  _display (
                    if (
                      string? "\nrotate 270 counterclockwise:\n"
                    )
                     "\nrotate 270 counterclockwise:\n" (
                      to-str "\nrotate 270 counterclockwise:\n"
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  let (
                    (
                      r270 (
                        rotate_270 mat
                      )
                    )
                  )
                   (
                    begin (
                      print_matrix r270
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
