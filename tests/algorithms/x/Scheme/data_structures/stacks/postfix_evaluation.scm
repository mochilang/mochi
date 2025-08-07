;; Generated on 2025-08-07 08:20 +0700
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
      start19 (
        current-jiffy
      )
    )
     (
      jps22 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        slice_without_last xs
      )
       (
        call/cc (
          lambda (
            ret1
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
                                    - (
                                      _len xs
                                    )
                                     1
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
        parse_float token
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                sign 1.0
              )
            )
             (
              begin (
                let (
                  (
                    idx 0
                  )
                )
                 (
                  begin (
                    if (
                      > (
                        _len token
                      )
                       0
                    )
                     (
                      begin (
                        let (
                          (
                            first (
                              _substring token 0 1
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              string=? first "-"
                            )
                             (
                              begin (
                                set! sign (
                                  - 1.0
                                )
                              )
                               (
                                set! idx 1
                              )
                            )
                             (
                              if (
                                string=? first "+"
                              )
                               (
                                begin (
                                  set! idx 1
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
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
                    let (
                      (
                        int_part 0
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
                                      and (
                                        < idx (
                                          _len token
                                        )
                                      )
                                       (
                                        not (
                                          string=? (
                                            _substring token idx (
                                              + idx 1
                                            )
                                          )
                                           "."
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! int_part (
                                          + (
                                            * int_part 10
                                          )
                                           (
                                            let (
                                              (
                                                v7 (
                                                  _substring token idx (
                                                    + idx 1
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              cond (
                                                (
                                                  string? v7
                                                )
                                                 (
                                                  exact (
                                                    floor (
                                                      string->number v7
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  boolean? v7
                                                )
                                                 (
                                                  if v7 1 0
                                                )
                                              )
                                               (
                                                else (
                                                  exact (
                                                    floor v7
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        set! idx (
                                          + idx 1
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
                        let (
                          (
                            result (
                              * 1.0 int_part
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              and (
                                < idx (
                                  _len token
                                )
                              )
                               (
                                string=? (
                                  _substring token idx (
                                    + idx 1
                                  )
                                )
                                 "."
                              )
                            )
                             (
                              begin (
                                set! idx (
                                  + idx 1
                                )
                              )
                               (
                                let (
                                  (
                                    place 0.1
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
                                                  < idx (
                                                    _len token
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        digit (
                                                          let (
                                                            (
                                                              v10 (
                                                                _substring token idx (
                                                                  + idx 1
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            cond (
                                                              (
                                                                string? v10
                                                              )
                                                               (
                                                                exact (
                                                                  floor (
                                                                    string->number v10
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                boolean? v10
                                                              )
                                                               (
                                                                if v10 1 0
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                exact (
                                                                  floor v10
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
                                                          _add result (
                                                            * place (
                                                              * 1.0 digit
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! place (
                                                          _div place 10.0
                                                        )
                                                      )
                                                       (
                                                        set! idx (
                                                          + idx 1
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
                                )
                              )
                            )
                             (
                              quote (
                                
                              )
                            )
                          )
                           (
                            ret4 (
                              * sign result
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
        pow_float base exp
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            let (
              (
                result 1.0
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
                    let (
                      (
                        e (
                          let (
                            (
                              v12 exp
                            )
                          )
                           (
                            cond (
                              (
                                string? v12
                              )
                               (
                                exact (
                                  floor (
                                    string->number v12
                                  )
                                )
                              )
                            )
                             (
                              (
                                boolean? v12
                              )
                               (
                                if v12 1 0
                              )
                            )
                             (
                              else (
                                exact (
                                  floor v12
                                )
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
                                      < i e
                                    )
                                     (
                                      begin (
                                        set! result (
                                          * result base
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
                        ret11 result
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
        apply_op a b op
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            begin (
              if (
                string=? op "+"
              )
               (
                begin (
                  ret15 (
                    + a b
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
                string=? op "-"
              )
               (
                begin (
                  ret15 (
                    - a b
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
                string=? op "*"
              )
               (
                begin (
                  ret15 (
                    * a b
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
                string=? op "/"
              )
               (
                begin (
                  ret15 (
                    _div a b
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
                string=? op "^"
              )
               (
                begin (
                  ret15 (
                    pow_float a b
                  )
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret15 0.0
            )
          )
        )
      )
    )
     (
      define (
        evaluate tokens
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            begin (
              if (
                equal? (
                  _len tokens
                )
                 0
              )
               (
                begin (
                  ret16 0.0
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
                  stack (
                    _list
                  )
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
                              xs
                            )
                             (
                              if (
                                null? xs
                              )
                               (
                                quote (
                                  
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      token (
                                        car xs
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      if (
                                        or (
                                          or (
                                            or (
                                              or (
                                                string=? token "+"
                                              )
                                               (
                                                string=? token "-"
                                              )
                                            )
                                             (
                                              string=? token "*"
                                            )
                                          )
                                           (
                                            string=? token "/"
                                          )
                                        )
                                         (
                                          string=? token "^"
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            and (
                                              or (
                                                string=? token "+"
                                              )
                                               (
                                                string=? token "-"
                                              )
                                            )
                                             (
                                              < (
                                                _len stack
                                              )
                                               2
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  b (
                                                    list-ref stack (
                                                      - (
                                                        _len stack
                                                      )
                                                       1
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  set! stack (
                                                    slice_without_last stack
                                                  )
                                                )
                                                 (
                                                  if (
                                                    string=? token "-"
                                                  )
                                                   (
                                                    begin (
                                                      set! stack (
                                                        append stack (
                                                          _list (
                                                            - 0.0 b
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! stack (
                                                        append stack (
                                                          _list b
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
                                              let (
                                                (
                                                  b (
                                                    list-ref stack (
                                                      - (
                                                        _len stack
                                                      )
                                                       1
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  set! stack (
                                                    slice_without_last stack
                                                  )
                                                )
                                                 (
                                                  let (
                                                    (
                                                      a (
                                                        list-ref stack (
                                                          - (
                                                            _len stack
                                                          )
                                                           1
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! stack (
                                                        slice_without_last stack
                                                      )
                                                    )
                                                     (
                                                      let (
                                                        (
                                                          result (
                                                            apply_op a b token
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          set! stack (
                                                            append stack (
                                                              _list result
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
                                        begin (
                                          set! stack (
                                            append stack (
                                              _list (
                                                parse_float token
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  loop17 (
                                    cdr xs
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                       (
                        loop17 tokens
                      )
                    )
                  )
                )
                 (
                  if (
                    not (
                      equal? (
                        _len stack
                      )
                       1
                    )
                  )
                   (
                    begin (
                      panic "Invalid postfix expression"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  ret16 (
                    list-ref stack 0
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
            to-str-space (
              evaluate (
                _list "2" "1" "+" "3" "*"
              )
            )
          )
        )
         (
          to-str-space (
            evaluate (
              _list "2" "1" "+" "3" "*"
            )
          )
        )
         (
          to-str (
            to-str-space (
              evaluate (
                _list "2" "1" "+" "3" "*"
              )
            )
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
            to-str-space (
              evaluate (
                _list "4" "13" "5" "/" "+"
              )
            )
          )
        )
         (
          to-str-space (
            evaluate (
              _list "4" "13" "5" "/" "+"
            )
          )
        )
         (
          to-str (
            to-str-space (
              evaluate (
                _list "4" "13" "5" "/" "+"
              )
            )
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
            to-str-space (
              evaluate (
                _list "5" "6" "9" "*" "+"
              )
            )
          )
        )
         (
          to-str-space (
            evaluate (
              _list "5" "6" "9" "*" "+"
            )
          )
        )
         (
          to-str (
            to-str-space (
              evaluate (
                _list "5" "6" "9" "*" "+"
              )
            )
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
            to-str-space (
              evaluate (
                _list "2" "-" "3" "+"
              )
            )
          )
        )
         (
          to-str-space (
            evaluate (
              _list "2" "-" "3" "+"
            )
          )
        )
         (
          to-str (
            to-str-space (
              evaluate (
                _list "2" "-" "3" "+"
              )
            )
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
            to-str-space (
              evaluate (
                _list
              )
            )
          )
        )
         (
          to-str-space (
            evaluate (
              _list
            )
          )
        )
         (
          to-str (
            to-str-space (
              evaluate (
                _list
              )
            )
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
          end20 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur21 (
              quotient (
                * (
                  - end20 start19
                )
                 1000000
              )
               jps22
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur21
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
