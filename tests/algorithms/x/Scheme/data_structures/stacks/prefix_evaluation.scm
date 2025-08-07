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
      start22 (
        current-jiffy
      )
    )
     (
      jps25 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        split s sep
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
                        set! res (
                          append res (
                            _list current
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
      )
    )
     (
      define (
        tokenize s
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                parts (
                  split s " "
                )
              )
            )
             (
              begin (
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
                                        _len parts
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            p (
                                              cond (
                                                (
                                                  string? parts
                                                )
                                                 (
                                                  _substring parts i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? parts
                                                )
                                                 (
                                                  hash-table-ref parts i
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref parts i
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              not (
                                                string=? p ""
                                              )
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  append res (
                                                    _list p
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
                                            set! i (
                                              + i 1
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
     (
      define (
        is_digit ch
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            ret7 (
              and (
                string>=? ch "0"
              )
               (
                string<=? ch "9"
              )
            )
          )
        )
      )
    )
     (
      define (
        is_operand token
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            begin (
              if (
                string=? token ""
              )
               (
                begin (
                  ret8 #f
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
                  i 0
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
                                < i (
                                  _len token
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      ch (
                                        _substring token i (
                                          + i 1
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      if (
                                        not (
                                          is_digit ch
                                        )
                                      )
                                       (
                                        begin (
                                          ret8 #f
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
                  ret8 #t
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
            ret11
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
                                            v14 (
                                              _substring token i (
                                                + i 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          cond (
                                            (
                                              string? v14
                                            )
                                             (
                                              inexact->exact (
                                                floor (
                                                  string->number v14
                                                )
                                              )
                                            )
                                          )
                                           (
                                            (
                                              boolean? v14
                                            )
                                             (
                                              if v14 1 0
                                            )
                                          )
                                           (
                                            else (
                                              inexact->exact (
                                                floor v14
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
                    ret11 res
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
        apply_op op a b
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
              ret15 0.0
            )
          )
        )
      )
    )
     (
      define (
        evaluate expression
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            let (
              (
                tokens (
                  tokenize expression
                )
              )
            )
             (
              begin (
                let (
                  (
                    stack (
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
                            _len tokens
                          )
                           1
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
                                    
                                  )
                                   (
                                    if (
                                      >= i 0
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            token (
                                              cond (
                                                (
                                                  string? tokens
                                                )
                                                 (
                                                  _substring tokens i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? tokens
                                                )
                                                 (
                                                  hash-table-ref tokens i
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref tokens i
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              not (
                                                string=? token ""
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  is_operand token
                                                )
                                                 (
                                                  begin (
                                                    set! stack (
                                                      append stack (
                                                        _list (
                                                          + 0.0 (
                                                            to_int token
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
                                                        o1 (
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
                                                        let (
                                                          (
                                                            o2 (
                                                              list-ref stack (
                                                                - (
                                                                  _len stack
                                                                )
                                                                 2
                                                              )
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
                                                                - (
                                                                  - (
                                                                    _len stack
                                                                  )
                                                                   2
                                                                )
                                                                 0
                                                              )
                                                            )
                                                          )
                                                           (
                                                            let (
                                                              (
                                                                res (
                                                                  apply_op token o1 o2
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! stack (
                                                                  append stack (
                                                                    _list res
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
                                              quote (
                                                
                                              )
                                            )
                                          )
                                           (
                                            set! i (
                                              - i 1
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop17
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
                              loop17
                            )
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
        )
      )
    )
     (
      define (
        eval_rec tokens pos
      )
       (
        call/cc (
          lambda (
            ret19
          )
           (
            let (
              (
                token (
                  list-ref tokens pos
                )
              )
            )
             (
              begin (
                let (
                  (
                    next (
                      + pos 1
                    )
                  )
                )
                 (
                  begin (
                    if (
                      is_operand token
                    )
                     (
                      begin (
                        ret19 (
                          _list (
                            + 0.0 (
                              to_int token
                            )
                          )
                           (
                            + 0.0 next
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
                        left (
                          eval_rec tokens next
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            a (
                              cond (
                                (
                                  string? left
                                )
                                 (
                                  _substring left 0 (
                                    + 0 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? left
                                )
                                 (
                                  hash-table-ref left 0
                                )
                              )
                               (
                                else (
                                  list-ref left 0
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                p1 (
                                  let (
                                    (
                                      v20 (
                                        cond (
                                          (
                                            string? left
                                          )
                                           (
                                            _substring left 1 (
                                              + 1 1
                                            )
                                          )
                                        )
                                         (
                                          (
                                            hash-table? left
                                          )
                                           (
                                            hash-table-ref left 1
                                          )
                                        )
                                         (
                                          else (
                                            list-ref left 1
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    cond (
                                      (
                                        string? v20
                                      )
                                       (
                                        inexact->exact (
                                          floor (
                                            string->number v20
                                          )
                                        )
                                      )
                                    )
                                     (
                                      (
                                        boolean? v20
                                      )
                                       (
                                        if v20 1 0
                                      )
                                    )
                                     (
                                      else (
                                        inexact->exact (
                                          floor v20
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
                                    right (
                                      eval_rec tokens p1
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        b (
                                          cond (
                                            (
                                              string? right
                                            )
                                             (
                                              _substring right 0 (
                                                + 0 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? right
                                            )
                                             (
                                              hash-table-ref right 0
                                            )
                                          )
                                           (
                                            else (
                                              list-ref right 0
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            p2 (
                                              cond (
                                                (
                                                  string? right
                                                )
                                                 (
                                                  _substring right 1 (
                                                    + 1 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? right
                                                )
                                                 (
                                                  hash-table-ref right 1
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref right 1
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            ret19 (
                                              _list (
                                                apply_op token a b
                                              )
                                               p2
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
        evaluate_recursive expression
      )
       (
        call/cc (
          lambda (
            ret21
          )
           (
            let (
              (
                tokens (
                  tokenize expression
                )
              )
            )
             (
              begin (
                let (
                  (
                    res (
                      eval_rec tokens 0
                    )
                  )
                )
                 (
                  begin (
                    ret21 (
                      cond (
                        (
                          string? res
                        )
                         (
                          _substring res 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? res
                        )
                         (
                          hash-table-ref res 0
                        )
                      )
                       (
                        else (
                          list-ref res 0
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
          test_expression "+ 9 * 2 6"
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                to-str-space (
                  evaluate test_expression
                )
              )
            )
             (
              to-str-space (
                evaluate test_expression
              )
            )
             (
              to-str (
                to-str-space (
                  evaluate test_expression
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
              test_expression2 "/ * 10 2 + 4 1 "
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    to-str-space (
                      evaluate test_expression2
                    )
                  )
                )
                 (
                  to-str-space (
                    evaluate test_expression2
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      evaluate test_expression2
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
                  test_expression3 "+ * 2 3 / 8 4"
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        to-str-space (
                          evaluate_recursive test_expression3
                        )
                      )
                    )
                     (
                      to-str-space (
                        evaluate_recursive test_expression3
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          evaluate_recursive test_expression3
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
      )
    )
     (
      let (
        (
          end23 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur24 (
              quotient (
                * (
                  - end23 start22
                )
                 1000000
              )
               jps25
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur24
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
