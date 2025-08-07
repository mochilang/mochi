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
      start21 (
        current-jiffy
      )
    )
     (
      jps24 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          PRECEDENCES (
            alist->hash-table (
              _list (
                cons "+" 1
              )
               (
                cons "-" 1
              )
               (
                cons "*" 2
              )
               (
                cons "/" 2
              )
               (
                cons "^" 3
              )
            )
          )
        )
      )
       (
        begin (
          let (
            (
              ASSOCIATIVITIES (
                alist->hash-table (
                  _list (
                    cons "+" "LR"
                  )
                   (
                    cons "-" "LR"
                  )
                   (
                    cons "*" "LR"
                  )
                   (
                    cons "/" "LR"
                  )
                   (
                    cons "^" "RL"
                  )
                )
              )
            )
          )
           (
            begin (
              define (
                precedence ch
              )
               (
                call/cc (
                  lambda (
                    ret1
                  )
                   (
                    begin (
                      if (
                        cond (
                          (
                            string? PRECEDENCES
                          )
                           (
                            if (
                              string-contains PRECEDENCES ch
                            )
                             #t #f
                          )
                        )
                         (
                          (
                            hash-table? PRECEDENCES
                          )
                           (
                            if (
                              hash-table-exists? PRECEDENCES ch
                            )
                             #t #f
                          )
                        )
                         (
                          else (
                            if (
                              member ch PRECEDENCES
                            )
                             #t #f
                          )
                        )
                      )
                       (
                        begin (
                          ret1 (
                            hash-table-ref/default PRECEDENCES ch (
                              quote (
                                
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
                      ret1 (
                        - 1
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                associativity ch
              )
               (
                call/cc (
                  lambda (
                    ret2
                  )
                   (
                    begin (
                      if (
                        cond (
                          (
                            string? ASSOCIATIVITIES
                          )
                           (
                            if (
                              string-contains ASSOCIATIVITIES ch
                            )
                             #t #f
                          )
                        )
                         (
                          (
                            hash-table? ASSOCIATIVITIES
                          )
                           (
                            if (
                              hash-table-exists? ASSOCIATIVITIES ch
                            )
                             #t #f
                          )
                        )
                         (
                          else (
                            if (
                              member ch ASSOCIATIVITIES
                            )
                             #t #f
                          )
                        )
                      )
                       (
                        begin (
                          ret2 (
                            hash-table-ref/default ASSOCIATIVITIES ch (
                              quote (
                                
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
                      ret2 ""
                    )
                  )
                )
              )
            )
             (
              define (
                balanced_parentheses expr
              )
               (
                call/cc (
                  lambda (
                    ret3
                  )
                   (
                    let (
                      (
                        count 0
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
                                          < i (
                                            _len expr
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                ch (
                                                  _substring expr i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  string=? ch "("
                                                )
                                                 (
                                                  begin (
                                                    set! count (
                                                      + count 1
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
                                                  string=? ch ")"
                                                )
                                                 (
                                                  begin (
                                                    set! count (
                                                      - count 1
                                                    )
                                                  )
                                                   (
                                                    if (
                                                      < count 0
                                                    )
                                                     (
                                                      begin (
                                                        ret3 #f
                                                      )
                                                    )
                                                     (
                                                      quote (
                                                        
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
                            ret3 (
                              equal? count 0
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
                is_letter ch
              )
               (
                call/cc (
                  lambda (
                    ret6
                  )
                   (
                    ret6 (
                      or (
                        and (
                          string<=? "a" ch
                        )
                         (
                          string<=? ch "z"
                        )
                      )
                       (
                        and (
                          string<=? "A" ch
                        )
                         (
                          string<=? ch "Z"
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
                        string<=? "0" ch
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
                is_alnum ch
              )
               (
                call/cc (
                  lambda (
                    ret8
                  )
                   (
                    ret8 (
                      or (
                        is_letter ch
                      )
                       (
                        is_digit ch
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                infix_to_postfix expression
              )
               (
                call/cc (
                  lambda (
                    ret9
                  )
                   (
                    begin (
                      if (
                        eq? (
                          balanced_parentheses expression
                        )
                         #f
                      )
                       (
                        begin (
                          panic "Mismatched parentheses"
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
                          let (
                            (
                              postfix (
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
                                                  _len expression
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      ch (
                                                        _substring expression i (
                                                          + i 1
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      if (
                                                        is_alnum ch
                                                      )
                                                       (
                                                        begin (
                                                          set! postfix (
                                                            append postfix (
                                                              _list ch
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        if (
                                                          string=? ch "("
                                                        )
                                                         (
                                                          begin (
                                                            set! stack (
                                                              append stack (
                                                                _list ch
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          if (
                                                            string=? ch ")"
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
                                                                            and (
                                                                              > (
                                                                                _len stack
                                                                              )
                                                                               0
                                                                            )
                                                                             (
                                                                              not (
                                                                                string=? (
                                                                                  list-ref stack (
                                                                                    - (
                                                                                      _len stack
                                                                                    )
                                                                                     1
                                                                                  )
                                                                                )
                                                                                 "("
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              set! postfix (
                                                                                append postfix (
                                                                                  _list (
                                                                                    list-ref stack (
                                                                                      - (
                                                                                        _len stack
                                                                                      )
                                                                                       1
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              set! stack (
                                                                                slice stack 0 (
                                                                                  - (
                                                                                    _len stack
                                                                                  )
                                                                                   1
                                                                                )
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
                                                              if (
                                                                > (
                                                                  _len stack
                                                                )
                                                                 0
                                                              )
                                                               (
                                                                begin (
                                                                  set! stack (
                                                                    slice stack 0 (
                                                                      - (
                                                                        _len stack
                                                                      )
                                                                       1
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                quote (
                                                                  
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            if (
                                                              string=? ch " "
                                                            )
                                                             (
                                                              begin
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
                                                                            if #t (
                                                                              begin (
                                                                                if (
                                                                                  equal? (
                                                                                    _len stack
                                                                                  )
                                                                                   0
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! stack (
                                                                                      append stack (
                                                                                        _list ch
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    break15 (
                                                                                      quote (
                                                                                        
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
                                                                                    cp (
                                                                                      precedence ch
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        tp (
                                                                                          precedence (
                                                                                            list-ref stack (
                                                                                              - (
                                                                                                _len stack
                                                                                              )
                                                                                               1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        if (
                                                                                          _gt cp tp
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! stack (
                                                                                              append stack (
                                                                                                _list ch
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            break15 (
                                                                                              quote (
                                                                                                
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
                                                                                          _lt cp tp
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! postfix (
                                                                                              append postfix (
                                                                                                _list (
                                                                                                  list-ref stack (
                                                                                                    - (
                                                                                                      _len stack
                                                                                                    )
                                                                                                     1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! stack (
                                                                                              slice stack 0 (
                                                                                                - (
                                                                                                  _len stack
                                                                                                )
                                                                                                 1
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
                                                                                       (
                                                                                        if (
                                                                                          string=? (
                                                                                            associativity ch
                                                                                          )
                                                                                           "RL"
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! stack (
                                                                                              append stack (
                                                                                                _list ch
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            break15 (
                                                                                              quote (
                                                                                                
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
                                                                                        set! postfix (
                                                                                          append postfix (
                                                                                            _list (
                                                                                              list-ref stack (
                                                                                                - (
                                                                                                  _len stack
                                                                                                )
                                                                                                 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        set! stack (
                                                                                          slice stack 0 (
                                                                                            - (
                                                                                              _len stack
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
                                                  _len stack
                                                )
                                                 0
                                              )
                                               (
                                                begin (
                                                  set! postfix (
                                                    append postfix (
                                                      _list (
                                                        list-ref stack (
                                                          - (
                                                            _len stack
                                                          )
                                                           1
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  set! stack (
                                                    slice stack 0 (
                                                      - (
                                                        _len stack
                                                      )
                                                       1
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
                                  let (
                                    (
                                      res ""
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
                                                        < j (
                                                          _len postfix
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          if (
                                                            > j 0
                                                          )
                                                           (
                                                            begin (
                                                              set! res (
                                                                string-append res " "
                                                              )
                                                            )
                                                          )
                                                           (
                                                            quote (
                                                              
                                                            )
                                                          )
                                                        )
                                                         (
                                                          set! res (
                                                            string-append res (
                                                              list-ref postfix j
                                                            )
                                                          )
                                                        )
                                                         (
                                                          set! j (
                                                            + j 1
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
                                          ret9 res
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
                    ret20
                  )
                   (
                    let (
                      (
                        expression "a+b*(c^d-e)^(f+g*h)-i"
                      )
                    )
                     (
                      begin (
                        _display (
                          if (
                            string? expression
                          )
                           expression (
                            to-str expression
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
                              infix_to_postfix expression
                            )
                          )
                           (
                            infix_to_postfix expression
                          )
                           (
                            to-str (
                              infix_to_postfix expression
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
             (
              main
            )
          )
        )
      )
    )
     (
      let (
        (
          end22 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur23 (
              quotient (
                * (
                  - end22 start21
                )
                 1000000
              )
               jps24
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur23
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
