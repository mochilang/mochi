;; Generated on 2025-08-06 23:15 +0700
(import (scheme base))
(import (scheme time))
(import (chibi string))
(import (only (scheme char) string-upcase string-downcase))
(import (srfi 69))
(import (srfi 1))
(define _list list)
(import (chibi io))
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
(define (_input)
  (let ((l (read-line)))
    (if (eof-object? l) "" l)))
(
  let (
    (
      start16 (
        current-jiffy
      )
    )
     (
      jps19 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        join_strings xs
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
                                    _len xs
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      string-append res (
                                        list-ref xs i
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
        encrypt_message key message
      )
       (
        call/cc (
          lambda (
            ret4
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
                    col 0
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
                                  < col key
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        pointer col
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
                                                      < pointer (
                                                        _len message
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! result (
                                                          string-append result (
                                                            _substring message pointer (
                                                              + pointer 1
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! pointer (
                                                          + pointer key
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
                                        set! col (
                                          + col 1
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
                    ret4 result
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
        decrypt_message key message
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                num_cols (
                  _div (
                    - (
                      + (
                        _len message
                      )
                       key
                    )
                     1
                  )
                   key
                )
              )
            )
             (
              begin (
                let (
                  (
                    num_rows key
                  )
                )
                 (
                  begin (
                    let (
                      (
                        num_shaded_boxes (
                          - (
                            * num_cols num_rows
                          )
                           (
                            _len message
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            plain_text (
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
                                              < i num_cols
                                            )
                                             (
                                              begin (
                                                set! plain_text (
                                                  append plain_text (
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
                                    col 0
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
                                        let (
                                          (
                                            index 0
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
                                                          < index (
                                                            _len message
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            list-set! plain_text col (
                                                              string-append (
                                                                list-ref plain_text col
                                                              )
                                                               (
                                                                _substring message index (
                                                                  + index 1
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! col (
                                                              + col 1
                                                            )
                                                          )
                                                           (
                                                            if (
                                                              or (
                                                                equal? col num_cols
                                                              )
                                                               (
                                                                and (
                                                                  equal? col (
                                                                    - num_cols 1
                                                                  )
                                                                )
                                                                 (
                                                                  >= row (
                                                                    - num_rows num_shaded_boxes
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! col 0
                                                              )
                                                               (
                                                                set! row (
                                                                  + row 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! index (
                                                              + index 1
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
                                            ret9 (
                                              join_strings plain_text
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
        main
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            begin (
              _display (
                if (
                  string? "Enter message: "
                )
                 "Enter message: " (
                  to-str "Enter message: "
                )
              )
            )
             (
              newline
            )
             (
              let (
                (
                  message (
                    _input
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      max_key (
                        - (
                          _len message
                        )
                         1
                      )
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            string-append (
                              string-append "Enter key [2-" (
                                to-str-space max_key
                              )
                            )
                             "]: "
                          )
                        )
                         (
                          string-append (
                            string-append "Enter key [2-" (
                              to-str-space max_key
                            )
                          )
                           "]: "
                        )
                         (
                          to-str (
                            string-append (
                              string-append "Enter key [2-" (
                                to-str-space max_key
                              )
                            )
                             "]: "
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
                          key (
                            let (
                              (
                                v15 (
                                  _input
                                )
                              )
                            )
                             (
                              cond (
                                (
                                  string? v15
                                )
                                 (
                                  exact (
                                    floor (
                                      string->number v15
                                    )
                                  )
                                )
                              )
                               (
                                (
                                  boolean? v15
                                )
                                 (
                                  if v15 1 0
                                )
                              )
                               (
                                else (
                                  exact (
                                    floor v15
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "Encryption/Decryption [e/d]: "
                            )
                             "Encryption/Decryption [e/d]: " (
                              to-str "Encryption/Decryption [e/d]: "
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          let (
                            (
                              mode (
                                _input
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  text ""
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      first (
                                        _substring mode 0 1
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      if (
                                        or (
                                          string=? first "e"
                                        )
                                         (
                                          string=? first "E"
                                        )
                                      )
                                       (
                                        begin (
                                          set! text (
                                            encrypt_message key message
                                          )
                                        )
                                      )
                                       (
                                        if (
                                          or (
                                            string=? first "d"
                                          )
                                           (
                                            string=? first "D"
                                          )
                                        )
                                         (
                                          begin (
                                            set! text (
                                              decrypt_message key message
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
                                      _display (
                                        if (
                                          string? (
                                            string-append (
                                              string-append "Output:\n" text
                                            )
                                             "|"
                                          )
                                        )
                                         (
                                          string-append (
                                            string-append "Output:\n" text
                                          )
                                           "|"
                                        )
                                         (
                                          to-str (
                                            string-append (
                                              string-append "Output:\n" text
                                            )
                                             "|"
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
     (
      main
    )
     (
      let (
        (
          end17 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur18 (
              quotient (
                * (
                  - end17 start16
                )
                 1000000
              )
               jps19
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur18
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
