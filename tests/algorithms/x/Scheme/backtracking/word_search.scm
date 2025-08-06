;; Generated on 2025-08-06 18:11 +0700
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
      start14 (
        current-jiffy
      )
    )
     (
      jps17 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        contains xs x
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
                                    ret1 #t
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
                ret1 #f
              )
            )
          )
        )
      )
    )
     (
      define (
        get_point_key len_board len_board_column row column
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            ret4 (
              + (
                * (
                  * len_board len_board_column
                )
                 row
              )
               column
            )
          )
        )
      )
    )
     (
      define (
        search_from board word row column word_index visited
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              if (
                not (
                  string=? (
                    cond (
                      (
                        string? (
                          list-ref board row
                        )
                      )
                       (
                        _substring (
                          list-ref board row
                        )
                         column (
                          + column 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? (
                          list-ref board row
                        )
                      )
                       (
                        hash-table-ref (
                          list-ref board row
                        )
                         column
                      )
                    )
                     (
                      else (
                        list-ref (
                          list-ref board row
                        )
                         column
                      )
                    )
                  )
                   (
                    _substring word word_index (
                      + word_index 1
                    )
                  )
                )
              )
               (
                begin (
                  ret5 #f
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                equal? word_index (
                  - (
                    _len word
                  )
                   1
                )
              )
               (
                begin (
                  ret5 #t
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
                  len_board (
                    _len board
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      len_board_column (
                        _len (
                          list-ref board 0
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          dir_i (
                            _list 0 0 (
                              - 1
                            )
                             1
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              dir_j (
                                _list 1 (
                                  - 1
                                )
                                 0 0
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  k 0
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
                                                < k 4
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      next_i (
                                                        + row (
                                                          list-ref dir_i k
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          next_j (
                                                            + column (
                                                              list-ref dir_j k
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          if (
                                                            not (
                                                              and (
                                                                and (
                                                                  and (
                                                                    <= 0 next_i
                                                                  )
                                                                   (
                                                                    < next_i len_board
                                                                  )
                                                                )
                                                                 (
                                                                  <= 0 next_j
                                                                )
                                                              )
                                                               (
                                                                < next_j len_board_column
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              set! k (
                                                                + k 1
                                                              )
                                                            )
                                                             (
                                                              loop6
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
                                                              key (
                                                                get_point_key len_board len_board_column next_i next_j
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              if (
                                                                contains visited key
                                                              )
                                                               (
                                                                begin (
                                                                  set! k (
                                                                    + k 1
                                                                  )
                                                                )
                                                                 (
                                                                  loop6
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
                                                                  new_visited (
                                                                    append visited (
                                                                      _list key
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  if (
                                                                    search_from board word next_i next_j (
                                                                      + word_index 1
                                                                    )
                                                                     new_visited
                                                                  )
                                                                   (
                                                                    begin (
                                                                      ret5 #t
                                                                    )
                                                                  )
                                                                   (
                                                                    quote (
                                                                      
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
                                                  )
                                                )
                                                 (
                                                  loop6
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
                                        loop6
                                      )
                                    )
                                  )
                                )
                                 (
                                  ret5 #f
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
        word_exists board word
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                len_board (
                  _len board
                )
              )
            )
             (
              begin (
                let (
                  (
                    len_board_column (
                      _len (
                        list-ref board 0
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
                                      < i len_board
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
                                                break12
                                              )
                                               (
                                                letrec (
                                                  (
                                                    loop11 (
                                                      lambda (
                                                        
                                                      )
                                                       (
                                                        if (
                                                          < j len_board_column
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                key (
                                                                  get_point_key len_board len_board_column i j
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    visited (
                                                                      append (
                                                                        _list
                                                                      )
                                                                       (
                                                                        _list key
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      search_from board word i j 0 visited
                                                                    )
                                                                     (
                                                                      begin (
                                                                        ret8 #t
                                                                      )
                                                                    )
                                                                     (
                                                                      quote (
                                                                        
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
                                                            )
                                                          )
                                                           (
                                                            loop11
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
                                                  loop11
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
                        ret8 #f
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
            ret13
          )
           (
            let (
              (
                board (
                  _list (
                    _list "A" "B" "C" "E"
                  )
                   (
                    _list "S" "F" "C" "S"
                  )
                   (
                    _list "A" "D" "E" "E"
                  )
                )
              )
            )
             (
              begin (
                _display (
                  if (
                    string? (
                      word_exists board "ABCCED"
                    )
                  )
                   (
                    word_exists board "ABCCED"
                  )
                   (
                    to-str (
                      word_exists board "ABCCED"
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
                      word_exists board "SEE"
                    )
                  )
                   (
                    word_exists board "SEE"
                  )
                   (
                    to-str (
                      word_exists board "SEE"
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
                      word_exists board "ABCB"
                    )
                  )
                   (
                    word_exists board "ABCB"
                  )
                   (
                    to-str (
                      word_exists board "ABCB"
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
     (
      let (
        (
          end15 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur16 (
              quotient (
                * (
                  - end15 start14
                )
                 1000000
              )
               jps17
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur16
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
