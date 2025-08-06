;; Generated on 2025-08-06 16:21 +0700
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
      start23 (
        current-jiffy
      )
    )
     (
      jps26 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        create_board n
      )
       (
        call/cc (
          lambda (
            ret1
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
                                                          < j n
                                                        )
                                                         (
                                                          begin (
                                                            set! row (
                                                              append row (
                                                                _list 0
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! j (
                                                              + j 1
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
                                            set! board (
                                              append board (
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
                    ret1 board
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
        is_safe board row column
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                n (
                  _len board
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
                                  < i row
                                )
                                 (
                                  begin (
                                    if (
                                      equal? (
                                        cond (
                                          (
                                            string? (
                                              list-ref board i
                                            )
                                          )
                                           (
                                            _substring (
                                              list-ref board i
                                            )
                                             column (
                                              + column 1
                                            )
                                          )
                                        )
                                         (
                                          (
                                            hash-table? (
                                              list-ref board i
                                            )
                                          )
                                           (
                                            hash-table-ref (
                                              list-ref board i
                                            )
                                             column
                                          )
                                        )
                                         (
                                          else (
                                            list-ref (
                                              list-ref board i
                                            )
                                             column
                                          )
                                        )
                                      )
                                       1
                                    )
                                     (
                                      begin (
                                        ret6 #f
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
                    set! i (
                      - row 1
                    )
                  )
                   (
                    let (
                      (
                        j (
                          - column 1
                        )
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
                                      and (
                                        >= i 0
                                      )
                                       (
                                        >= j 0
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          equal? (
                                            cond (
                                              (
                                                string? (
                                                  list-ref board i
                                                )
                                              )
                                               (
                                                _substring (
                                                  list-ref board i
                                                )
                                                 j (
                                                  + j 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? (
                                                  list-ref board i
                                                )
                                              )
                                               (
                                                hash-table-ref (
                                                  list-ref board i
                                                )
                                                 j
                                              )
                                            )
                                             (
                                              else (
                                                list-ref (
                                                  list-ref board i
                                                )
                                                 j
                                              )
                                            )
                                          )
                                           1
                                        )
                                         (
                                          begin (
                                            ret6 #f
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
                                       (
                                        set! j (
                                          - j 1
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
                        set! i (
                          - row 1
                        )
                      )
                       (
                        set! j (
                          + column 1
                        )
                      )
                       (
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
                                      and (
                                        >= i 0
                                      )
                                       (
                                        < j n
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          equal? (
                                            cond (
                                              (
                                                string? (
                                                  list-ref board i
                                                )
                                              )
                                               (
                                                _substring (
                                                  list-ref board i
                                                )
                                                 j (
                                                  + j 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? (
                                                  list-ref board i
                                                )
                                              )
                                               (
                                                hash-table-ref (
                                                  list-ref board i
                                                )
                                                 j
                                              )
                                            )
                                             (
                                              else (
                                                list-ref (
                                                  list-ref board i
                                                )
                                                 j
                                              )
                                            )
                                          )
                                           1
                                        )
                                         (
                                          begin (
                                            ret6 #f
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
                                       (
                                        set! j (
                                          + j 1
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
                        ret6 #t
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
        row_string row
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            let (
              (
                s ""
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
                                  < j (
                                    _len row
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? (
                                        list-ref row j
                                      )
                                       1
                                    )
                                     (
                                      begin (
                                        set! s (
                                          string-append s "Q "
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! s (
                                          string-append s ". "
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
                    ret13 s
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
        printboard board
      )
       (
        call/cc (
          lambda (
            ret16
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
                              < i (
                                _len board
                              )
                            )
                             (
                              begin (
                                _display (
                                  if (
                                    string? (
                                      row_string (
                                        list-ref board i
                                      )
                                    )
                                  )
                                   (
                                    row_string (
                                      list-ref board i
                                    )
                                  )
                                   (
                                    to-str (
                                      row_string (
                                        list-ref board i
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
            )
          )
        )
      )
    )
     (
      define (
        solve board row
      )
       (
        call/cc (
          lambda (
            ret19
          )
           (
            begin (
              if (
                >= row (
                  _len board
                )
              )
               (
                begin (
                  printboard board
                )
                 (
                  _display (
                    if (
                      string? ""
                    )
                     "" (
                      to-str ""
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  ret19 1
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
                                    < i (
                                      _len board
                                    )
                                  )
                                   (
                                    begin (
                                      if (
                                        is_safe board row i
                                      )
                                       (
                                        begin (
                                          list-set! (
                                            list-ref board row
                                          )
                                           i 1
                                        )
                                         (
                                          set! count (
                                            _add count (
                                              solve board (
                                                + row 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          list-set! (
                                            list-ref board row
                                          )
                                           i 0
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
                      ret19 count
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
        n_queens n
      )
       (
        call/cc (
          lambda (
            ret22
          )
           (
            let (
              (
                board (
                  create_board n
                )
              )
            )
             (
              begin (
                let (
                  (
                    total (
                      solve board 0
                    )
                  )
                )
                 (
                  begin (
                    _display (
                      if (
                        string? (
                          string-append "The total number of solutions are: " (
                            to-str-space total
                          )
                        )
                      )
                       (
                        string-append "The total number of solutions are: " (
                          to-str-space total
                        )
                      )
                       (
                        to-str (
                          string-append "The total number of solutions are: " (
                            to-str-space total
                          )
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    ret22 total
                  )
                )
              )
            )
          )
        )
      )
    )
     (
      n_queens 4
    )
     (
      let (
        (
          end24 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur25 (
              quotient (
                * (
                  - end24 start23
                )
                 1000000
              )
               jps26
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur25
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
