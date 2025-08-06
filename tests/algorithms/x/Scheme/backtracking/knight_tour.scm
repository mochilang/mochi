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
      define (
        get_valid_pos position n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                y (
                  list-ref position 0
                )
              )
            )
             (
              begin (
                let (
                  (
                    x (
                      list-ref position 1
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        positions (
                          _list (
                            _list (
                              + y 1
                            )
                             (
                              + x 2
                            )
                          )
                           (
                            _list (
                              - y 1
                            )
                             (
                              + x 2
                            )
                          )
                           (
                            _list (
                              + y 1
                            )
                             (
                              - x 2
                            )
                          )
                           (
                            _list (
                              - y 1
                            )
                             (
                              - x 2
                            )
                          )
                           (
                            _list (
                              + y 2
                            )
                             (
                              + x 1
                            )
                          )
                           (
                            _list (
                              + y 2
                            )
                             (
                              - x 1
                            )
                          )
                           (
                            _list (
                              - y 2
                            )
                             (
                              + x 1
                            )
                          )
                           (
                            _list (
                              - y 2
                            )
                             (
                              - x 1
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            permissible (
                              _list
                            )
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
                                        idx
                                      )
                                       (
                                        if (
                                          < idx (
                                            _len positions
                                          )
                                        )
                                         (
                                          begin (
                                            begin (
                                              let (
                                                (
                                                  inner (
                                                    list-ref positions idx
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      y_test (
                                                        list-ref inner 0
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          x_test (
                                                            list-ref inner 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          if (
                                                            and (
                                                              and (
                                                                and (
                                                                  >= y_test 0
                                                                )
                                                                 (
                                                                  < y_test n
                                                                )
                                                              )
                                                               (
                                                                >= x_test 0
                                                              )
                                                            )
                                                             (
                                                              < x_test n
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              set! permissible (
                                                                append permissible (
                                                                  _list inner
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
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop2 (
                                              + idx 1
                                            )
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
                                  loop2 0
                                )
                              )
                            )
                          )
                           (
                            ret1 permissible
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
        is_complete board
      )
       (
        call/cc (
          lambda (
            ret4
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
                          i
                        )
                         (
                          if (
                            < i (
                              _len board
                            )
                          )
                           (
                            begin (
                              begin (
                                let (
                                  (
                                    row (
                                      list-ref board i
                                    )
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
                                                j
                                              )
                                               (
                                                if (
                                                  < j (
                                                    _len row
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    begin (
                                                      if (
                                                        equal? (
                                                          list-ref row j
                                                        )
                                                         0
                                                      )
                                                       (
                                                        begin (
                                                          ret4 #f
                                                        )
                                                      )
                                                       (
                                                        quote (
                                                          
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    loop7 (
                                                      + j 1
                                                    )
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
                                          loop7 0
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop5 (
                                + i 1
                              )
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
                    loop5 0
                  )
                )
              )
            )
             (
              ret4 #t
            )
          )
        )
      )
    )
     (
      define (
        open_knight_tour_helper board pos curr
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            begin (
              if (
                is_complete board
              )
               (
                begin (
                  ret9 #t
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
                  moves (
                    get_valid_pos pos (
                      _len board
                    )
                  )
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
                              i
                            )
                             (
                              if (
                                < i (
                                  _len moves
                                )
                              )
                               (
                                begin (
                                  begin (
                                    let (
                                      (
                                        position (
                                          cond (
                                            (
                                              string? moves
                                            )
                                             (
                                              _substring moves i (
                                                + i 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? moves
                                            )
                                             (
                                              hash-table-ref moves i
                                            )
                                          )
                                           (
                                            else (
                                              list-ref moves i
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            y (
                                              cond (
                                                (
                                                  string? position
                                                )
                                                 (
                                                  _substring position 0 (
                                                    + 0 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? position
                                                )
                                                 (
                                                  hash-table-ref position 0
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref position 0
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                x (
                                                  cond (
                                                    (
                                                      string? position
                                                    )
                                                     (
                                                      _substring position 1 (
                                                        + 1 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? position
                                                    )
                                                     (
                                                      hash-table-ref position 1
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref position 1
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  equal? (
                                                    cond (
                                                      (
                                                        string? (
                                                          list-ref board y
                                                        )
                                                      )
                                                       (
                                                        _substring (
                                                          list-ref board y
                                                        )
                                                         x (
                                                          + x 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        hash-table? (
                                                          list-ref board y
                                                        )
                                                      )
                                                       (
                                                        hash-table-ref (
                                                          list-ref board y
                                                        )
                                                         x
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        list-ref (
                                                          list-ref board y
                                                        )
                                                         x
                                                      )
                                                    )
                                                  )
                                                   0
                                                )
                                                 (
                                                  begin (
                                                    list-set! (
                                                      list-ref board y
                                                    )
                                                     x (
                                                      + curr 1
                                                    )
                                                  )
                                                   (
                                                    if (
                                                      open_knight_tour_helper board position (
                                                        + curr 1
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        ret9 #t
                                                      )
                                                    )
                                                     (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                  )
                                                   (
                                                    list-set! (
                                                      list-ref board y
                                                    )
                                                     x 0
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
                                    )
                                  )
                                )
                                 (
                                  loop10 (
                                    + i 1
                                  )
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
                        loop10 0
                      )
                    )
                  )
                )
                 (
                  ret9 #f
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        open_knight_tour n
      )
       (
        call/cc (
          lambda (
            ret12
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
                call/cc (
                  lambda (
                    break14
                  )
                   (
                    letrec (
                      (
                        loop13 (
                          lambda (
                            i
                          )
                           (
                            if (
                              < i n
                            )
                             (
                              begin (
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
                                      call/cc (
                                        lambda (
                                          break16
                                        )
                                         (
                                          letrec (
                                            (
                                              loop15 (
                                                lambda (
                                                  j
                                                )
                                                 (
                                                  if (
                                                    < j n
                                                  )
                                                   (
                                                    begin (
                                                      begin (
                                                        set! row (
                                                          append row (
                                                            _list 0
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      loop15 (
                                                        + j 1
                                                      )
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
                                            loop15 0
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
                                  )
                                )
                              )
                               (
                                loop13 (
                                  + i 1
                                )
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
                      loop13 0
                    )
                  )
                )
              )
               (
                call/cc (
                  lambda (
                    break18
                  )
                   (
                    letrec (
                      (
                        loop17 (
                          lambda (
                            i
                          )
                           (
                            if (
                              < i n
                            )
                             (
                              begin (
                                begin (
                                  call/cc (
                                    lambda (
                                      break20
                                    )
                                     (
                                      letrec (
                                        (
                                          loop19 (
                                            lambda (
                                              j
                                            )
                                             (
                                              if (
                                                < j n
                                              )
                                               (
                                                begin (
                                                  begin (
                                                    list-set! (
                                                      list-ref board i
                                                    )
                                                     j 1
                                                  )
                                                   (
                                                    if (
                                                      open_knight_tour_helper board (
                                                        _list i j
                                                      )
                                                       1
                                                    )
                                                     (
                                                      begin (
                                                        ret12 board
                                                      )
                                                    )
                                                     (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                  )
                                                   (
                                                    list-set! (
                                                      list-ref board i
                                                    )
                                                     j 0
                                                  )
                                                )
                                                 (
                                                  loop19 (
                                                    + j 1
                                                  )
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
                                        loop19 0
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                loop17 (
                                  + i 1
                                )
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
                      loop17 0
                    )
                  )
                )
              )
               (
                _display (
                  if (
                    string? (
                      string-append "Open Knight Tour cannot be performed on a board of size " (
                        to-str-space n
                      )
                    )
                  )
                   (
                    string-append "Open Knight Tour cannot be performed on a board of size " (
                      to-str-space n
                    )
                  )
                   (
                    to-str (
                      string-append "Open Knight Tour cannot be performed on a board of size " (
                        to-str-space n
                      )
                    )
                  )
                )
              )
               (
                newline
              )
               (
                ret12 board
              )
            )
          )
        )
      )
    )
     (
      let (
        (
          board (
            open_knight_tour 1
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                cond (
                  (
                    string? (
                      cond (
                        (
                          string? board
                        )
                         (
                          _substring board 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? board
                        )
                         (
                          hash-table-ref board 0
                        )
                      )
                       (
                        else (
                          list-ref board 0
                        )
                      )
                    )
                  )
                   (
                    _substring (
                      cond (
                        (
                          string? board
                        )
                         (
                          _substring board 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? board
                        )
                         (
                          hash-table-ref board 0
                        )
                      )
                       (
                        else (
                          list-ref board 0
                        )
                      )
                    )
                     0 (
                      + 0 1
                    )
                  )
                )
                 (
                  (
                    hash-table? (
                      cond (
                        (
                          string? board
                        )
                         (
                          _substring board 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? board
                        )
                         (
                          hash-table-ref board 0
                        )
                      )
                       (
                        else (
                          list-ref board 0
                        )
                      )
                    )
                  )
                   (
                    hash-table-ref (
                      cond (
                        (
                          string? board
                        )
                         (
                          _substring board 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? board
                        )
                         (
                          hash-table-ref board 0
                        )
                      )
                       (
                        else (
                          list-ref board 0
                        )
                      )
                    )
                     0
                  )
                )
                 (
                  else (
                    list-ref (
                      cond (
                        (
                          string? board
                        )
                         (
                          _substring board 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? board
                        )
                         (
                          hash-table-ref board 0
                        )
                      )
                       (
                        else (
                          list-ref board 0
                        )
                      )
                    )
                     0
                  )
                )
              )
            )
             (
              cond (
                (
                  string? (
                    cond (
                      (
                        string? board
                      )
                       (
                        _substring board 0 (
                          + 0 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? board
                      )
                       (
                        hash-table-ref board 0
                      )
                    )
                     (
                      else (
                        list-ref board 0
                      )
                    )
                  )
                )
                 (
                  _substring (
                    cond (
                      (
                        string? board
                      )
                       (
                        _substring board 0 (
                          + 0 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? board
                      )
                       (
                        hash-table-ref board 0
                      )
                    )
                     (
                      else (
                        list-ref board 0
                      )
                    )
                  )
                   0 (
                    + 0 1
                  )
                )
              )
               (
                (
                  hash-table? (
                    cond (
                      (
                        string? board
                      )
                       (
                        _substring board 0 (
                          + 0 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? board
                      )
                       (
                        hash-table-ref board 0
                      )
                    )
                     (
                      else (
                        list-ref board 0
                      )
                    )
                  )
                )
                 (
                  hash-table-ref (
                    cond (
                      (
                        string? board
                      )
                       (
                        _substring board 0 (
                          + 0 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? board
                      )
                       (
                        hash-table-ref board 0
                      )
                    )
                     (
                      else (
                        list-ref board 0
                      )
                    )
                  )
                   0
                )
              )
               (
                else (
                  list-ref (
                    cond (
                      (
                        string? board
                      )
                       (
                        _substring board 0 (
                          + 0 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? board
                      )
                       (
                        hash-table-ref board 0
                      )
                    )
                     (
                      else (
                        list-ref board 0
                      )
                    )
                  )
                   0
                )
              )
            )
             (
              to-str (
                cond (
                  (
                    string? (
                      cond (
                        (
                          string? board
                        )
                         (
                          _substring board 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? board
                        )
                         (
                          hash-table-ref board 0
                        )
                      )
                       (
                        else (
                          list-ref board 0
                        )
                      )
                    )
                  )
                   (
                    _substring (
                      cond (
                        (
                          string? board
                        )
                         (
                          _substring board 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? board
                        )
                         (
                          hash-table-ref board 0
                        )
                      )
                       (
                        else (
                          list-ref board 0
                        )
                      )
                    )
                     0 (
                      + 0 1
                    )
                  )
                )
                 (
                  (
                    hash-table? (
                      cond (
                        (
                          string? board
                        )
                         (
                          _substring board 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? board
                        )
                         (
                          hash-table-ref board 0
                        )
                      )
                       (
                        else (
                          list-ref board 0
                        )
                      )
                    )
                  )
                   (
                    hash-table-ref (
                      cond (
                        (
                          string? board
                        )
                         (
                          _substring board 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? board
                        )
                         (
                          hash-table-ref board 0
                        )
                      )
                       (
                        else (
                          list-ref board 0
                        )
                      )
                    )
                     0
                  )
                )
                 (
                  else (
                    list-ref (
                      cond (
                        (
                          string? board
                        )
                         (
                          _substring board 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? board
                        )
                         (
                          hash-table-ref board 0
                        )
                      )
                       (
                        else (
                          list-ref board 0
                        )
                      )
                    )
                     0
                  )
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
