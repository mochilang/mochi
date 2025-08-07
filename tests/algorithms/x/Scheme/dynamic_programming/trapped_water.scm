;; Generated on 2025-08-07 08:40 +0700
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
      start13 (
        current-jiffy
      )
    )
     (
      jps16 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        make_list len value
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                arr (
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
                                  < i len
                                )
                                 (
                                  begin (
                                    set! arr (
                                      append arr (
                                        _list value
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
                    ret1 arr
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
        trapped_rainwater heights
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                equal? (
                  _len heights
                )
                 0
              )
               (
                begin (
                  ret4 0
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
                                  _len heights
                                )
                              )
                               (
                                begin (
                                  if (
                                    < (
                                      list-ref heights i
                                    )
                                     0
                                  )
                                   (
                                    begin (
                                      panic "No height can be negative"
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
                      length (
                        _len heights
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          left_max (
                            make_list length 0
                          )
                        )
                      )
                       (
                        begin (
                          list-set! left_max 0 (
                            list-ref heights 0
                          )
                        )
                         (
                          set! i 1
                        )
                         (
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
                                        < i length
                                      )
                                       (
                                        begin (
                                          if (
                                            _gt (
                                              list-ref heights i
                                            )
                                             (
                                              cond (
                                                (
                                                  string? left_max
                                                )
                                                 (
                                                  _substring left_max (
                                                    - i 1
                                                  )
                                                   (
                                                    + (
                                                      - i 1
                                                    )
                                                     1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? left_max
                                                )
                                                 (
                                                  hash-table-ref left_max (
                                                    - i 1
                                                  )
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref left_max (
                                                    - i 1
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              list-set! left_max i (
                                                list-ref heights i
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              list-set! left_max i (
                                                cond (
                                                  (
                                                    string? left_max
                                                  )
                                                   (
                                                    _substring left_max (
                                                      - i 1
                                                    )
                                                     (
                                                      + (
                                                        - i 1
                                                      )
                                                       1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? left_max
                                                  )
                                                   (
                                                    hash-table-ref left_max (
                                                      - i 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref left_max (
                                                      - i 1
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
                          let (
                            (
                              right_max (
                                make_list length 0
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  last (
                                    - length 1
                                  )
                                )
                              )
                               (
                                begin (
                                  list-set! right_max last (
                                    list-ref heights last
                                  )
                                )
                                 (
                                  set! i (
                                    - last 1
                                  )
                                )
                                 (
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
                                                >= i 0
                                              )
                                               (
                                                begin (
                                                  if (
                                                    _gt (
                                                      list-ref heights i
                                                    )
                                                     (
                                                      cond (
                                                        (
                                                          string? right_max
                                                        )
                                                         (
                                                          _substring right_max (
                                                            + i 1
                                                          )
                                                           (
                                                            + (
                                                              + i 1
                                                            )
                                                             1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? right_max
                                                        )
                                                         (
                                                          hash-table-ref right_max (
                                                            + i 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref right_max (
                                                            + i 1
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      list-set! right_max i (
                                                        list-ref heights i
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      list-set! right_max i (
                                                        cond (
                                                          (
                                                            string? right_max
                                                          )
                                                           (
                                                            _substring right_max (
                                                              + i 1
                                                            )
                                                             (
                                                              + (
                                                                + i 1
                                                              )
                                                               1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          (
                                                            hash-table? right_max
                                                          )
                                                           (
                                                            hash-table-ref right_max (
                                                              + i 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          else (
                                                            list-ref right_max (
                                                              + i 1
                                                            )
                                                          )
                                                        )
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
                                  let (
                                    (
                                      total 0
                                    )
                                  )
                                   (
                                    begin (
                                      set! i 0
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
                                                    < i length
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          left (
                                                            cond (
                                                              (
                                                                string? left_max
                                                              )
                                                               (
                                                                _substring left_max i (
                                                                  + i 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? left_max
                                                              )
                                                               (
                                                                hash-table-ref left_max i
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref left_max i
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
                                                                cond (
                                                                  (
                                                                    string? right_max
                                                                  )
                                                                   (
                                                                    _substring right_max i (
                                                                      + i 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? right_max
                                                                  )
                                                                   (
                                                                    hash-table-ref right_max i
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref right_max i
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  smaller (
                                                                    if (
                                                                      _lt left right
                                                                    )
                                                                     left right
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  set! total (
                                                                    _add total (
                                                                      - smaller (
                                                                        list-ref heights i
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
                                      ret4 total
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
      _display (
        if (
          string? (
            to-str-space (
              trapped_rainwater (
                _list 0 1 0 2 1 0 1 3 2 1 2 1
              )
            )
          )
        )
         (
          to-str-space (
            trapped_rainwater (
              _list 0 1 0 2 1 0 1 3 2 1 2 1
            )
          )
        )
         (
          to-str (
            to-str-space (
              trapped_rainwater (
                _list 0 1 0 2 1 0 1 3 2 1 2 1
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
              trapped_rainwater (
                _list 7 1 5 3 6 4
              )
            )
          )
        )
         (
          to-str-space (
            trapped_rainwater (
              _list 7 1 5 3 6 4
            )
          )
        )
         (
          to-str (
            to-str-space (
              trapped_rainwater (
                _list 7 1 5 3 6 4
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
          end14 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur15 (
              quotient (
                * (
                  - end14 start13
                )
                 1000000
              )
               jps16
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur15
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
