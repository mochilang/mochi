;; Generated on 2025-08-06 23:57 +0700
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
        tail xs
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
                    i 1
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
        rotate_left xs
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
                  _len xs
                )
                 0
              )
               (
                begin (
                  ret4 xs
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
                  res (
                    _list
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      i 1
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
                                      _len xs
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
                      set! res (
                        append res (
                          _list (
                            list-ref xs 0
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
     (
      define (
        permute_recursive nums
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              if (
                equal? (
                  _len nums
                )
                 0
              )
               (
                begin (
                  let (
                    (
                      base (
                        _list
                      )
                    )
                  )
                   (
                    begin (
                      ret7 (
                        append base (
                          _list (
                            _list
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
                  result (
                    _list
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      current nums
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          count 0
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
                                        < count (
                                          _len nums
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              n (
                                                list-ref current 0
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  rest (
                                                    tail current
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      perms (
                                                        permute_recursive rest
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
                                                                        < j (
                                                                          _len perms
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          let (
                                                                            (
                                                                              perm (
                                                                                append (
                                                                                  cond (
                                                                                    (
                                                                                      string? perms
                                                                                    )
                                                                                     (
                                                                                      _substring perms j (
                                                                                        + j 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? perms
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref perms j
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref perms j
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _list n
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              set! result (
                                                                                append result (
                                                                                  _list perm
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
                                                          set! current (
                                                            rotate_left current
                                                          )
                                                        )
                                                         (
                                                          set! count (
                                                            + count 1
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
    )
     (
      define (
        swap xs i j
      )
       (
        call/cc (
          lambda (
            ret12
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
                    k 0
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
                                  < k (
                                    _len xs
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? k i
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              list-ref xs j
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      if (
                                        equal? k j
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
                                      )
                                       (
                                        begin (
                                          set! res (
                                            append res (
                                              _list (
                                                list-ref xs k
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    set! k (
                                      + k 1
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
                    ret12 res
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
        permute_backtrack_helper nums start output
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            begin (
              if (
                equal? start (
                  - (
                    _len nums
                  )
                   1
                )
              )
               (
                begin (
                  ret15 (
                    append output (
                      _list nums
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
                  i start
                )
              )
               (
                begin (
                  let (
                    (
                      res output
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
                                      _len nums
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          swapped (
                                            swap nums start i
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          set! res (
                                            permute_backtrack_helper swapped (
                                              + start 1
                                            )
                                             res
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
                      ret15 res
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
        permute_backtrack nums
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            let (
              (
                output (
                  _list
                )
              )
            )
             (
              begin (
                ret18 (
                  permute_backtrack_helper nums 0 output
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
              permute_recursive (
                _list 1 2 3
              )
            )
          )
        )
         (
          to-str-space (
            permute_recursive (
              _list 1 2 3
            )
          )
        )
         (
          to-str (
            to-str-space (
              permute_recursive (
                _list 1 2 3
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
              permute_backtrack (
                _list 1 2 3
              )
            )
          )
        )
         (
          to-str-space (
            permute_backtrack (
              _list 1 2 3
            )
          )
        )
         (
          to-str (
            to-str-space (
              permute_backtrack (
                _list 1 2 3
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
