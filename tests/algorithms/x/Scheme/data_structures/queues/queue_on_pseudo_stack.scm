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
        empty_queue
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              alist->hash-table (
                _list (
                  cons "stack" (
                    _list
                  )
                )
                 (
                  cons "length" 0
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        put q item
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                s (
                  append (
                    hash-table-ref q "stack"
                  )
                   (
                    _list item
                  )
                )
              )
            )
             (
              begin (
                ret2 (
                  alist->hash-table (
                    _list (
                      cons "stack" s
                    )
                     (
                      cons "length" (
                        + (
                          hash-table-ref q "length"
                        )
                         1
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
        drop_first xs
      )
       (
        call/cc (
          lambda (
            ret3
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
                    ret3 res
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
        drop_last xs
      )
       (
        call/cc (
          lambda (
            ret6
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
                    ret6 res
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
        rotate q rotation
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                s (
                  hash-table-ref q "stack"
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
                                  and (
                                    < i rotation
                                  )
                                   (
                                    > (
                                      _len s
                                    )
                                     0
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        temp (
                                          list-ref s 0
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! s (
                                          drop_first s
                                        )
                                      )
                                       (
                                        set! s (
                                          append s (
                                            _list temp
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
                    ret9 (
                      alist->hash-table (
                        _list (
                          cons "stack" s
                        )
                         (
                          cons "length" (
                            hash-table-ref q "length"
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
        get q
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            begin (
              if (
                equal? (
                  hash-table-ref q "length"
                )
                 0
              )
               (
                begin (
                  panic "queue empty"
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
                  q1 (
                    rotate q 1
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      v (
                        cond (
                          (
                            string? (
                              hash-table-ref q1 "stack"
                            )
                          )
                           (
                            _substring (
                              hash-table-ref q1 "stack"
                            )
                             (
                              - (
                                hash-table-ref q1 "length"
                              )
                               1
                            )
                             (
                              + (
                                - (
                                  hash-table-ref q1 "length"
                                )
                                 1
                              )
                               1
                            )
                          )
                        )
                         (
                          (
                            hash-table? (
                              hash-table-ref q1 "stack"
                            )
                          )
                           (
                            hash-table-ref (
                              hash-table-ref q1 "stack"
                            )
                             (
                              - (
                                hash-table-ref q1 "length"
                              )
                               1
                            )
                          )
                        )
                         (
                          else (
                            list-ref (
                              hash-table-ref q1 "stack"
                            )
                             (
                              - (
                                hash-table-ref q1 "length"
                              )
                               1
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
                          s (
                            drop_last (
                              hash-table-ref q1 "stack"
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              q2 (
                                alist->hash-table (
                                  _list (
                                    cons "stack" s
                                  )
                                   (
                                    cons "length" (
                                      hash-table-ref q1 "length"
                                    )
                                  )
                                )
                              )
                            )
                          )
                           (
                            begin (
                              set! q2 (
                                rotate q2 (
                                  - (
                                    hash-table-ref q2 "length"
                                  )
                                   1
                                )
                              )
                            )
                             (
                              set! q2 (
                                alist->hash-table (
                                  _list (
                                    cons "stack" (
                                      hash-table-ref q2 "stack"
                                    )
                                  )
                                   (
                                    cons "length" (
                                      - (
                                        hash-table-ref q2 "length"
                                      )
                                       1
                                    )
                                  )
                                )
                              )
                            )
                             (
                              ret12 (
                                alist->hash-table (
                                  _list (
                                    cons "queue" q2
                                  )
                                   (
                                    cons "value" v
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
        front q
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            let (
              (
                r (
                  get q
                )
              )
            )
             (
              begin (
                let (
                  (
                    q2 (
                      put (
                        hash-table-ref r "queue"
                      )
                       (
                        hash-table-ref r "value"
                      )
                    )
                  )
                )
                 (
                  begin (
                    set! q2 (
                      rotate q2 (
                        - (
                          hash-table-ref q2 "length"
                        )
                         1
                      )
                    )
                  )
                   (
                    ret13 (
                      alist->hash-table (
                        _list (
                          cons "queue" q2
                        )
                         (
                          cons "value" (
                            hash-table-ref r "value"
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
        size q
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            ret14 (
              hash-table-ref q "length"
            )
          )
        )
      )
    )
     (
      define (
        to_string q
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            let (
              (
                s "<"
              )
            )
             (
              begin (
                if (
                  > (
                    hash-table-ref q "length"
                  )
                   0
                )
                 (
                  begin (
                    set! s (
                      string-append s (
                        to-str-space (
                          list-ref (
                            hash-table-ref q "stack"
                          )
                           0
                        )
                      )
                    )
                  )
                   (
                    let (
                      (
                        i 1
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
                                        hash-table-ref q "length"
                                      )
                                    )
                                     (
                                      begin (
                                        set! s (
                                          string-append (
                                            string-append s ", "
                                          )
                                           (
                                            to-str-space (
                                              list-ref (
                                                hash-table-ref q "stack"
                                              )
                                               i
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
                    )
                  )
                )
                 (
                  quote (
                    
                  )
                )
              )
               (
                set! s (
                  string-append s ">"
                )
              )
               (
                ret15 s
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
            ret18
          )
           (
            let (
              (
                q (
                  empty_queue
                )
              )
            )
             (
              begin (
                set! q (
                  put q 1
                )
              )
               (
                set! q (
                  put q 2
                )
              )
               (
                set! q (
                  put q 3
                )
              )
               (
                _display (
                  if (
                    string? (
                      to_string q
                    )
                  )
                   (
                    to_string q
                  )
                   (
                    to-str (
                      to_string q
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
                    g (
                      get q
                    )
                  )
                )
                 (
                  begin (
                    set! q (
                      hash-table-ref g "queue"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref g "value"
                        )
                      )
                       (
                        hash-table-ref g "value"
                      )
                       (
                        to-str (
                          hash-table-ref g "value"
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
                          to_string q
                        )
                      )
                       (
                        to_string q
                      )
                       (
                        to-str (
                          to_string q
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
                        f (
                          front q
                        )
                      )
                    )
                     (
                      begin (
                        set! q (
                          hash-table-ref f "queue"
                        )
                      )
                       (
                        _display (
                          if (
                            string? (
                              hash-table-ref f "value"
                            )
                          )
                           (
                            hash-table-ref f "value"
                          )
                           (
                            to-str (
                              hash-table-ref f "value"
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
                              to_string q
                            )
                          )
                           (
                            to_string q
                          )
                           (
                            to-str (
                              to_string q
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
                              size q
                            )
                          )
                           (
                            size q
                          )
                           (
                            to-str (
                              size q
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
     (
      main
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
