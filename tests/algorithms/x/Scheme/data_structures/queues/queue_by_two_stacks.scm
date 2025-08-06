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
      start18 (
        current-jiffy
      )
    )
     (
      jps21 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        new_queue items
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
                  cons "stack1" items
                )
                 (
                  cons "stack2" (
                    _list
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
        len_queue q
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            ret2 (
              + (
                _len (
                  hash-table-ref q "stack1"
                )
              )
               (
                _len (
                  hash-table-ref q "stack2"
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        str_queue q
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            let (
              (
                items (
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
                        _len (
                          hash-table-ref q "stack2"
                        )
                      )
                       1
                    )
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
                                  >= i 0
                                )
                                 (
                                  begin (
                                    set! items (
                                      append items (
                                        _list (
                                          list-ref (
                                            hash-table-ref q "stack2"
                                          )
                                           i
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
                    let (
                      (
                        j 0
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
                                      < j (
                                        _len (
                                          hash-table-ref q "stack1"
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! items (
                                          append items (
                                            _list (
                                              list-ref (
                                                hash-table-ref q "stack1"
                                              )
                                               j
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
                        let (
                          (
                            s "Queue(("
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
                                              < k (
                                                _len items
                                              )
                                            )
                                             (
                                              begin (
                                                set! s (
                                                  string-append s (
                                                    to-str-space (
                                                      list-ref items k
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                if (
                                                  < k (
                                                    - (
                                                      _len items
                                                    )
                                                     1
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! s (
                                                      string-append s ", "
                                                    )
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
                                set! s (
                                  string-append s "))"
                                )
                              )
                               (
                                ret3 s
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
        put q item
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                s1 (
                  hash-table-ref q "stack1"
                )
              )
            )
             (
              begin (
                set! s1 (
                  append s1 (
                    _list item
                  )
                )
              )
               (
                ret10 (
                  alist->hash-table (
                    _list (
                      cons "stack1" s1
                    )
                     (
                      cons "stack2" (
                        hash-table-ref q "stack2"
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
            ret11
          )
           (
            let (
              (
                s1 (
                  hash-table-ref q "stack1"
                )
              )
            )
             (
              begin (
                let (
                  (
                    s2 (
                      hash-table-ref q "stack2"
                    )
                  )
                )
                 (
                  begin (
                    if (
                      equal? (
                        _len s2
                      )
                       0
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
                                      > (
                                        _len s1
                                      )
                                       0
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            idx (
                                              - (
                                                _len s1
                                              )
                                               1
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                v (
                                                  list-ref s1 idx
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    new_s1 (
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
                                                                      < i idx
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! new_s1 (
                                                                          append new_s1 (
                                                                            _list (
                                                                              list-ref s1 i
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
                                                        set! s1 new_s1
                                                      )
                                                       (
                                                        set! s2 (
                                                          append s2 (
                                                            _list v
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
                    )
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
                    if (
                      equal? (
                        _len s2
                      )
                       0
                    )
                     (
                      begin (
                        panic "Queue is empty"
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
                        idx2 (
                          - (
                            _len s2
                          )
                           1
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            value (
                              list-ref s2 idx2
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                new_s2 (
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
                                                  < j idx2
                                                )
                                                 (
                                                  begin (
                                                    set! new_s2 (
                                                      append new_s2 (
                                                        _list (
                                                          list-ref s2 j
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
                                    set! s2 new_s2
                                  )
                                   (
                                    ret11 (
                                      alist->hash-table (
                                        _list (
                                          cons "queue" (
                                            alist->hash-table (
                                              _list (
                                                cons "stack1" s1
                                              )
                                               (
                                                cons "stack2" s2
                                              )
                                            )
                                          )
                                        )
                                         (
                                          cons "value" value
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
      let (
        (
          q (
            new_queue (
              _list 10 20 30
            )
          )
        )
      )
       (
        begin (
          let (
            (
              r1 (
                get q
              )
            )
          )
           (
            begin (
              set! q (
                hash-table-ref r1 "queue"
              )
            )
             (
              _display (
                if (
                  string? (
                    hash-table-ref r1 "value"
                  )
                )
                 (
                  hash-table-ref r1 "value"
                )
                 (
                  to-str (
                    hash-table-ref r1 "value"
                  )
                )
              )
            )
             (
              newline
            )
             (
              set! q (
                put q 40
              )
            )
             (
              let (
                (
                  r2 (
                    get q
                  )
                )
              )
               (
                begin (
                  set! q (
                    hash-table-ref r2 "queue"
                  )
                )
                 (
                  _display (
                    if (
                      string? (
                        hash-table-ref r2 "value"
                      )
                    )
                     (
                      hash-table-ref r2 "value"
                    )
                     (
                      to-str (
                        hash-table-ref r2 "value"
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
                      r3 (
                        get q
                      )
                    )
                  )
                   (
                    begin (
                      set! q (
                        hash-table-ref r3 "queue"
                      )
                    )
                     (
                      _display (
                        if (
                          string? (
                            hash-table-ref r3 "value"
                          )
                        )
                         (
                          hash-table-ref r3 "value"
                        )
                         (
                          to-str (
                            hash-table-ref r3 "value"
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
                            len_queue q
                          )
                        )
                         (
                          len_queue q
                        )
                         (
                          to-str (
                            len_queue q
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
                          r4 (
                            get q
                          )
                        )
                      )
                       (
                        begin (
                          set! q (
                            hash-table-ref r4 "queue"
                          )
                        )
                         (
                          _display (
                            if (
                              string? (
                                hash-table-ref r4 "value"
                              )
                            )
                             (
                              hash-table-ref r4 "value"
                            )
                             (
                              to-str (
                                hash-table-ref r4 "value"
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
     (
      let (
        (
          end19 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur20 (
              quotient (
                * (
                  - end19 start18
                )
                 1000000
              )
               jps21
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur20
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
