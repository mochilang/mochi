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
      start27 (
        current-jiffy
      )
    )
     (
      jps30 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        panic msg
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              _display (
                if (
                  string? msg
                )
                 msg (
                  to-str msg
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
     (
      define (
        fpq_new
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            ret2 (
              alist->hash-table (
                _list (
                  cons "queues" (
                    _list (
                      _list
                    )
                     (
                      _list
                    )
                     (
                      _list
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
        fpq_enqueue fpq priority data
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            begin (
              if (
                or (
                  < priority 0
                )
                 (
                  >= priority (
                    _len (
                      hash-table-ref fpq "queues"
                    )
                  )
                )
              )
               (
                begin (
                  panic "Valid priorities are 0, 1, and 2"
                )
                 (
                  ret3 fpq
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                >= (
                  _len (
                    list-ref (
                      hash-table-ref fpq "queues"
                    )
                     priority
                  )
                )
                 100
              )
               (
                begin (
                  panic "Maximum queue size is 100"
                )
                 (
                  ret3 fpq
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
                  qs (
                    hash-table-ref fpq "queues"
                  )
                )
              )
               (
                begin (
                  list-set! qs priority (
                    append (
                      list-ref qs priority
                    )
                     (
                      _list data
                    )
                  )
                )
                 (
                  hash-table-set! fpq "queues" qs
                )
                 (
                  ret3 fpq
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        fpq_dequeue fpq
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                qs (
                  hash-table-ref fpq "queues"
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
                                    _len qs
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        q (
                                          list-ref qs i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          > (
                                            _len q
                                          )
                                           0
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                val (
                                                  list-ref q 0
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    new_q (
                                                      _list
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        j 1
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
                                                                      < j (
                                                                        _len q
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! new_q (
                                                                          append new_q (
                                                                            _list (
                                                                              list-ref q j
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
                                                        list-set! qs i new_q
                                                      )
                                                       (
                                                        hash-table-set! fpq "queues" qs
                                                      )
                                                       (
                                                        ret4 (
                                                          alist->hash-table (
                                                            _list (
                                                              cons "queue" fpq
                                                            )
                                                             (
                                                              cons "value" val
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
                    panic "All queues are empty"
                  )
                   (
                    ret4 (
                      alist->hash-table (
                        _list (
                          cons "queue" fpq
                        )
                         (
                          cons "value" 0
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
        fpq_to_string fpq
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                lines (
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
                                    _len (
                                      hash-table-ref fpq "queues"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        q_str "["
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            q (
                                              list-ref (
                                                hash-table-ref fpq "queues"
                                              )
                                               i
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
                                                              < j (
                                                                _len q
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  > j 0
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! q_str (
                                                                      string-append q_str ", "
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  quote (
                                                                    
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! q_str (
                                                                  string-append q_str (
                                                                    to-str-space (
                                                                      list-ref q j
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
                                                set! q_str (
                                                  string-append q_str "]"
                                                )
                                              )
                                               (
                                                set! lines (
                                                  append lines (
                                                    _list (
                                                      string-append (
                                                        string-append (
                                                          string-append "Priority " (
                                                            to-str-space i
                                                          )
                                                        )
                                                         ": "
                                                      )
                                                       q_str
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
                        res ""
                      )
                    )
                     (
                      begin (
                        set! i 0
                      )
                       (
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
                                      < i (
                                        _len lines
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          > i 0
                                        )
                                         (
                                          begin (
                                            set! res (
                                              string-append res "\n"
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
                                            list-ref lines i
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
     (
      define (
        epq_new
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            ret16 (
              alist->hash-table (
                _list (
                  cons "queue" (
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
        epq_enqueue epq data
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            begin (
              if (
                >= (
                  _len (
                    hash-table-ref epq "queue"
                  )
                )
                 100
              )
               (
                begin (
                  panic "Maximum queue size is 100"
                )
                 (
                  ret17 epq
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              hash-table-set! epq "queue" (
                append (
                  hash-table-ref epq "queue"
                )
                 (
                  _list data
                )
              )
            )
             (
              ret17 epq
            )
          )
        )
      )
    )
     (
      define (
        epq_dequeue epq
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            begin (
              if (
                equal? (
                  _len (
                    hash-table-ref epq "queue"
                  )
                )
                 0
              )
               (
                begin (
                  panic "The queue is empty"
                )
                 (
                  ret18 (
                    alist->hash-table (
                      _list (
                        cons "queue" epq
                      )
                       (
                        cons "value" 0
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
                  min_val (
                    list-ref (
                      hash-table-ref epq "queue"
                    )
                     0
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      idx 0
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
                              break20
                            )
                             (
                              letrec (
                                (
                                  loop19 (
                                    lambda (
                                      
                                    )
                                     (
                                      if (
                                        < i (
                                          _len (
                                            hash-table-ref epq "queue"
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              v (
                                                list-ref (
                                                  hash-table-ref epq "queue"
                                                )
                                                 i
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                < v min_val
                                              )
                                               (
                                                begin (
                                                  set! min_val v
                                                )
                                                 (
                                                  set! idx i
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
                                          loop19
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
                                loop19
                              )
                            )
                          )
                        )
                         (
                          let (
                            (
                              new_q (
                                _list
                              )
                            )
                          )
                           (
                            begin (
                              set! i 0
                            )
                             (
                              call/cc (
                                lambda (
                                  break22
                                )
                                 (
                                  letrec (
                                    (
                                      loop21 (
                                        lambda (
                                          
                                        )
                                         (
                                          if (
                                            < i (
                                              _len (
                                                hash-table-ref epq "queue"
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                not (
                                                  equal? i idx
                                                )
                                              )
                                               (
                                                begin (
                                                  set! new_q (
                                                    append new_q (
                                                      _list (
                                                        list-ref (
                                                          hash-table-ref epq "queue"
                                                        )
                                                         i
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
                                                + i 1
                                              )
                                            )
                                             (
                                              loop21
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
                                    loop21
                                  )
                                )
                              )
                            )
                             (
                              hash-table-set! epq "queue" new_q
                            )
                             (
                              ret18 (
                                alist->hash-table (
                                  _list (
                                    cons "queue" epq
                                  )
                                   (
                                    cons "value" min_val
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
        epq_to_string epq
      )
       (
        call/cc (
          lambda (
            ret23
          )
           (
            ret23 (
              to-str-space (
                hash-table-ref epq "queue"
              )
            )
          )
        )
      )
    )
     (
      define (
        fixed_priority_queue
      )
       (
        call/cc (
          lambda (
            ret24
          )
           (
            let (
              (
                fpq (
                  fpq_new
                )
              )
            )
             (
              begin (
                set! fpq (
                  fpq_enqueue fpq 0 10
                )
              )
               (
                set! fpq (
                  fpq_enqueue fpq 1 70
                )
              )
               (
                set! fpq (
                  fpq_enqueue fpq 0 100
                )
              )
               (
                set! fpq (
                  fpq_enqueue fpq 2 1
                )
              )
               (
                set! fpq (
                  fpq_enqueue fpq 2 5
                )
              )
               (
                set! fpq (
                  fpq_enqueue fpq 1 7
                )
              )
               (
                set! fpq (
                  fpq_enqueue fpq 2 4
                )
              )
               (
                set! fpq (
                  fpq_enqueue fpq 1 64
                )
              )
               (
                set! fpq (
                  fpq_enqueue fpq 0 128
                )
              )
               (
                _display (
                  if (
                    string? (
                      fpq_to_string fpq
                    )
                  )
                   (
                    fpq_to_string fpq
                  )
                   (
                    to-str (
                      fpq_to_string fpq
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
                    res (
                      fpq_dequeue fpq
                    )
                  )
                )
                 (
                  begin (
                    set! fpq (
                      hash-table-ref res "queue"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      fpq_dequeue fpq
                    )
                  )
                   (
                    set! fpq (
                      hash-table-ref res "queue"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      fpq_dequeue fpq
                    )
                  )
                   (
                    set! fpq (
                      hash-table-ref res "queue"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      fpq_dequeue fpq
                    )
                  )
                   (
                    set! fpq (
                      hash-table-ref res "queue"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      fpq_dequeue fpq
                    )
                  )
                   (
                    set! fpq (
                      hash-table-ref res "queue"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
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
                          fpq_to_string fpq
                        )
                      )
                       (
                        fpq_to_string fpq
                      )
                       (
                        to-str (
                          fpq_to_string fpq
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      fpq_dequeue fpq
                    )
                  )
                   (
                    set! fpq (
                      hash-table-ref res "queue"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      fpq_dequeue fpq
                    )
                  )
                   (
                    set! fpq (
                      hash-table-ref res "queue"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      fpq_dequeue fpq
                    )
                  )
                   (
                    set! fpq (
                      hash-table-ref res "queue"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      fpq_dequeue fpq
                    )
                  )
                   (
                    set! fpq (
                      hash-table-ref res "queue"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      fpq_dequeue fpq
                    )
                  )
                   (
                    set! fpq (
                      hash-table-ref res "queue"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
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
     (
      define (
        element_priority_queue
      )
       (
        call/cc (
          lambda (
            ret25
          )
           (
            let (
              (
                epq (
                  epq_new
                )
              )
            )
             (
              begin (
                set! epq (
                  epq_enqueue epq 10
                )
              )
               (
                set! epq (
                  epq_enqueue epq 70
                )
              )
               (
                set! epq (
                  epq_enqueue epq 100
                )
              )
               (
                set! epq (
                  epq_enqueue epq 1
                )
              )
               (
                set! epq (
                  epq_enqueue epq 5
                )
              )
               (
                set! epq (
                  epq_enqueue epq 7
                )
              )
               (
                set! epq (
                  epq_enqueue epq 4
                )
              )
               (
                set! epq (
                  epq_enqueue epq 64
                )
              )
               (
                set! epq (
                  epq_enqueue epq 128
                )
              )
               (
                _display (
                  if (
                    string? (
                      epq_to_string epq
                    )
                  )
                   (
                    epq_to_string epq
                  )
                   (
                    to-str (
                      epq_to_string epq
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
                    res (
                      epq_dequeue epq
                    )
                  )
                )
                 (
                  begin (
                    set! epq (
                      hash-table-ref res "queue"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      epq_dequeue epq
                    )
                  )
                   (
                    set! epq (
                      hash-table-ref res "queue"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      epq_dequeue epq
                    )
                  )
                   (
                    set! epq (
                      hash-table-ref res "queue"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      epq_dequeue epq
                    )
                  )
                   (
                    set! epq (
                      hash-table-ref res "queue"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      epq_dequeue epq
                    )
                  )
                   (
                    set! epq (
                      hash-table-ref res "queue"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
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
                          epq_to_string epq
                        )
                      )
                       (
                        epq_to_string epq
                      )
                       (
                        to-str (
                          epq_to_string epq
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      epq_dequeue epq
                    )
                  )
                   (
                    set! epq (
                      hash-table-ref res "queue"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      epq_dequeue epq
                    )
                  )
                   (
                    set! epq (
                      hash-table-ref res "queue"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      epq_dequeue epq
                    )
                  )
                   (
                    set! epq (
                      hash-table-ref res "queue"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      epq_dequeue epq
                    )
                  )
                   (
                    set! epq (
                      hash-table-ref res "queue"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      epq_dequeue epq
                    )
                  )
                   (
                    set! epq (
                      hash-table-ref res "queue"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
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
     (
      define (
        main
      )
       (
        call/cc (
          lambda (
            ret26
          )
           (
            begin (
              fixed_priority_queue
            )
             (
              element_priority_queue
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
          end28 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur29 (
              quotient (
                * (
                  - end28 start27
                )
                 1000000
              )
               jps30
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur29
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
