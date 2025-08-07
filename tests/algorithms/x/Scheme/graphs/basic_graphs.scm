;; Generated on 2025-08-07 08:56 +0700
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
      start73 (
        current-jiffy
      )
    )
     (
      jps76 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        dfs g s
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                visited (
                  alist->hash-table (
                    _list
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    stack (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    hash-table-set! visited s #t
                  )
                   (
                    set! stack (
                      append stack (
                        _list s
                      )
                    )
                  )
                   (
                    _display (
                      if (
                        string? s
                      )
                       s (
                        to-str s
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
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
                                  > (
                                    _len stack
                                  )
                                   0
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        u (
                                          list-ref stack (
                                            - (
                                              _len stack
                                            )
                                             1
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            found #f
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
                                                        xs
                                                      )
                                                       (
                                                        if (
                                                          null? xs
                                                        )
                                                         (
                                                          quote (
                                                            
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                v (
                                                                  car xs
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  not (
                                                                    cond (
                                                                      (
                                                                        string? visited
                                                                      )
                                                                       (
                                                                        if (
                                                                          string-contains visited v
                                                                        )
                                                                         #t #f
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? visited
                                                                      )
                                                                       (
                                                                        if (
                                                                          hash-table-exists? visited v
                                                                        )
                                                                         #t #f
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        if (
                                                                          member v visited
                                                                        )
                                                                         #t #f
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    hash-table-set! visited v #t
                                                                  )
                                                                   (
                                                                    set! stack (
                                                                      append stack (
                                                                        _list v
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    _display (
                                                                      if (
                                                                        string? v
                                                                      )
                                                                       v (
                                                                        to-str v
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    newline
                                                                  )
                                                                   (
                                                                    set! found #t
                                                                  )
                                                                   (
                                                                    break5 (
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
                                                            )
                                                          )
                                                           (
                                                            loop4 (
                                                              cdr xs
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  loop4 (
                                                    hash-table-ref/default g u (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            if (
                                              not found
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
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        bfs g s
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                visited (
                  alist->hash-table (
                    _list
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    q (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    hash-table-set! visited s #t
                  )
                   (
                    set! q (
                      append q (
                        _list s
                      )
                    )
                  )
                   (
                    _display (
                      if (
                        string? s
                      )
                       s (
                        to-str s
                      )
                    )
                  )
                   (
                    newline
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
                                  > (
                                    _len q
                                  )
                                   0
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        u (
                                          list-ref q 0
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! q (
                                          slice q 1 (
                                            _len q
                                          )
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
                                                    xs
                                                  )
                                                   (
                                                    if (
                                                      null? xs
                                                    )
                                                     (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            v (
                                                              car xs
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              not (
                                                                cond (
                                                                  (
                                                                    string? visited
                                                                  )
                                                                   (
                                                                    if (
                                                                      string-contains visited v
                                                                    )
                                                                     #t #f
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? visited
                                                                  )
                                                                   (
                                                                    if (
                                                                      hash-table-exists? visited v
                                                                    )
                                                                     #t #f
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    if (
                                                                      member v visited
                                                                    )
                                                                     #t #f
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                hash-table-set! visited v #t
                                                              )
                                                               (
                                                                set! q (
                                                                  append q (
                                                                    _list v
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                _display (
                                                                  if (
                                                                    string? v
                                                                  )
                                                                   v (
                                                                    to-str v
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                newline
                                                              )
                                                            )
                                                             (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        loop9 (
                                                          cdr xs
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              loop9 (
                                                hash-table-ref/default g u (
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
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        sort_ints a
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            let (
              (
                arr a
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
                                  < i (
                                    _len arr
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
                                                        - (
                                                          - (
                                                            _len arr
                                                          )
                                                           i
                                                        )
                                                         1
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          > (
                                                            list-ref arr j
                                                          )
                                                           (
                                                            list-ref arr (
                                                              + j 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                tmp (
                                                                  list-ref arr j
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                list-set! arr j (
                                                                  list-ref arr (
                                                                    + j 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                list-set! arr (
                                                                  + j 1
                                                                )
                                                                 tmp
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
                                        set! i (
                                          + i 1
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
                   (
                    ret11 arr
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
        dijkstra g s
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            let (
              (
                dist (
                  alist->hash-table (
                    _list
                  )
                )
              )
            )
             (
              begin (
                hash-table-set! dist s 0
              )
               (
                let (
                  (
                    path (
                      alist->hash-table (
                        _list
                      )
                    )
                  )
                )
                 (
                  begin (
                    hash-table-set! path s 0
                  )
                   (
                    let (
                      (
                        known (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            keys (
                              _list s
                            )
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
                                          < (
                                            _len known
                                          )
                                           (
                                            _len keys
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                mini 100000
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    u (
                                                      - 1
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
                                                                        _len keys
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            k (
                                                                              list-ref keys i
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                d (
                                                                                  hash-table-ref/default dist k (
                                                                                    quote (
                                                                                      
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                if (
                                                                                  and (
                                                                                    not (
                                                                                      cond (
                                                                                        (
                                                                                          string? known
                                                                                        )
                                                                                         (
                                                                                          if (
                                                                                            string-contains known k
                                                                                          )
                                                                                           #t #f
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? known
                                                                                        )
                                                                                         (
                                                                                          if (
                                                                                            hash-table-exists? known k
                                                                                          )
                                                                                           #t #f
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          if (
                                                                                            member k known
                                                                                          )
                                                                                           #t #f
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    < d mini
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! mini d
                                                                                  )
                                                                                   (
                                                                                    set! u k
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
                                                        set! known (
                                                          append known (
                                                            _list u
                                                          )
                                                        )
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
                                                                    xs
                                                                  )
                                                                   (
                                                                    if (
                                                                      null? xs
                                                                    )
                                                                     (
                                                                      quote (
                                                                        
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            e (
                                                                              car xs
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
                                                                                      string? e
                                                                                    )
                                                                                     (
                                                                                      _substring e 0 (
                                                                                        + 0 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? e
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref e 0
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref e 0
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    w (
                                                                                      cond (
                                                                                        (
                                                                                          string? e
                                                                                        )
                                                                                         (
                                                                                          _substring e 1 (
                                                                                            + 1 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? e
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref e 1
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref e 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    if (
                                                                                      not (
                                                                                        cond (
                                                                                          (
                                                                                            string? keys
                                                                                          )
                                                                                           (
                                                                                            if (
                                                                                              string-contains keys v
                                                                                            )
                                                                                             #t #f
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? keys
                                                                                          )
                                                                                           (
                                                                                            if (
                                                                                              hash-table-exists? keys v
                                                                                            )
                                                                                             #t #f
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            if (
                                                                                              member v keys
                                                                                            )
                                                                                             #t #f
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! keys (
                                                                                          append keys (
                                                                                            _list v
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
                                                                                        alt (
                                                                                          _add (
                                                                                            hash-table-ref/default dist u (
                                                                                              quote (
                                                                                                
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           w
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            cur (
                                                                                              if (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? dist
                                                                                                  )
                                                                                                   (
                                                                                                    if (
                                                                                                      string-contains dist v
                                                                                                    )
                                                                                                     #t #f
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  (
                                                                                                    hash-table? dist
                                                                                                  )
                                                                                                   (
                                                                                                    if (
                                                                                                      hash-table-exists? dist v
                                                                                                    )
                                                                                                     #t #f
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    if (
                                                                                                      member v dist
                                                                                                    )
                                                                                                     #t #f
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref/default dist v (
                                                                                                  quote (
                                                                                                    
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               100000
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            if (
                                                                                              and (
                                                                                                not (
                                                                                                  cond (
                                                                                                    (
                                                                                                      string? known
                                                                                                    )
                                                                                                     (
                                                                                                      if (
                                                                                                        string-contains known v
                                                                                                      )
                                                                                                       #t #f
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    (
                                                                                                      hash-table? known
                                                                                                    )
                                                                                                     (
                                                                                                      if (
                                                                                                        hash-table-exists? known v
                                                                                                      )
                                                                                                       #t #f
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    else (
                                                                                                      if (
                                                                                                        member v known
                                                                                                      )
                                                                                                       #t #f
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _lt alt cur
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                hash-table-set! dist v alt
                                                                                              )
                                                                                               (
                                                                                                hash-table-set! path v u
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
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        loop21 (
                                                                          cdr xs
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              loop21 (
                                                                hash-table-ref/default g u (
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
                                              )
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
                           (
                            let (
                              (
                                ordered (
                                  sort_ints keys
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
                                    call/cc (
                                      lambda (
                                        break24
                                      )
                                       (
                                        letrec (
                                          (
                                            loop23 (
                                              lambda (
                                                
                                              )
                                               (
                                                if (
                                                  < idx (
                                                    _len ordered
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        k (
                                                          cond (
                                                            (
                                                              string? ordered
                                                            )
                                                             (
                                                              _substring ordered idx (
                                                                + idx 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? ordered
                                                            )
                                                             (
                                                              hash-table-ref ordered idx
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref ordered idx
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          not (
                                                            equal? k s
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            _display (
                                                              if (
                                                                string? (
                                                                  hash-table-ref/default dist k (
                                                                    quote (
                                                                      
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                hash-table-ref/default dist k (
                                                                  quote (
                                                                    
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                to-str (
                                                                  hash-table-ref/default dist k (
                                                                    quote (
                                                                      
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
                                                         (
                                                          quote (
                                                            
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! idx (
                                                          + idx 1
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    loop23
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
                                          loop23
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
        topo g n
      )
       (
        call/cc (
          lambda (
            ret25
          )
           (
            let (
              (
                ind (
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
                        break27
                      )
                       (
                        letrec (
                          (
                            loop26 (
                              lambda (
                                
                              )
                               (
                                if (
                                  <= i n
                                )
                                 (
                                  begin (
                                    set! ind (
                                      append ind (
                                        _list 0
                                      )
                                    )
                                  )
                                   (
                                    set! i (
                                      + i 1
                                    )
                                  )
                                   (
                                    loop26
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
                          loop26
                        )
                      )
                    )
                  )
                   (
                    let (
                      (
                        node 1
                      )
                    )
                     (
                      begin (
                        call/cc (
                          lambda (
                            break29
                          )
                           (
                            letrec (
                              (
                                loop28 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      <= node n
                                    )
                                     (
                                      begin (
                                        call/cc (
                                          lambda (
                                            break31
                                          )
                                           (
                                            letrec (
                                              (
                                                loop30 (
                                                  lambda (
                                                    xs
                                                  )
                                                   (
                                                    if (
                                                      null? xs
                                                    )
                                                     (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            v (
                                                              car xs
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            list-set! ind v (
                                                              + (
                                                                list-ref ind v
                                                              )
                                                               1
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        loop30 (
                                                          cdr xs
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              loop30 (
                                                hash-table-ref/default g node (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        set! node (
                                          + node 1
                                        )
                                      )
                                       (
                                        loop28
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
                              loop28
                            )
                          )
                        )
                      )
                       (
                        let (
                          (
                            q (
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
                                    break33
                                  )
                                   (
                                    letrec (
                                      (
                                        loop32 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              <= j n
                                            )
                                             (
                                              begin (
                                                if (
                                                  equal? (
                                                    list-ref ind j
                                                  )
                                                   0
                                                )
                                                 (
                                                  begin (
                                                    set! q (
                                                      append q (
                                                        _list j
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
                                                set! j (
                                                  + j 1
                                                )
                                              )
                                               (
                                                loop32
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
                                      loop32
                                    )
                                  )
                                )
                              )
                               (
                                call/cc (
                                  lambda (
                                    break35
                                  )
                                   (
                                    letrec (
                                      (
                                        loop34 (
                                          lambda (
                                            
                                          )
                                           (
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
                                                    v (
                                                      list-ref q 0
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! q (
                                                      slice q 1 (
                                                        _len q
                                                      )
                                                    )
                                                  )
                                                   (
                                                    _display (
                                                      if (
                                                        string? v
                                                      )
                                                       v (
                                                        to-str v
                                                      )
                                                    )
                                                  )
                                                   (
                                                    newline
                                                  )
                                                   (
                                                    call/cc (
                                                      lambda (
                                                        break37
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop36 (
                                                              lambda (
                                                                xs
                                                              )
                                                               (
                                                                if (
                                                                  null? xs
                                                                )
                                                                 (
                                                                  quote (
                                                                    
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        w (
                                                                          car xs
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        list-set! ind w (
                                                                          - (
                                                                            list-ref ind w
                                                                          )
                                                                           1
                                                                        )
                                                                      )
                                                                       (
                                                                        if (
                                                                          equal? (
                                                                            list-ref ind w
                                                                          )
                                                                           0
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! q (
                                                                              append q (
                                                                                _list w
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
                                                                   (
                                                                    loop36 (
                                                                      cdr xs
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          loop36 (
                                                            hash-table-ref/default g v (
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
                                               (
                                                loop34
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
                                      loop34
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
        floyd a
      )
       (
        call/cc (
          lambda (
            ret38
          )
           (
            let (
              (
                n (
                  _len a
                )
              )
            )
             (
              begin (
                let (
                  (
                    dist (
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
                            break40
                          )
                           (
                            letrec (
                              (
                                loop39 (
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
                                                    break42
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop41 (
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
                                                                    _list (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            list-ref a i
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref a i
                                                                          )
                                                                           j (
                                                                            + j 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref a i
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref a i
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            list-ref a i
                                                                          )
                                                                           j
                                                                        )
                                                                      )
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
                                                                loop41
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
                                                      loop41
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! dist (
                                                  append dist (
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
                                        loop39
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
                              loop39
                            )
                          )
                        )
                      )
                       (
                        let (
                          (
                            k 0
                          )
                        )
                         (
                          begin (
                            call/cc (
                              lambda (
                                break44
                              )
                               (
                                letrec (
                                  (
                                    loop43 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < k n
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                ii 0
                                              )
                                            )
                                             (
                                              begin (
                                                call/cc (
                                                  lambda (
                                                    break46
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop45 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              < ii n
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    jj 0
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    call/cc (
                                                                      lambda (
                                                                        break48
                                                                      )
                                                                       (
                                                                        letrec (
                                                                          (
                                                                            loop47 (
                                                                              lambda (
                                                                                
                                                                              )
                                                                               (
                                                                                if (
                                                                                  < jj n
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    if (
                                                                                      > (
                                                                                        cond (
                                                                                          (
                                                                                            string? (
                                                                                              list-ref dist ii
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            _substring (
                                                                                              list-ref dist ii
                                                                                            )
                                                                                             jj (
                                                                                              + jj 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? (
                                                                                              list-ref dist ii
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref (
                                                                                              list-ref dist ii
                                                                                            )
                                                                                             jj
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref (
                                                                                              list-ref dist ii
                                                                                            )
                                                                                             jj
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        + (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                list-ref dist ii
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref dist ii
                                                                                              )
                                                                                               k (
                                                                                                + k 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref dist ii
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref dist ii
                                                                                              )
                                                                                               k
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref dist ii
                                                                                              )
                                                                                               k
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                list-ref dist k
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref dist k
                                                                                              )
                                                                                               jj (
                                                                                                + jj 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref dist k
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref dist k
                                                                                              )
                                                                                               jj
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref dist k
                                                                                              )
                                                                                               jj
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        list-set! (
                                                                                          list-ref dist ii
                                                                                        )
                                                                                         jj (
                                                                                          + (
                                                                                            cond (
                                                                                              (
                                                                                                string? (
                                                                                                  list-ref dist ii
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _substring (
                                                                                                  list-ref dist ii
                                                                                                )
                                                                                                 k (
                                                                                                  + k 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? (
                                                                                                  list-ref dist ii
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref (
                                                                                                  list-ref dist ii
                                                                                                )
                                                                                                 k
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref (
                                                                                                  list-ref dist ii
                                                                                                )
                                                                                                 k
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            cond (
                                                                                              (
                                                                                                string? (
                                                                                                  list-ref dist k
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _substring (
                                                                                                  list-ref dist k
                                                                                                )
                                                                                                 jj (
                                                                                                  + jj 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? (
                                                                                                  list-ref dist k
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref (
                                                                                                  list-ref dist k
                                                                                                )
                                                                                                 jj
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref (
                                                                                                  list-ref dist k
                                                                                                )
                                                                                                 jj
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
                                                                                    set! jj (
                                                                                      + jj 1
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    loop47
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
                                                                          loop47
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! ii (
                                                                      + ii 1
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                loop45
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
                                                      loop45
                                                    )
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
                                           (
                                            loop43
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
                                  loop43
                                )
                              )
                            )
                          )
                           (
                            _display (
                              if (
                                string? dist
                              )
                               dist (
                                to-str dist
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
     (
      define (
        prim g s n
      )
       (
        call/cc (
          lambda (
            ret49
          )
           (
            let (
              (
                dist (
                  alist->hash-table (
                    _list
                  )
                )
              )
            )
             (
              begin (
                hash-table-set! dist s 0
              )
               (
                let (
                  (
                    known (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        keys (
                          _list s
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            total 0
                          )
                        )
                         (
                          begin (
                            call/cc (
                              lambda (
                                break51
                              )
                               (
                                letrec (
                                  (
                                    loop50 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < (
                                            _len known
                                          )
                                           n
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                mini 100000
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    u (
                                                      - 1
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
                                                            break53
                                                          )
                                                           (
                                                            letrec (
                                                              (
                                                                loop52 (
                                                                  lambda (
                                                                    
                                                                  )
                                                                   (
                                                                    if (
                                                                      < i (
                                                                        _len keys
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            k (
                                                                              list-ref keys i
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                d (
                                                                                  hash-table-ref/default dist k (
                                                                                    quote (
                                                                                      
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                if (
                                                                                  and (
                                                                                    not (
                                                                                      cond (
                                                                                        (
                                                                                          string? known
                                                                                        )
                                                                                         (
                                                                                          if (
                                                                                            string-contains known k
                                                                                          )
                                                                                           #t #f
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? known
                                                                                        )
                                                                                         (
                                                                                          if (
                                                                                            hash-table-exists? known k
                                                                                          )
                                                                                           #t #f
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          if (
                                                                                            member k known
                                                                                          )
                                                                                           #t #f
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    < d mini
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! mini d
                                                                                  )
                                                                                   (
                                                                                    set! u k
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
                                                                        )
                                                                      )
                                                                       (
                                                                        loop52
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
                                                              loop52
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! known (
                                                          append known (
                                                            _list u
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! total (
                                                          + total mini
                                                        )
                                                      )
                                                       (
                                                        call/cc (
                                                          lambda (
                                                            break55
                                                          )
                                                           (
                                                            letrec (
                                                              (
                                                                loop54 (
                                                                  lambda (
                                                                    xs
                                                                  )
                                                                   (
                                                                    if (
                                                                      null? xs
                                                                    )
                                                                     (
                                                                      quote (
                                                                        
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            e (
                                                                              car xs
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
                                                                                      string? e
                                                                                    )
                                                                                     (
                                                                                      _substring e 0 (
                                                                                        + 0 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? e
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref e 0
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref e 0
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    w (
                                                                                      cond (
                                                                                        (
                                                                                          string? e
                                                                                        )
                                                                                         (
                                                                                          _substring e 1 (
                                                                                            + 1 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? e
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref e 1
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref e 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    if (
                                                                                      not (
                                                                                        cond (
                                                                                          (
                                                                                            string? keys
                                                                                          )
                                                                                           (
                                                                                            if (
                                                                                              string-contains keys v
                                                                                            )
                                                                                             #t #f
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? keys
                                                                                          )
                                                                                           (
                                                                                            if (
                                                                                              hash-table-exists? keys v
                                                                                            )
                                                                                             #t #f
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            if (
                                                                                              member v keys
                                                                                            )
                                                                                             #t #f
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! keys (
                                                                                          append keys (
                                                                                            _list v
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
                                                                                        cur (
                                                                                          if (
                                                                                            cond (
                                                                                              (
                                                                                                string? dist
                                                                                              )
                                                                                               (
                                                                                                if (
                                                                                                  string-contains dist v
                                                                                                )
                                                                                                 #t #f
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? dist
                                                                                              )
                                                                                               (
                                                                                                if (
                                                                                                  hash-table-exists? dist v
                                                                                                )
                                                                                                 #t #f
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                if (
                                                                                                  member v dist
                                                                                                )
                                                                                                 #t #f
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref/default dist v (
                                                                                              quote (
                                                                                                
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           100000
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        if (
                                                                                          and (
                                                                                            not (
                                                                                              cond (
                                                                                                (
                                                                                                  string? known
                                                                                                )
                                                                                                 (
                                                                                                  if (
                                                                                                    string-contains known v
                                                                                                  )
                                                                                                   #t #f
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                (
                                                                                                  hash-table? known
                                                                                                )
                                                                                                 (
                                                                                                  if (
                                                                                                    hash-table-exists? known v
                                                                                                  )
                                                                                                   #t #f
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  if (
                                                                                                    member v known
                                                                                                  )
                                                                                                   #t #f
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            _lt w cur
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            hash-table-set! dist v w
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
                                                                      )
                                                                       (
                                                                        loop54 (
                                                                          cdr xs
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              loop54 (
                                                                hash-table-ref/default g u (
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
                                              )
                                            )
                                          )
                                           (
                                            loop50
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
                                  loop50
                                )
                              )
                            )
                          )
                           (
                            ret49 total
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
        sort_edges edges
      )
       (
        call/cc (
          lambda (
            ret56
          )
           (
            let (
              (
                es edges
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
                        break58
                      )
                       (
                        letrec (
                          (
                            loop57 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len es
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
                                            break60
                                          )
                                           (
                                            letrec (
                                              (
                                                loop59 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < j (
                                                        - (
                                                          - (
                                                            _len es
                                                          )
                                                           i
                                                        )
                                                         1
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          > (
                                                            cond (
                                                              (
                                                                string? (
                                                                  list-ref es j
                                                                )
                                                              )
                                                               (
                                                                _substring (
                                                                  list-ref es j
                                                                )
                                                                 2 (
                                                                  + 2 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? (
                                                                  list-ref es j
                                                                )
                                                              )
                                                               (
                                                                hash-table-ref (
                                                                  list-ref es j
                                                                )
                                                                 2
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref (
                                                                  list-ref es j
                                                                )
                                                                 2
                                                              )
                                                            )
                                                          )
                                                           (
                                                            cond (
                                                              (
                                                                string? (
                                                                  list-ref es (
                                                                    + j 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                _substring (
                                                                  list-ref es (
                                                                    + j 1
                                                                  )
                                                                )
                                                                 2 (
                                                                  + 2 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? (
                                                                  list-ref es (
                                                                    + j 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                hash-table-ref (
                                                                  list-ref es (
                                                                    + j 1
                                                                  )
                                                                )
                                                                 2
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref (
                                                                  list-ref es (
                                                                    + j 1
                                                                  )
                                                                )
                                                                 2
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                tmp (
                                                                  list-ref es j
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                list-set! es j (
                                                                  list-ref es (
                                                                    + j 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                list-set! es (
                                                                  + j 1
                                                                )
                                                                 tmp
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
                                                        set! j (
                                                          + j 1
                                                        )
                                                      )
                                                       (
                                                        loop59
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
                                              loop59
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
                                    loop57
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
                          loop57
                        )
                      )
                    )
                  )
                   (
                    ret56 es
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
        find_parent parent x
      )
       (
        call/cc (
          lambda (
            ret61
          )
           (
            let (
              (
                r x
              )
            )
             (
              begin (
                call/cc (
                  lambda (
                    break63
                  )
                   (
                    letrec (
                      (
                        loop62 (
                          lambda (
                            
                          )
                           (
                            if (
                              not (
                                equal? (
                                  list-ref parent r
                                )
                                 r
                              )
                            )
                             (
                              begin (
                                set! r (
                                  list-ref parent r
                                )
                              )
                               (
                                loop62
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
                      loop62
                    )
                  )
                )
              )
               (
                ret61 r
              )
            )
          )
        )
      )
    )
     (
      define (
        union_parent parent a b
      )
       (
        call/cc (
          lambda (
            ret64
          )
           (
            list-set! parent a b
          )
        )
      )
    )
     (
      define (
        kruskal edges n
      )
       (
        call/cc (
          lambda (
            ret65
          )
           (
            let (
              (
                es (
                  sort_edges edges
                )
              )
            )
             (
              begin (
                let (
                  (
                    parent (
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
                            break67
                          )
                           (
                            letrec (
                              (
                                loop66 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      <= i n
                                    )
                                     (
                                      begin (
                                        set! parent (
                                          append parent (
                                            _list i
                                          )
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
                                        )
                                      )
                                       (
                                        loop66
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
                              loop66
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
                            let (
                              (
                                count 0
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
                                    call/cc (
                                      lambda (
                                        break69
                                      )
                                       (
                                        letrec (
                                          (
                                            loop68 (
                                              lambda (
                                                
                                              )
                                               (
                                                if (
                                                  and (
                                                    < count (
                                                      - n 1
                                                    )
                                                  )
                                                   (
                                                    < idx (
                                                      _len es
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        e (
                                                          cond (
                                                            (
                                                              string? es
                                                            )
                                                             (
                                                              _substring es idx (
                                                                + idx 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? es
                                                            )
                                                             (
                                                              hash-table-ref es idx
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref es idx
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! idx (
                                                          + idx 1
                                                        )
                                                      )
                                                       (
                                                        let (
                                                          (
                                                            u (
                                                              cond (
                                                                (
                                                                  string? e
                                                                )
                                                                 (
                                                                  _substring e 0 (
                                                                    + 0 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                (
                                                                  hash-table? e
                                                                )
                                                                 (
                                                                  hash-table-ref e 0
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref e 0
                                                                )
                                                              )
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
                                                                      string? e
                                                                    )
                                                                     (
                                                                      _substring e 1 (
                                                                        + 1 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? e
                                                                    )
                                                                     (
                                                                      hash-table-ref e 1
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref e 1
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    w (
                                                                      cond (
                                                                        (
                                                                          string? e
                                                                        )
                                                                         (
                                                                          _substring e 2 (
                                                                            + 2 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? e
                                                                        )
                                                                         (
                                                                          hash-table-ref e 2
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref e 2
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        ru (
                                                                          find_parent parent u
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            rv (
                                                                              find_parent parent v
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            if (
                                                                              not (
                                                                                equal? ru rv
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                union_parent parent ru rv
                                                                              )
                                                                               (
                                                                                set! total (
                                                                                  _add total w
                                                                                )
                                                                              )
                                                                               (
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
                                                    loop68
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
                                          loop68
                                        )
                                      )
                                    )
                                  )
                                   (
                                    ret65 total
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
        find_isolated_nodes g nodes
      )
       (
        call/cc (
          lambda (
            ret70
          )
           (
            let (
              (
                isolated (
                  _list
                )
              )
            )
             (
              begin (
                call/cc (
                  lambda (
                    break72
                  )
                   (
                    letrec (
                      (
                        loop71 (
                          lambda (
                            xs
                          )
                           (
                            if (
                              null? xs
                            )
                             (
                              quote (
                                
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    node (
                                      car xs
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? (
                                        _len (
                                          hash-table-ref/default g node (
                                            quote (
                                              
                                            )
                                          )
                                        )
                                      )
                                       0
                                    )
                                     (
                                      begin (
                                        set! isolated (
                                          append isolated (
                                            _list node
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
                               (
                                loop71 (
                                  cdr xs
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      loop71 nodes
                    )
                  )
                )
              )
               (
                ret70 isolated
              )
            )
          )
        )
      )
    )
     (
      let (
        (
          g_dfs (
            alist->hash-table (
              _list (
                cons 1 (
                  _list 2 3
                )
              )
               (
                cons 2 (
                  _list 4 5
                )
              )
               (
                cons 3 (
                  _list
                )
              )
               (
                cons 4 (
                  _list
                )
              )
               (
                cons 5 (
                  _list
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
              g_bfs (
                alist->hash-table (
                  _list (
                    cons 1 (
                      _list 2 3
                    )
                  )
                   (
                    cons 2 (
                      _list 4 5
                    )
                  )
                   (
                    cons 3 (
                      _list 6 7
                    )
                  )
                   (
                    cons 4 (
                      _list
                    )
                  )
                   (
                    cons 5 (
                      _list 8
                    )
                  )
                   (
                    cons 6 (
                      _list
                    )
                  )
                   (
                    cons 7 (
                      _list
                    )
                  )
                   (
                    cons 8 (
                      _list
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
                  g_weighted (
                    alist->hash-table (
                      _list (
                        cons 1 (
                          _list (
                            _list 2 7
                          )
                           (
                            _list 3 9
                          )
                           (
                            _list 6 14
                          )
                        )
                      )
                       (
                        cons 2 (
                          _list (
                            _list 1 7
                          )
                           (
                            _list 3 10
                          )
                           (
                            _list 4 15
                          )
                        )
                      )
                       (
                        cons 3 (
                          _list (
                            _list 1 9
                          )
                           (
                            _list 2 10
                          )
                           (
                            _list 4 11
                          )
                           (
                            _list 6 2
                          )
                        )
                      )
                       (
                        cons 4 (
                          _list (
                            _list 2 15
                          )
                           (
                            _list 3 11
                          )
                           (
                            _list 5 6
                          )
                        )
                      )
                       (
                        cons 5 (
                          _list (
                            _list 4 6
                          )
                           (
                            _list 6 9
                          )
                        )
                      )
                       (
                        cons 6 (
                          _list (
                            _list 1 14
                          )
                           (
                            _list 3 2
                          )
                           (
                            _list 5 9
                          )
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
                      g_topo (
                        alist->hash-table (
                          _list (
                            cons 1 (
                              _list 2 3
                            )
                          )
                           (
                            cons 2 (
                              _list 4
                            )
                          )
                           (
                            cons 3 (
                              _list 4
                            )
                          )
                           (
                            cons 4 (
                              _list
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
                          matrix (
                            _list (
                              _list 0 5 9 100000
                            )
                             (
                              _list 100000 0 2 8
                            )
                             (
                              _list 100000 100000 0 7
                            )
                             (
                              _list 4 100000 100000 0
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              g_prim (
                                alist->hash-table (
                                  _list (
                                    cons 1 (
                                      _list (
                                        _list 2 1
                                      )
                                       (
                                        _list 3 3
                                      )
                                    )
                                  )
                                   (
                                    cons 2 (
                                      _list (
                                        _list 1 1
                                      )
                                       (
                                        _list 3 1
                                      )
                                       (
                                        _list 4 6
                                      )
                                    )
                                  )
                                   (
                                    cons 3 (
                                      _list (
                                        _list 1 3
                                      )
                                       (
                                        _list 2 1
                                      )
                                       (
                                        _list 4 2
                                      )
                                    )
                                  )
                                   (
                                    cons 4 (
                                      _list (
                                        _list 2 6
                                      )
                                       (
                                        _list 3 2
                                      )
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
                                  edges_kruskal (
                                    _list (
                                      _list 1 2 1
                                    )
                                     (
                                      _list 2 3 2
                                    )
                                     (
                                      _list 1 3 2
                                    )
                                     (
                                      _list 3 4 1
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      g_iso (
                                        alist->hash-table (
                                          _list (
                                            cons 1 (
                                              _list 2 3
                                            )
                                          )
                                           (
                                            cons 2 (
                                              _list 1 3
                                            )
                                          )
                                           (
                                            cons 3 (
                                              _list 1 2
                                            )
                                          )
                                           (
                                            cons 4 (
                                              _list
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      dfs g_dfs 1
                                    )
                                     (
                                      bfs g_bfs 1
                                    )
                                     (
                                      dijkstra g_weighted 1
                                    )
                                     (
                                      topo g_topo 4
                                    )
                                     (
                                      floyd matrix
                                    )
                                     (
                                      _display (
                                        if (
                                          string? (
                                            prim g_prim 1 4
                                          )
                                        )
                                         (
                                          prim g_prim 1 4
                                        )
                                         (
                                          to-str (
                                            prim g_prim 1 4
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
                                            kruskal edges_kruskal 4
                                          )
                                        )
                                         (
                                          kruskal edges_kruskal 4
                                        )
                                         (
                                          to-str (
                                            kruskal edges_kruskal 4
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
                                          iso (
                                            find_isolated_nodes g_iso (
                                              _list 1 2 3 4
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          _display (
                                            if (
                                              string? iso
                                            )
                                             iso (
                                              to-str iso
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
      )
    )
     (
      let (
        (
          end74 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur75 (
              quotient (
                * (
                  - end74 start73
                )
                 1000000
              )
               jps76
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur75
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
