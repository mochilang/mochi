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
        join xs
      )
       (
        call/cc (
          lambda (
            ret1
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
                                    set! s (
                                      string-append s (
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
                    ret1 s
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
        breadth_first_search graph start
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                explored (
                  alist->hash-table (
                    _list
                  )
                )
              )
            )
             (
              begin (
                hash-table-set! explored start #t
              )
               (
                let (
                  (
                    result (
                      _list start
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        queue (
                          _list start
                        )
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
                                      > (
                                        _len queue
                                      )
                                       0
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            v (
                                              list-ref queue 0
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! queue (
                                              take (
                                                drop queue 1
                                              )
                                               (
                                                - (
                                                  _len queue
                                                )
                                                 1
                                              )
                                            )
                                          )
                                           (
                                            let (
                                              (
                                                children (
                                                  hash-table-ref/default graph v (
                                                    quote (
                                                      
                                                    )
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
                                                                    _len children
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        w (
                                                                          list-ref children i
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          not (
                                                                            cond (
                                                                              (
                                                                                string? explored
                                                                              )
                                                                               (
                                                                                if (
                                                                                  string-contains explored w
                                                                                )
                                                                                 #t #f
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? explored
                                                                              )
                                                                               (
                                                                                if (
                                                                                  hash-table-exists? explored w
                                                                                )
                                                                                 #t #f
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                if (
                                                                                  member w explored
                                                                                )
                                                                                 #t #f
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            hash-table-set! explored w #t
                                                                          )
                                                                           (
                                                                            set! result (
                                                                              append result (
                                                                                _list w
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! queue (
                                                                              append queue (
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
                                                                       (
                                                                        set! i (
                                                                          + i 1
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
      )
    )
     (
      define (
        breadth_first_search_with_deque graph start
      )
       (
        call/cc (
          lambda (
            ret9
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
                hash-table-set! visited start #t
              )
               (
                let (
                  (
                    result (
                      _list start
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        queue (
                          _list start
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            head 0
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
                                          < head (
                                            _len queue
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                v (
                                                  list-ref queue head
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! head (
                                                  + head 1
                                                )
                                              )
                                               (
                                                let (
                                                  (
                                                    children (
                                                      hash-table-ref/default graph v (
                                                        quote (
                                                          
                                                        )
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
                                                                        _len children
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            child (
                                                                              list-ref children i
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
                                                                                      string-contains visited child
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
                                                                                      hash-table-exists? visited child
                                                                                    )
                                                                                     #t #f
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    if (
                                                                                      member child visited
                                                                                    )
                                                                                     #t #f
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                hash-table-set! visited child #t
                                                                              )
                                                                               (
                                                                                set! result (
                                                                                  append result (
                                                                                    _list child
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! queue (
                                                                                  append queue (
                                                                                    _list child
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
                            ret9 result
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
          G (
            alist->hash-table (
              _list (
                cons "A" (
                  _list "B" "C"
                )
              )
               (
                cons "B" (
                  _list "A" "D" "E"
                )
              )
               (
                cons "C" (
                  _list "A" "F"
                )
              )
               (
                cons "D" (
                  _list "B"
                )
              )
               (
                cons "E" (
                  _list "B" "F"
                )
              )
               (
                cons "F" (
                  _list "C" "E"
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
              string? (
                join (
                  breadth_first_search G "A"
                )
              )
            )
             (
              join (
                breadth_first_search G "A"
              )
            )
             (
              to-str (
                join (
                  breadth_first_search G "A"
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
                join (
                  breadth_first_search_with_deque G "A"
                )
              )
            )
             (
              join (
                breadth_first_search_with_deque G "A"
              )
            )
             (
              to-str (
                join (
                  breadth_first_search_with_deque G "A"
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
