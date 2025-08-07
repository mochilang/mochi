;; Generated on 2025-08-07 16:11 +0700
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
        ((number? x)
         (if (integer? x)
             (number->string (inexact->exact x))
             (number->string x)))
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
(define (_div a b) (if (and (integer? a) (integer? b) (exact? a) (exact? b)) (quotient a b) (/ a b)))
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
(define (list-ref-safe lst idx) (if (and (integer? idx) (>= idx 0) (< idx (length lst))) (list-ref lst idx) '()))
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
        new_cache n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                < n 0
              )
               (
                begin (
                  panic "n should be an integer greater than 0."
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  cap (
                    if (
                      equal? n 0
                    )
                     2147483647 n
                  )
                )
              )
               (
                begin (
                  ret1 (
                    alist->hash-table (
                      _list (
                        cons "max_capacity" cap
                      )
                       (
                        cons "store" (
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
      )
    )
     (
      define (
        remove_element xs x
      )
       (
        call/cc (
          lambda (
            ret2
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
                    removed #f
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
                            break4
                          )
                           (
                            letrec (
                              (
                                loop3 (
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
                                        let (
                                          (
                                            v (
                                              list-ref-safe xs i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              and (
                                                eq? removed #f
                                              )
                                               (
                                                string=? v x
                                              )
                                            )
                                             (
                                              begin (
                                                set! removed #t
                                              )
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  append res (
                                                    _list v
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
                                        )
                                      )
                                       (
                                        loop3
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop3
                            )
                          )
                        )
                      )
                       (
                        ret2 res
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
        refer cache x
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                store (
                  hash-table-ref cache "store"
                )
              )
            )
             (
              begin (
                let (
                  (
                    exists #f
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
                                      < i (
                                        _len store
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          string=? (
                                            list-ref-safe store i
                                          )
                                           x
                                        )
                                         (
                                          begin (
                                            set! exists #t
                                          )
                                        )
                                         '(
                                          
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
                                        )
                                      )
                                       (
                                        loop6
                                      )
                                    )
                                     '(
                                      
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
                        if exists (
                          begin (
                            set! store (
                              remove_element store x
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              equal? (
                                _len store
                              )
                               (
                                hash-table-ref cache "max_capacity"
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    new_store (
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
                                                      < j (
                                                        - (
                                                          _len store
                                                        )
                                                         1
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! new_store (
                                                          append new_store (
                                                            _list (
                                                              list-ref-safe store j
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
                                                        loop8
                                                      )
                                                    )
                                                     '(
                                                      
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
                                        set! store new_store
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             '(
                              
                            )
                          )
                        )
                      )
                       (
                        set! store (
                          append (
                            _list x
                          )
                           store
                        )
                      )
                       (
                        ret5 (
                          alist->hash-table (
                            _list (
                              cons "max_capacity" (
                                hash-table-ref cache "max_capacity"
                              )
                            )
                             (
                              cons "store" store
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
        display cache
      )
       (
        call/cc (
          lambda (
            ret10
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
                              < i (
                                _len (
                                  hash-table-ref cache "store"
                                )
                              )
                            )
                             (
                              begin (
                                _display (
                                  if (
                                    string? (
                                      list-ref-safe (
                                        hash-table-ref cache "store"
                                      )
                                       i
                                    )
                                  )
                                   (
                                    list-ref-safe (
                                      hash-table-ref cache "store"
                                    )
                                     i
                                  )
                                   (
                                    to-str (
                                      list-ref-safe (
                                        hash-table-ref cache "store"
                                      )
                                       i
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
                                loop11
                              )
                            )
                             '(
                              
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
            )
          )
        )
      )
    )
     (
      define (
        repr_item s
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            let (
              (
                all_digits #t
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
                                  < i (
                                    _len s
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        ch (
                                          _substring s i (
                                            + i 1
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          or (
                                            string<? ch "0"
                                          )
                                           (
                                            string>? ch "9"
                                          )
                                        )
                                         (
                                          begin (
                                            set! all_digits #f
                                          )
                                        )
                                         '(
                                          
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
                                    loop14
                                  )
                                )
                                 '(
                                  
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
                    if all_digits (
                      begin (
                        ret13 s
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    ret13 (
                      string-append (
                        string-append "'" s
                      )
                       "'"
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
        cache_repr cache
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            let (
              (
                res (
                  string-append (
                    string-append "LRUCache(" (
                      to-str-space (
                        hash-table-ref cache "max_capacity"
                      )
                    )
                  )
                   ") => ["
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
                                    _len (
                                      hash-table-ref cache "store"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      string-append res (
                                        repr_item (
                                          list-ref-safe (
                                            hash-table-ref cache "store"
                                          )
                                           i
                                        )
                                      )
                                    )
                                  )
                                   (
                                    if (
                                      < i (
                                        - (
                                          _len (
                                            hash-table-ref cache "store"
                                          )
                                        )
                                         1
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          string-append res ", "
                                        )
                                      )
                                    )
                                     '(
                                      
                                    )
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
                                 '(
                                  
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
                    set! res (
                      string-append res "]"
                    )
                  )
                   (
                    ret16 res
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
          lru (
            new_cache 4
          )
        )
      )
       (
        begin (
          set! lru (
            refer lru "A"
          )
        )
         (
          set! lru (
            refer lru "2"
          )
        )
         (
          set! lru (
            refer lru "3"
          )
        )
         (
          set! lru (
            refer lru "A"
          )
        )
         (
          set! lru (
            refer lru "4"
          )
        )
         (
          set! lru (
            refer lru "5"
          )
        )
         (
          let (
            (
              r (
                cache_repr lru
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? r
                )
                 r (
                  to-str r
                )
              )
            )
             (
              newline
            )
             (
              if (
                not (
                  string=? r "LRUCache(4) => [5, 4, 'A', 3]"
                )
              )
               (
                begin (
                  panic "Assertion error"
                )
              )
               '(
                
              )
            )
          )
        )
      )
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
