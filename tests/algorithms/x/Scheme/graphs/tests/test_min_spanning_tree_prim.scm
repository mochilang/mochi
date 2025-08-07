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
        prims_algorithm adjacency
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
                hash-table-set! visited 0 #t
              )
               (
                let (
                  (
                    mst (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        count 1
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
                                break3
                              )
                               (
                                letrec (
                                  (
                                    loop2 (
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
                                                k (
                                                  car xs
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! total (
                                                  + total 1
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop2 (
                                              cdr xs
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  loop2 (
                                    hash-table-keys adjacency
                                  )
                                )
                              )
                            )
                          )
                           (
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
                                          < count total
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                best_u 0
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    best_v 0
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        best_cost 2147483647
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
                                                                            u_str (
                                                                              car xs
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                u (
                                                                                  let (
                                                                                    (
                                                                                      v8 u_str
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    cond (
                                                                                      (
                                                                                        string? v8
                                                                                      )
                                                                                       (
                                                                                        exact (
                                                                                          floor (
                                                                                            string->number v8
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        boolean? v8
                                                                                      )
                                                                                       (
                                                                                        if v8 1 0
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        exact (
                                                                                          floor v8
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                if (
                                                                                  hash-table-ref/default visited u (
                                                                                    quote (
                                                                                      
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
                                                                                                        n (
                                                                                                          car xs
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        if (
                                                                                                          and (
                                                                                                            not (
                                                                                                              hash-table-ref/default visited (
                                                                                                                hash-table-ref n "node"
                                                                                                              )
                                                                                                               (
                                                                                                                quote (
                                                                                                                  
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            _lt (
                                                                                                              hash-table-ref n "cost"
                                                                                                            )
                                                                                                             best_cost
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            set! best_cost (
                                                                                                              hash-table-ref n "cost"
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            set! best_u u
                                                                                                          )
                                                                                                           (
                                                                                                            set! best_v (
                                                                                                              hash-table-ref n "node"
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
                                                                                            hash-table-ref/default adjacency u (
                                                                                              quote (
                                                                                                
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
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        loop6 (
                                                                          cdr xs
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              loop6 (
                                                                hash-table-keys adjacency
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        hash-table-set! visited best_v #t
                                                      )
                                                       (
                                                        set! mst (
                                                          append mst (
                                                            _list (
                                                              alist->hash-table (
                                                                _list (
                                                                  cons "u" best_u
                                                                )
                                                                 (
                                                                  cons "v" best_v
                                                                )
                                                              )
                                                            )
                                                          )
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
                            ret1 mst
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
        test_prim_successful_result
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            let (
              (
                edges (
                  _list (
                    _list 0 1 4
                  )
                   (
                    _list 0 7 8
                  )
                   (
                    _list 1 2 8
                  )
                   (
                    _list 7 8 7
                  )
                   (
                    _list 7 6 1
                  )
                   (
                    _list 2 8 2
                  )
                   (
                    _list 8 6 6
                  )
                   (
                    _list 2 3 7
                  )
                   (
                    _list 2 5 4
                  )
                   (
                    _list 6 5 2
                  )
                   (
                    _list 3 5 14
                  )
                   (
                    _list 3 4 9
                  )
                   (
                    _list 5 4 10
                  )
                   (
                    _list 1 7 11
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    adjacency (
                      alist->hash-table (
                        _list
                      )
                    )
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
                                                    if (
                                                      not (
                                                        cond (
                                                          (
                                                            string? adjacency
                                                          )
                                                           (
                                                            if (
                                                              string-contains adjacency u
                                                            )
                                                             #t #f
                                                          )
                                                        )
                                                         (
                                                          (
                                                            hash-table? adjacency
                                                          )
                                                           (
                                                            if (
                                                              hash-table-exists? adjacency u
                                                            )
                                                             #t #f
                                                          )
                                                        )
                                                         (
                                                          else (
                                                            if (
                                                              member u adjacency
                                                            )
                                                             #t #f
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        hash-table-set! adjacency u (
                                                          _list
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
                                                      not (
                                                        cond (
                                                          (
                                                            string? adjacency
                                                          )
                                                           (
                                                            if (
                                                              string-contains adjacency v
                                                            )
                                                             #t #f
                                                          )
                                                        )
                                                         (
                                                          (
                                                            hash-table? adjacency
                                                          )
                                                           (
                                                            if (
                                                              hash-table-exists? adjacency v
                                                            )
                                                             #t #f
                                                          )
                                                        )
                                                         (
                                                          else (
                                                            if (
                                                              member v adjacency
                                                            )
                                                             #t #f
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        hash-table-set! adjacency v (
                                                          _list
                                                        )
                                                      )
                                                    )
                                                     (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                  )
                                                   (
                                                    hash-table-set! adjacency u (
                                                      append (
                                                        hash-table-ref/default adjacency u (
                                                          quote (
                                                            
                                                          )
                                                        )
                                                      )
                                                       (
                                                        _list (
                                                          alist->hash-table (
                                                            _list (
                                                              cons "node" v
                                                            )
                                                             (
                                                              cons "cost" w
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    hash-table-set! adjacency v (
                                                      append (
                                                        hash-table-ref/default adjacency v (
                                                          quote (
                                                            
                                                          )
                                                        )
                                                      )
                                                       (
                                                        _list (
                                                          alist->hash-table (
                                                            _list (
                                                              cons "node" u
                                                            )
                                                             (
                                                              cons "cost" w
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
                                    loop12 (
                                      cdr xs
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          loop12 edges
                        )
                      )
                    )
                  )
                   (
                    let (
                      (
                        result (
                          prims_algorithm adjacency
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            seen (
                              alist->hash-table (
                                _list
                              )
                            )
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
                                                    key1 (
                                                      string-append (
                                                        string-append (
                                                          to-str-space (
                                                            hash-table-ref e "u"
                                                          )
                                                        )
                                                         ","
                                                      )
                                                       (
                                                        to-str-space (
                                                          hash-table-ref e "v"
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        key2 (
                                                          string-append (
                                                            string-append (
                                                              to-str-space (
                                                                hash-table-ref e "v"
                                                              )
                                                            )
                                                             ","
                                                          )
                                                           (
                                                            to-str-space (
                                                              hash-table-ref e "u"
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        hash-table-set! seen key1 #t
                                                      )
                                                       (
                                                        hash-table-set! seen key2 #t
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop14 (
                                              cdr xs
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  loop14 result
                                )
                              )
                            )
                          )
                           (
                            let (
                              (
                                expected (
                                  _list (
                                    _list 7 6 1
                                  )
                                   (
                                    _list 2 8 2
                                  )
                                   (
                                    _list 6 5 2
                                  )
                                   (
                                    _list 0 1 4
                                  )
                                   (
                                    _list 2 5 4
                                  )
                                   (
                                    _list 2 3 7
                                  )
                                   (
                                    _list 0 7 8
                                  )
                                   (
                                    _list 3 4 9
                                  )
                                )
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
                                                    ans (
                                                      car xs
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        key (
                                                          string-append (
                                                            string-append (
                                                              to-str-space (
                                                                cond (
                                                                  (
                                                                    string? ans
                                                                  )
                                                                   (
                                                                    _substring ans 0 (
                                                                      + 0 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? ans
                                                                  )
                                                                   (
                                                                    hash-table-ref ans 0
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref ans 0
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             ","
                                                          )
                                                           (
                                                            to-str-space (
                                                              cond (
                                                                (
                                                                  string? ans
                                                                )
                                                                 (
                                                                  _substring ans 1 (
                                                                    + 1 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                (
                                                                  hash-table? ans
                                                                )
                                                                 (
                                                                  hash-table-ref ans 1
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref ans 1
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          not (
                                                            hash-table-ref/default seen key (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            ret11 #f
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
                                                loop16 (
                                                  cdr xs
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      loop16 expected
                                    )
                                  )
                                )
                              )
                               (
                                ret11 #t
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
            test_prim_successful_result
          )
        )
         (
          test_prim_successful_result
        )
         (
          to-str (
            test_prim_successful_result
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
            if #t #t #f
          )
        )
         (
          if #t #t #f
        )
         (
          to-str (
            if #t #t #f
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
