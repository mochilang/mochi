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
      start34 (
        current-jiffy
      )
    )
     (
      jps37 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          INF 1000000000
        )
      )
       (
        begin (
          define (
            connect graph a b w
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                let (
                  (
                    u (
                      - a 1
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        v (
                          - b 1
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            g graph
                          )
                        )
                         (
                          begin (
                            hash-table-set! g u (
                              append (
                                hash-table-ref/default g u (
                                  quote (
                                    
                                  )
                                )
                              )
                               (
                                _list (
                                  _list v w
                                )
                              )
                            )
                          )
                           (
                            hash-table-set! g v (
                              append (
                                hash-table-ref/default g v (
                                  quote (
                                    
                                  )
                                )
                              )
                               (
                                _list (
                                  _list u w
                                )
                              )
                            )
                          )
                           (
                            ret1 g
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
            in_list arr x
          )
           (
            call/cc (
              lambda (
                ret2
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
                                    _len arr
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? (
                                        list-ref arr i
                                      )
                                       x
                                    )
                                     (
                                      begin (
                                        ret2 #t
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
                                    loop3
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
                          loop3
                        )
                      )
                    )
                  )
                   (
                    ret2 #f
                  )
                )
              )
            )
          )
        )
         (
          define (
            prim graph s n
          )
           (
            call/cc (
              lambda (
                ret5
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
                    let (
                      (
                        parent (
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
                        hash-table-set! parent s (
                          - 1
                        )
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
                                              < (
                                                _len known
                                              )
                                               n
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    mini INF
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
                                                                                          in_list known k
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
                                                            set! known (
                                                              append known (
                                                                _list u
                                                              )
                                                            )
                                                          )
                                                           (
                                                            call/cc (
                                                              lambda (
                                                                break11
                                                              )
                                                               (
                                                                letrec (
                                                                  (
                                                                    loop10 (
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
                                                                                            in_list keys v
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
                                                                                               INF
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            if (
                                                                                              and (
                                                                                                not (
                                                                                                  in_list known v
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
                                                                                               (
                                                                                                hash-table-set! parent v u
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
                                                                            loop10 (
                                                                              cdr xs
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  loop10 (
                                                                    hash-table-ref/default graph u (
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
                                    edges (
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
                                                        _len keys
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            v (
                                                              list-ref keys j
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              not (
                                                                equal? v s
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! edges (
                                                                  append edges (
                                                                    _list (
                                                                      _list (
                                                                        + v 1
                                                                      )
                                                                       (
                                                                        + (
                                                                          hash-table-ref/default parent v (
                                                                            quote (
                                                                              
                                                                            )
                                                                          )
                                                                        )
                                                                         1
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
                                                            set! j (
                                                              + j 1
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
                                        ret5 edges
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
            sort_heap h dist
          )
           (
            call/cc (
              lambda (
                ret14
              )
               (
                let (
                  (
                    a h
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
                            break16
                          )
                           (
                            letrec (
                              (
                                loop15 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len a
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
                                                          < j (
                                                            - (
                                                              - (
                                                                _len a
                                                              )
                                                               i
                                                            )
                                                             1
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                dj (
                                                                  if (
                                                                    cond (
                                                                      (
                                                                        string? dist
                                                                      )
                                                                       (
                                                                        if (
                                                                          string-contains dist (
                                                                            list-ref a j
                                                                          )
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
                                                                          hash-table-exists? dist (
                                                                            list-ref a j
                                                                          )
                                                                        )
                                                                         #t #f
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        if (
                                                                          member (
                                                                            list-ref a j
                                                                          )
                                                                           dist
                                                                        )
                                                                         #t #f
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    hash-table-ref/default dist (
                                                                      list-ref a j
                                                                    )
                                                                     (
                                                                      quote (
                                                                        
                                                                      )
                                                                    )
                                                                  )
                                                                   INF
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    dj1 (
                                                                      if (
                                                                        cond (
                                                                          (
                                                                            string? dist
                                                                          )
                                                                           (
                                                                            if (
                                                                              string-contains dist (
                                                                                list-ref a (
                                                                                  + j 1
                                                                                )
                                                                              )
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
                                                                              hash-table-exists? dist (
                                                                                list-ref a (
                                                                                  + j 1
                                                                                )
                                                                              )
                                                                            )
                                                                             #t #f
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            if (
                                                                              member (
                                                                                list-ref a (
                                                                                  + j 1
                                                                                )
                                                                              )
                                                                               dist
                                                                            )
                                                                             #t #f
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref/default dist (
                                                                          list-ref a (
                                                                            + j 1
                                                                          )
                                                                        )
                                                                         (
                                                                          quote (
                                                                            
                                                                          )
                                                                        )
                                                                      )
                                                                       INF
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      > dj dj1
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            t (
                                                                              list-ref a j
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            list-set! a j (
                                                                              list-ref a (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            list-set! a (
                                                                              + j 1
                                                                            )
                                                                             t
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
                                            set! i (
                                              + i 1
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop15
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
                              loop15
                            )
                          )
                        )
                      )
                       (
                        ret14 a
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
            prim_heap graph s n
          )
           (
            call/cc (
              lambda (
                ret19
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
                    let (
                      (
                        parent (
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
                        hash-table-set! parent s (
                          - 1
                        )
                      )
                       (
                        let (
                          (
                            h (
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
                                    break21
                                  )
                                   (
                                    letrec (
                                      (
                                        loop20 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i n
                                            )
                                             (
                                              begin (
                                                set! h (
                                                  append h (
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
                                                loop20
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
                                      loop20
                                    )
                                  )
                                )
                              )
                               (
                                set! h (
                                  sort_heap h dist
                                )
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
                                    call/cc (
                                      lambda (
                                        break23
                                      )
                                       (
                                        letrec (
                                          (
                                            loop22 (
                                              lambda (
                                                
                                              )
                                               (
                                                if (
                                                  > (
                                                    _len h
                                                  )
                                                   0
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        u (
                                                          list-ref h 0
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! h (
                                                          slice h 1 (
                                                            _len h
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
                                                            break25
                                                          )
                                                           (
                                                            letrec (
                                                              (
                                                                loop24 (
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
                                                                                           INF
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        if (
                                                                                          and (
                                                                                            not (
                                                                                              in_list known v
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
                                                                                           (
                                                                                            hash-table-set! parent v u
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
                                                                        loop24 (
                                                                          cdr xs
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              loop24 (
                                                                hash-table-ref/default graph u (
                                                                  quote (
                                                                    
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! h (
                                                          sort_heap h dist
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    loop22
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
                                          loop22
                                        )
                                      )
                                    )
                                  )
                                   (
                                    let (
                                      (
                                        edges (
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
                                                          < j n
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              not (
                                                                equal? j s
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! edges (
                                                                  append edges (
                                                                    _list (
                                                                      _list (
                                                                        + j 1
                                                                      )
                                                                       (
                                                                        + (
                                                                          hash-table-ref/default parent j (
                                                                            quote (
                                                                              
                                                                            )
                                                                          )
                                                                        )
                                                                         1
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
                                                            set! j (
                                                              + j 1
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
                                            ret19 edges
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
            print_edges edges
          )
           (
            call/cc (
              lambda (
                ret28
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
                        break30
                      )
                       (
                        letrec (
                          (
                            loop29 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len edges
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        e (
                                          list-ref edges i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        _display (
                                          if (
                                            string? (
                                              string-append (
                                                string-append (
                                                  string-append (
                                                    string-append "(" (
                                                      to-str-space (
                                                        list-ref e 0
                                                      )
                                                    )
                                                  )
                                                   ", "
                                                )
                                                 (
                                                  to-str-space (
                                                    list-ref e 1
                                                  )
                                                )
                                              )
                                               ")"
                                            )
                                          )
                                           (
                                            string-append (
                                              string-append (
                                                string-append (
                                                  string-append "(" (
                                                    to-str-space (
                                                      list-ref e 0
                                                    )
                                                  )
                                                )
                                                 ", "
                                              )
                                               (
                                                to-str-space (
                                                  list-ref e 1
                                                )
                                              )
                                            )
                                             ")"
                                          )
                                           (
                                            to-str (
                                              string-append (
                                                string-append (
                                                  string-append (
                                                    string-append "(" (
                                                      to-str-space (
                                                        list-ref e 0
                                                      )
                                                    )
                                                  )
                                                   ", "
                                                )
                                                 (
                                                  to-str-space (
                                                    list-ref e 1
                                                  )
                                                )
                                              )
                                               ")"
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
                                    )
                                  )
                                   (
                                    loop29
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
                          loop29
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
            test_vector
          )
           (
            call/cc (
              lambda (
                ret31
              )
               (
                let (
                  (
                    x 5
                  )
                )
                 (
                  begin (
                    let (
                      (
                        G (
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
                            i 0
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
                                          < i x
                                        )
                                         (
                                          begin (
                                            hash-table-set! G i (
                                              _list
                                            )
                                          )
                                           (
                                            set! i (
                                              + i 1
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
                            set! G (
                              connect G 1 2 15
                            )
                          )
                           (
                            set! G (
                              connect G 1 3 12
                            )
                          )
                           (
                            set! G (
                              connect G 2 4 13
                            )
                          )
                           (
                            set! G (
                              connect G 2 5 5
                            )
                          )
                           (
                            set! G (
                              connect G 3 2 6
                            )
                          )
                           (
                            set! G (
                              connect G 3 4 6
                            )
                          )
                           (
                            let (
                              (
                                mst (
                                  prim G 0 x
                                )
                              )
                            )
                             (
                              begin (
                                print_edges mst
                              )
                               (
                                let (
                                  (
                                    mst_heap (
                                      prim_heap G 0 x
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    print_edges mst_heap
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
          test_vector
        )
      )
    )
     (
      let (
        (
          end35 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur36 (
              quotient (
                * (
                  - end35 start34
                )
                 1000000
              )
               jps37
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur36
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
