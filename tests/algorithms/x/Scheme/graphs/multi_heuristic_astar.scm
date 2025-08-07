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
      start51 (
        current-jiffy
      )
    )
     (
      jps54 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          W1 1.0
        )
      )
       (
        begin (
          let (
            (
              W2 1.0
            )
          )
           (
            begin (
              let (
                (
                  n 20
                )
              )
               (
                begin (
                  let (
                    (
                      n_heuristic 3
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          INF 1000000000.0
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              t 1
                            )
                          )
                           (
                            begin (
                              define (
                                pos_equal a b
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret1
                                  )
                                   (
                                    ret1 (
                                      and (
                                        equal? (
                                          hash-table-ref a "x"
                                        )
                                         (
                                          hash-table-ref b "x"
                                        )
                                      )
                                       (
                                        equal? (
                                          hash-table-ref a "y"
                                        )
                                         (
                                          hash-table-ref b "y"
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              define (
                                pos_key p
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret2
                                  )
                                   (
                                    ret2 (
                                      string-append (
                                        string-append (
                                          to-str-space (
                                            hash-table-ref p "x"
                                          )
                                        )
                                         ","
                                      )
                                       (
                                        to-str-space (
                                          hash-table-ref p "y"
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              define (
                                sqrtApprox x
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret3
                                  )
                                   (
                                    begin (
                                      if (
                                        <= x 0.0
                                      )
                                       (
                                        begin (
                                          ret3 0.0
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
                                          guess x
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
                                                            < i 10
                                                          )
                                                           (
                                                            begin (
                                                              set! guess (
                                                                _div (
                                                                  _add guess (
                                                                    _div x guess
                                                                  )
                                                                )
                                                                 2.0
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
                                              ret3 guess
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
                                consistent_heuristic p goal
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret6
                                  )
                                   (
                                    let (
                                      (
                                        dx (
                                          + 0.0 (
                                            - (
                                              hash-table-ref p "x"
                                            )
                                             (
                                              hash-table-ref goal "x"
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            dy (
                                              + 0.0 (
                                                - (
                                                  hash-table-ref p "y"
                                                )
                                                 (
                                                  hash-table-ref goal "y"
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            ret6 (
                                              sqrtApprox (
                                                _add (
                                                  * dx dx
                                                )
                                                 (
                                                  * dy dy
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
                                iabs x
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret7
                                  )
                                   (
                                    begin (
                                      if (
                                        < x 0
                                      )
                                       (
                                        begin (
                                          ret7 (
                                            - x
                                          )
                                        )
                                      )
                                       (
                                        quote (
                                          
                                        )
                                      )
                                    )
                                     (
                                      ret7 x
                                    )
                                  )
                                )
                              )
                            )
                             (
                              define (
                                heuristic_1 p goal
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret8
                                  )
                                   (
                                    ret8 (
                                      + 0.0 (
                                        _add (
                                          iabs (
                                            - (
                                              hash-table-ref p "x"
                                            )
                                             (
                                              hash-table-ref goal "x"
                                            )
                                          )
                                        )
                                         (
                                          iabs (
                                            - (
                                              hash-table-ref p "y"
                                            )
                                             (
                                              hash-table-ref goal "y"
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
                                heuristic_2 p goal
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret9
                                  )
                                   (
                                    let (
                                      (
                                        h (
                                          consistent_heuristic p goal
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        ret9 (
                                          _div h (
                                            + 0.0 t
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
                                heuristic i p goal
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret10
                                  )
                                   (
                                    begin (
                                      if (
                                        equal? i 0
                                      )
                                       (
                                        begin (
                                          ret10 (
                                            consistent_heuristic p goal
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
                                        equal? i 1
                                      )
                                       (
                                        begin (
                                          ret10 (
                                            heuristic_1 p goal
                                          )
                                        )
                                      )
                                       (
                                        quote (
                                          
                                        )
                                      )
                                    )
                                     (
                                      ret10 (
                                        heuristic_2 p goal
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              define (
                                key_fn start i goal g_func
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret11
                                  )
                                   (
                                    let (
                                      (
                                        g (
                                          hash-table-ref/default g_func (
                                            pos_key start
                                          )
                                           (
                                            quote (
                                              
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        ret11 (
                                          _add g (
                                            * W1 (
                                              heuristic i start goal
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
                                valid p
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret12
                                  )
                                   (
                                    begin (
                                      if (
                                        or (
                                          < (
                                            hash-table-ref p "x"
                                          )
                                           0
                                        )
                                         (
                                          > (
                                            hash-table-ref p "x"
                                          )
                                           (
                                            - n 1
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          ret12 #f
                                        )
                                      )
                                       (
                                        quote (
                                          
                                        )
                                      )
                                    )
                                     (
                                      if (
                                        or (
                                          < (
                                            hash-table-ref p "y"
                                          )
                                           0
                                        )
                                         (
                                          > (
                                            hash-table-ref p "y"
                                          )
                                           (
                                            - n 1
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          ret12 #f
                                        )
                                      )
                                       (
                                        quote (
                                          
                                        )
                                      )
                                    )
                                     (
                                      ret12 #t
                                    )
                                  )
                                )
                              )
                            )
                             (
                              let (
                                (
                                  blocks (
                                    _list (
                                      alist->hash-table (
                                        _list (
                                          cons "x" 0
                                        )
                                         (
                                          cons "y" 1
                                        )
                                      )
                                    )
                                     (
                                      alist->hash-table (
                                        _list (
                                          cons "x" 1
                                        )
                                         (
                                          cons "y" 1
                                        )
                                      )
                                    )
                                     (
                                      alist->hash-table (
                                        _list (
                                          cons "x" 2
                                        )
                                         (
                                          cons "y" 1
                                        )
                                      )
                                    )
                                     (
                                      alist->hash-table (
                                        _list (
                                          cons "x" 3
                                        )
                                         (
                                          cons "y" 1
                                        )
                                      )
                                    )
                                     (
                                      alist->hash-table (
                                        _list (
                                          cons "x" 4
                                        )
                                         (
                                          cons "y" 1
                                        )
                                      )
                                    )
                                     (
                                      alist->hash-table (
                                        _list (
                                          cons "x" 5
                                        )
                                         (
                                          cons "y" 1
                                        )
                                      )
                                    )
                                     (
                                      alist->hash-table (
                                        _list (
                                          cons "x" 6
                                        )
                                         (
                                          cons "y" 1
                                        )
                                      )
                                    )
                                     (
                                      alist->hash-table (
                                        _list (
                                          cons "x" 7
                                        )
                                         (
                                          cons "y" 1
                                        )
                                      )
                                    )
                                     (
                                      alist->hash-table (
                                        _list (
                                          cons "x" 8
                                        )
                                         (
                                          cons "y" 1
                                        )
                                      )
                                    )
                                     (
                                      alist->hash-table (
                                        _list (
                                          cons "x" 9
                                        )
                                         (
                                          cons "y" 1
                                        )
                                      )
                                    )
                                     (
                                      alist->hash-table (
                                        _list (
                                          cons "x" 10
                                        )
                                         (
                                          cons "y" 1
                                        )
                                      )
                                    )
                                     (
                                      alist->hash-table (
                                        _list (
                                          cons "x" 11
                                        )
                                         (
                                          cons "y" 1
                                        )
                                      )
                                    )
                                     (
                                      alist->hash-table (
                                        _list (
                                          cons "x" 12
                                        )
                                         (
                                          cons "y" 1
                                        )
                                      )
                                    )
                                     (
                                      alist->hash-table (
                                        _list (
                                          cons "x" 13
                                        )
                                         (
                                          cons "y" 1
                                        )
                                      )
                                    )
                                     (
                                      alist->hash-table (
                                        _list (
                                          cons "x" 14
                                        )
                                         (
                                          cons "y" 1
                                        )
                                      )
                                    )
                                     (
                                      alist->hash-table (
                                        _list (
                                          cons "x" 15
                                        )
                                         (
                                          cons "y" 1
                                        )
                                      )
                                    )
                                     (
                                      alist->hash-table (
                                        _list (
                                          cons "x" 16
                                        )
                                         (
                                          cons "y" 1
                                        )
                                      )
                                    )
                                     (
                                      alist->hash-table (
                                        _list (
                                          cons "x" 17
                                        )
                                         (
                                          cons "y" 1
                                        )
                                      )
                                    )
                                     (
                                      alist->hash-table (
                                        _list (
                                          cons "x" 18
                                        )
                                         (
                                          cons "y" 1
                                        )
                                      )
                                    )
                                     (
                                      alist->hash-table (
                                        _list (
                                          cons "x" 19
                                        )
                                         (
                                          cons "y" 1
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  define (
                                    in_blocks p
                                  )
                                   (
                                    call/cc (
                                      lambda (
                                        ret13
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
                                                            _len blocks
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              pos_equal (
                                                                list-ref blocks i
                                                              )
                                                               p
                                                            )
                                                             (
                                                              begin (
                                                                ret13 #t
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
                                            ret13 #f
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  define (
                                    pq_put pq node pri
                                  )
                                   (
                                    call/cc (
                                      lambda (
                                        ret16
                                      )
                                       (
                                        let (
                                          (
                                            updated #f
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
                                                                _len pq
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  pos_equal (
                                                                    hash-table-ref (
                                                                      list-ref pq i
                                                                    )
                                                                     "pos"
                                                                  )
                                                                   node
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      < pri (
                                                                        hash-table-ref (
                                                                          list-ref pq i
                                                                        )
                                                                         "pri"
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        list-set! pq i (
                                                                          alist->hash-table (
                                                                            _list (
                                                                              cons "pos" node
                                                                            )
                                                                             (
                                                                              cons "pri" pri
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
                                                                    set! updated #t
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
                                                if (
                                                  not updated
                                                )
                                                 (
                                                  begin (
                                                    set! pq (
                                                      append pq (
                                                        _list (
                                                          alist->hash-table (
                                                            _list (
                                                              cons "pos" node
                                                            )
                                                             (
                                                              cons "pri" pri
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
                                                ret16 pq
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
                                    pq_minkey pq
                                  )
                                   (
                                    call/cc (
                                      lambda (
                                        ret19
                                      )
                                       (
                                        begin (
                                          if (
                                            equal? (
                                              _len pq
                                            )
                                             0
                                          )
                                           (
                                            begin (
                                              ret19 INF
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
                                              first (
                                                list-ref pq 0
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  m (
                                                    hash-table-ref first "pri"
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
                                                                    < i (
                                                                      _len pq
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      let (
                                                                        (
                                                                          item (
                                                                            list-ref pq i
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          if (
                                                                            < (
                                                                              hash-table-ref item "pri"
                                                                            )
                                                                             m
                                                                          )
                                                                           (
                                                                            begin (
                                                                              set! m (
                                                                                hash-table-ref item "pri"
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
                                                      ret19 m
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
                                    pq_pop_min pq
                                  )
                                   (
                                    call/cc (
                                      lambda (
                                        ret22
                                      )
                                       (
                                        let (
                                          (
                                            best (
                                              list-ref pq 0
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
                                                                  < i (
                                                                    _len pq
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      < (
                                                                        hash-table-ref (
                                                                          list-ref pq i
                                                                        )
                                                                         "pri"
                                                                      )
                                                                       (
                                                                        hash-table-ref best "pri"
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! best (
                                                                          list-ref pq i
                                                                        )
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
                                                   (
                                                    let (
                                                      (
                                                        new_pq (
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
                                                            break26
                                                          )
                                                           (
                                                            letrec (
                                                              (
                                                                loop25 (
                                                                  lambda (
                                                                    
                                                                  )
                                                                   (
                                                                    if (
                                                                      < i (
                                                                        _len pq
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
                                                                            set! new_pq (
                                                                              append new_pq (
                                                                                _list (
                                                                                  list-ref pq i
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
                                                                        loop25
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
                                                              loop25
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        ret22 (
                                                          alist->hash-table (
                                                            _list (
                                                              cons "pq" new_pq
                                                            )
                                                             (
                                                              cons "node" best
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
                                    pq_remove pq node
                                  )
                                   (
                                    call/cc (
                                      lambda (
                                        ret27
                                      )
                                       (
                                        let (
                                          (
                                            new_pq (
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
                                                              < i (
                                                                _len pq
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  not (
                                                                    pos_equal (
                                                                      hash-table-ref (
                                                                        list-ref pq i
                                                                      )
                                                                       "pos"
                                                                    )
                                                                     node
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! new_pq (
                                                                      append new_pq (
                                                                        _list (
                                                                          list-ref pq i
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
                                                ret27 new_pq
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
                                    reconstruct back_pointer goal start
                                  )
                                   (
                                    call/cc (
                                      lambda (
                                        ret30
                                      )
                                       (
                                        let (
                                          (
                                            path (
                                              _list
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                current goal
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    key (
                                                      pos_key current
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! path (
                                                      append path (
                                                        _list current
                                                      )
                                                    )
                                                  )
                                                   (
                                                    call/cc (
                                                      lambda (
                                                        break32
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop31 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  not (
                                                                    pos_equal current start
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! current (
                                                                      hash-table-ref/default back_pointer key (
                                                                        quote (
                                                                          
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! key (
                                                                      pos_key current
                                                                    )
                                                                  )
                                                                   (
                                                                    set! path (
                                                                      append path (
                                                                        _list current
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    loop31
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
                                                          loop31
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    let (
                                                      (
                                                        rev (
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
                                                                _len path
                                                              )
                                                               1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            call/cc (
                                                              lambda (
                                                                break34
                                                              )
                                                               (
                                                                letrec (
                                                                  (
                                                                    loop33 (
                                                                      lambda (
                                                                        
                                                                      )
                                                                       (
                                                                        if (
                                                                          >= i 0
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! rev (
                                                                              append rev (
                                                                                _list (
                                                                                  list-ref path i
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
                                                                            loop33
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
                                                                  loop33
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            ret30 rev
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
                                    neighbours p
                                  )
                                   (
                                    call/cc (
                                      lambda (
                                        ret35
                                      )
                                       (
                                        let (
                                          (
                                            left (
                                              alist->hash-table (
                                                _list (
                                                  cons "x" (
                                                    - (
                                                      hash-table-ref p "x"
                                                    )
                                                     1
                                                  )
                                                )
                                                 (
                                                  cons "y" (
                                                    hash-table-ref p "y"
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
                                                right (
                                                  alist->hash-table (
                                                    _list (
                                                      cons "x" (
                                                        + (
                                                          hash-table-ref p "x"
                                                        )
                                                         1
                                                      )
                                                    )
                                                     (
                                                      cons "y" (
                                                        hash-table-ref p "y"
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
                                                    up (
                                                      alist->hash-table (
                                                        _list (
                                                          cons "x" (
                                                            hash-table-ref p "x"
                                                          )
                                                        )
                                                         (
                                                          cons "y" (
                                                            + (
                                                              hash-table-ref p "y"
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
                                                        down (
                                                          alist->hash-table (
                                                            _list (
                                                              cons "x" (
                                                                hash-table-ref p "x"
                                                              )
                                                            )
                                                             (
                                                              cons "y" (
                                                                - (
                                                                  hash-table-ref p "y"
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
                                                        ret35 (
                                                          _list left right up down
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
                                    multi_a_star start goal n_heuristic
                                  )
                                   (
                                    call/cc (
                                      lambda (
                                        ret36
                                      )
                                       (
                                        let (
                                          (
                                            g_function (
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
                                                back_pointer (
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
                                                        open_list (
                                                          _list
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        hash-table-set! g_function (
                                                          pos_key start
                                                        )
                                                         0.0
                                                      )
                                                       (
                                                        hash-table-set! g_function (
                                                          pos_key goal
                                                        )
                                                         INF
                                                      )
                                                       (
                                                        hash-table-set! back_pointer (
                                                          pos_key start
                                                        )
                                                         (
                                                          alist->hash-table (
                                                            _list (
                                                              cons "x" (
                                                                - 1
                                                              )
                                                            )
                                                             (
                                                              cons "y" (
                                                                - 1
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        hash-table-set! back_pointer (
                                                          pos_key goal
                                                        )
                                                         (
                                                          alist->hash-table (
                                                            _list (
                                                              cons "x" (
                                                                - 1
                                                              )
                                                            )
                                                             (
                                                              cons "y" (
                                                                - 1
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        hash-table-set! visited (
                                                          pos_key start
                                                        )
                                                         #t
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
                                                                break38
                                                              )
                                                               (
                                                                letrec (
                                                                  (
                                                                    loop37 (
                                                                      lambda (
                                                                        
                                                                      )
                                                                       (
                                                                        if (
                                                                          < i n_heuristic
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! open_list (
                                                                              append open_list (
                                                                                _list (
                                                                                  _list
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            let (
                                                                              (
                                                                                pri (
                                                                                  key_fn start i goal g_function
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                list-set! open_list i (
                                                                                  pq_put (
                                                                                    list-ref open_list i
                                                                                  )
                                                                                   start pri
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
                                                                            loop37
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
                                                                  loop37
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
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
                                                                          _lt (
                                                                            pq_minkey (
                                                                              list-ref open_list 0
                                                                            )
                                                                          )
                                                                           INF
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                chosen 0
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! i 1
                                                                              )
                                                                               (
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
                                                                                              < i n_heuristic
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                if (
                                                                                                  _le (
                                                                                                    pq_minkey (
                                                                                                      list-ref open_list i
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    * W2 (
                                                                                                      pq_minkey (
                                                                                                        list-ref open_list 0
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    set! chosen i
                                                                                                  )
                                                                                                   (
                                                                                                    break42 (
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
                                                                                               (
                                                                                                set! i (
                                                                                                  + i 1
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
                                                                                if (
                                                                                  not (
                                                                                    equal? chosen 0
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! t (
                                                                                      + t 1
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
                                                                                    pair (
                                                                                      pq_pop_min (
                                                                                        list-ref open_list chosen
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    list-set! open_list chosen (
                                                                                      hash-table-ref pair "pq"
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    let (
                                                                                      (
                                                                                        current (
                                                                                          hash-table-ref pair "node"
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
                                                                                                      < i n_heuristic
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        if (
                                                                                                          not (
                                                                                                            equal? i chosen
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            list-set! open_list i (
                                                                                                              pq_remove (
                                                                                                                list-ref open_list i
                                                                                                              )
                                                                                                               (
                                                                                                                hash-table-ref current "pos"
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
                                                                                        let (
                                                                                          (
                                                                                            ckey (
                                                                                              pos_key (
                                                                                                hash-table-ref current "pos"
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            if (
                                                                                              cond (
                                                                                                (
                                                                                                  string? visited
                                                                                                )
                                                                                                 (
                                                                                                  if (
                                                                                                    string-contains visited ckey
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
                                                                                                    hash-table-exists? visited ckey
                                                                                                  )
                                                                                                   #t #f
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  if (
                                                                                                    member ckey visited
                                                                                                  )
                                                                                                   #t #f
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                loop39
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              quote (
                                                                                                
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            hash-table-set! visited ckey #t
                                                                                          )
                                                                                           (
                                                                                            if (
                                                                                              pos_equal (
                                                                                                hash-table-ref current "pos"
                                                                                              )
                                                                                               goal
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                let (
                                                                                                  (
                                                                                                    path (
                                                                                                      reconstruct back_pointer goal start
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
                                                                                                                      < j (
                                                                                                                        _len path
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        let (
                                                                                                                          (
                                                                                                                            p (
                                                                                                                              cond (
                                                                                                                                (
                                                                                                                                  string? path
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  _substring path j (
                                                                                                                                    + j 1
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                (
                                                                                                                                  hash-table? path
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  hash-table-ref path j
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                else (
                                                                                                                                  list-ref path j
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
                                                                                                                                  string-append (
                                                                                                                                    string-append (
                                                                                                                                      string-append (
                                                                                                                                        string-append "(" (
                                                                                                                                          to-str-space (
                                                                                                                                            hash-table-ref p "x"
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       ","
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      to-str-space (
                                                                                                                                        hash-table-ref p "y"
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
                                                                                                                                          hash-table-ref p "x"
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     ","
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    to-str-space (
                                                                                                                                      hash-table-ref p "y"
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
                                                                                                                                            hash-table-ref p "x"
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       ","
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      to-str-space (
                                                                                                                                        hash-table-ref p "y"
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
                                                                                                                            set! j (
                                                                                                                              + j 1
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
                                                                                                        ret36 (
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
                                                                                           (
                                                                                            let (
                                                                                              (
                                                                                                neighs (
                                                                                                  neighbours (
                                                                                                    hash-table-ref current "pos"
                                                                                                  )
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
                                                                                                                  < k (
                                                                                                                    _len neighs
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    let (
                                                                                                                      (
                                                                                                                        nb (
                                                                                                                          cond (
                                                                                                                            (
                                                                                                                              string? neighs
                                                                                                                            )
                                                                                                                             (
                                                                                                                              _substring neighs k (
                                                                                                                                + k 1
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            (
                                                                                                                              hash-table? neighs
                                                                                                                            )
                                                                                                                             (
                                                                                                                              hash-table-ref neighs k
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            else (
                                                                                                                              list-ref neighs k
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        if (
                                                                                                                          and (
                                                                                                                            valid nb
                                                                                                                          )
                                                                                                                           (
                                                                                                                            eq? (
                                                                                                                              in_blocks nb
                                                                                                                            )
                                                                                                                             #f
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            let (
                                                                                                                              (
                                                                                                                                nkey (
                                                                                                                                  pos_key nb
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                let (
                                                                                                                                  (
                                                                                                                                    tentative (
                                                                                                                                      + (
                                                                                                                                        hash-table-ref/default g_function ckey (
                                                                                                                                          quote (
                                                                                                                                            
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       1.0
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  begin (
                                                                                                                                    if (
                                                                                                                                      or (
                                                                                                                                        not (
                                                                                                                                          cond (
                                                                                                                                            (
                                                                                                                                              string? g_function
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              if (
                                                                                                                                                string-contains g_function nkey
                                                                                                                                              )
                                                                                                                                               #t #f
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            (
                                                                                                                                              hash-table? g_function
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              if (
                                                                                                                                                hash-table-exists? g_function nkey
                                                                                                                                              )
                                                                                                                                               #t #f
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            else (
                                                                                                                                              if (
                                                                                                                                                member nkey g_function
                                                                                                                                              )
                                                                                                                                               #t #f
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        < tentative (
                                                                                                                                          hash-table-ref/default g_function nkey (
                                                                                                                                            quote (
                                                                                                                                              
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      begin (
                                                                                                                                        hash-table-set! g_function nkey tentative
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        hash-table-set! back_pointer nkey (
                                                                                                                                          hash-table-ref current "pos"
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        set! i 0
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        call/cc (
                                                                                                                                          lambda (
                                                                                                                                            break50
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            letrec (
                                                                                                                                              (
                                                                                                                                                loop49 (
                                                                                                                                                  lambda (
                                                                                                                                                    
                                                                                                                                                  )
                                                                                                                                                   (
                                                                                                                                                    if (
                                                                                                                                                      < i n_heuristic
                                                                                                                                                    )
                                                                                                                                                     (
                                                                                                                                                      begin (
                                                                                                                                                        let (
                                                                                                                                                          (
                                                                                                                                                            pri2 (
                                                                                                                                                              _add tentative (
                                                                                                                                                                * W1 (
                                                                                                                                                                  heuristic i nb goal
                                                                                                                                                                )
                                                                                                                                                              )
                                                                                                                                                            )
                                                                                                                                                          )
                                                                                                                                                        )
                                                                                                                                                         (
                                                                                                                                                          begin (
                                                                                                                                                            list-set! open_list i (
                                                                                                                                                              pq_put (
                                                                                                                                                                list-ref open_list i
                                                                                                                                                              )
                                                                                                                                                               nb pri2
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
                                                                                                                                                        loop49
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
                                                                                                                                              loop49
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
                                                            _display (
                                                              if (
                                                                string? "No path found to goal"
                                                              )
                                                               "No path found to goal" (
                                                                to-str "No path found to goal"
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
                                 (
                                  let (
                                    (
                                      start (
                                        alist->hash-table (
                                          _list (
                                            cons "x" 0
                                          )
                                           (
                                            cons "y" 0
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          goal (
                                            alist->hash-table (
                                              _list (
                                                cons "x" (
                                                  - n 1
                                                )
                                              )
                                               (
                                                cons "y" (
                                                  - n 1
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          multi_a_star start goal n_heuristic
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
          end52 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur53 (
              quotient (
                * (
                  - end52 start51
                )
                 1000000
              )
               jps54
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur53
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
