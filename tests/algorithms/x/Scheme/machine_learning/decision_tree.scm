;; Generated on 2025-08-07 10:06 +0700
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
      define OP_LEAF 0
    )
     (
      define OP_BRANCH 1
    )
     (
      let (
        (
          PI 3.141592653589793
        )
      )
       (
        begin (
          let (
            (
              TWO_PI 6.283185307179586
            )
          )
           (
            begin (
              define (
                _mod x m
              )
               (
                call/cc (
                  lambda (
                    ret1
                  )
                   (
                    ret1 (
                      - x (
                        * (
                          + 0.0 (
                            let (
                              (
                                v2 (
                                  _div x m
                                )
                              )
                            )
                             (
                              cond (
                                (
                                  string? v2
                                )
                                 (
                                  exact (
                                    floor (
                                      string->number v2
                                    )
                                  )
                                )
                              )
                               (
                                (
                                  boolean? v2
                                )
                                 (
                                  if v2 1 0
                                )
                              )
                               (
                                else (
                                  exact (
                                    floor v2
                                  )
                                )
                              )
                            )
                          )
                        )
                         m
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                sin x
              )
               (
                call/cc (
                  lambda (
                    ret3
                  )
                   (
                    let (
                      (
                        y (
                          - (
                            _mod (
                              + x PI
                            )
                             TWO_PI
                          )
                           PI
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            y2 (
                              * y y
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                y3 (
                                  * y2 y
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    y5 (
                                      * y3 y2
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        y7 (
                                          * y5 y2
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        ret3 (
                                          - (
                                            _add (
                                              - y (
                                                _div y3 6.0
                                              )
                                            )
                                             (
                                              _div y5 120.0
                                            )
                                          )
                                           (
                                            _div y7 5040.0
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
                  seed 123456789
                )
              )
               (
                begin (
                  define (
                    rand
                  )
                   (
                    call/cc (
                      lambda (
                        ret4
                      )
                       (
                        begin (
                          set! seed (
                            _mod (
                              + (
                                * 1103515245 seed
                              )
                               12345
                            )
                             2147483648
                          )
                        )
                         (
                          ret4 (
                            _div (
                              + 0.0 seed
                            )
                             2147483648.0
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    mean vals
                  )
                   (
                    call/cc (
                      lambda (
                        ret5
                      )
                       (
                        let (
                          (
                            sum 0.0
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
                                                _len vals
                                              )
                                            )
                                             (
                                              begin (
                                                set! sum (
                                                  + sum (
                                                    list-ref vals i
                                                  )
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
                                ret5 (
                                  _div sum (
                                    _len vals
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
                    mean_squared_error labels prediction
                  )
                   (
                    call/cc (
                      lambda (
                        ret8
                      )
                       (
                        let (
                          (
                            total 0.0
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
                                    break10
                                  )
                                   (
                                    letrec (
                                      (
                                        loop9 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i (
                                                _len labels
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    diff (
                                                      - (
                                                        list-ref labels i
                                                      )
                                                       prediction
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! total (
                                                      _add total (
                                                        * diff diff
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
                                                loop9
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
                                      loop9
                                    )
                                  )
                                )
                              )
                               (
                                ret8 (
                                  _div total (
                                    _len labels
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
                    train_tree x y depth min_leaf_size
                  )
                   (
                    call/cc (
                      lambda (
                        ret11
                      )
                       (
                        begin (
                          if (
                            < (
                              _len x
                            )
                             (
                              * 2 min_leaf_size
                            )
                          )
                           (
                            begin (
                              ret11 (
                                alist->hash-table (
                                  _list (
                                    cons "__tag" OP_LEAF
                                  )
                                   (
                                    cons "prediction" (
                                      mean y
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
                          if (
                            equal? depth 1
                          )
                           (
                            begin (
                              ret11 (
                                alist->hash-table (
                                  _list (
                                    cons "__tag" OP_LEAF
                                  )
                                   (
                                    cons "prediction" (
                                      mean y
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
                              best_split 0
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  min_error (
                                    * (
                                      mean_squared_error x (
                                        mean y
                                      )
                                    )
                                     2.0
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
                                                      _len x
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      if (
                                                        < (
                                                          _len (
                                                            take (
                                                              drop x 0
                                                            )
                                                             (
                                                              - i 0
                                                            )
                                                          )
                                                        )
                                                         min_leaf_size
                                                      )
                                                       (
                                                        begin (
                                                          set! i i
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          if (
                                                            < (
                                                              _len (
                                                                take (
                                                                  drop x i
                                                                )
                                                                 (
                                                                  - (
                                                                    length x
                                                                  )
                                                                   i
                                                                )
                                                              )
                                                            )
                                                             min_leaf_size
                                                          )
                                                           (
                                                            begin (
                                                              set! i i
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  err_left (
                                                                    mean_squared_error (
                                                                      take (
                                                                        drop x 0
                                                                      )
                                                                       (
                                                                        - i 0
                                                                      )
                                                                    )
                                                                     (
                                                                      mean (
                                                                        take (
                                                                          drop y 0
                                                                        )
                                                                         (
                                                                          - i 0
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
                                                                      err_right (
                                                                        mean_squared_error (
                                                                          take (
                                                                            drop x i
                                                                          )
                                                                           (
                                                                            - (
                                                                              length x
                                                                            )
                                                                             i
                                                                          )
                                                                        )
                                                                         (
                                                                          mean (
                                                                            take (
                                                                              drop y i
                                                                            )
                                                                             (
                                                                              - (
                                                                                length y
                                                                              )
                                                                               i
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
                                                                          err (
                                                                            _add err_left err_right
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          if (
                                                                            _lt err min_error
                                                                          )
                                                                           (
                                                                            begin (
                                                                              set! best_split i
                                                                            )
                                                                             (
                                                                              set! min_error err
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
                                                      set! i (
                                                        + i 1
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
                                      if (
                                        not (
                                          equal? best_split 0
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              left_x (
                                                take (
                                                  drop x 0
                                                )
                                                 (
                                                  - best_split 0
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  left_y (
                                                    take (
                                                      drop y 0
                                                    )
                                                     (
                                                      - best_split 0
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      right_x (
                                                        take (
                                                          drop x best_split
                                                        )
                                                         (
                                                          - (
                                                            length x
                                                          )
                                                           best_split
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          right_y (
                                                            take (
                                                              drop y best_split
                                                            )
                                                             (
                                                              - (
                                                                length y
                                                              )
                                                               best_split
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              boundary (
                                                                list-ref x best_split
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  left_tree (
                                                                    train_tree left_x left_y (
                                                                      - depth 1
                                                                    )
                                                                     min_leaf_size
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      right_tree (
                                                                        train_tree right_x right_y (
                                                                          - depth 1
                                                                        )
                                                                         min_leaf_size
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      ret11 (
                                                                        alist->hash-table (
                                                                          _list (
                                                                            cons "__tag" OP_BRANCH
                                                                          )
                                                                           (
                                                                            cons "decision_boundary" boundary
                                                                          )
                                                                           (
                                                                            cons "left" left_tree
                                                                          )
                                                                           (
                                                                            cons "right" right_tree
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
                                        quote (
                                          
                                        )
                                      )
                                    )
                                     (
                                      ret11 (
                                        alist->hash-table (
                                          _list (
                                            cons "__tag" OP_LEAF
                                          )
                                           (
                                            cons "prediction" (
                                              mean y
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
                    predict tree value
                  )
                   (
                    call/cc (
                      lambda (
                        ret14
                      )
                       (
                        ret14 (
                          let (
                            (
                              match15 tree
                            )
                          )
                           (
                            if (
                              equal? (
                                hash-table-ref match15 "__tag"
                              )
                               OP_LEAF
                            )
                             (
                              let (
                                (
                                  p (
                                    hash-table-ref match15 "prediction"
                                  )
                                )
                              )
                               p
                            )
                             (
                              if (
                                equal? (
                                  hash-table-ref match15 "__tag"
                                )
                                 OP_BRANCH
                              )
                               (
                                let (
                                  (
                                    b (
                                      hash-table-ref match15 "decision_boundary"
                                    )
                                  )
                                   (
                                    l (
                                      hash-table-ref match15 "left"
                                    )
                                  )
                                   (
                                    r (
                                      hash-table-ref match15 "right"
                                    )
                                  )
                                )
                                 (
                                  if (
                                    >= value b
                                  )
                                   (
                                    predict r value
                                  )
                                   (
                                    predict l value
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
                 (
                  define (
                    main
                  )
                   (
                    call/cc (
                      lambda (
                        ret16
                      )
                       (
                        let (
                          (
                            x (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                v (
                                  - 1.0
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
                                              < v 1.0
                                            )
                                             (
                                              begin (
                                                set! x (
                                                  append x (
                                                    _list v
                                                  )
                                                )
                                              )
                                               (
                                                set! v (
                                                  + v 0.005
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
                                    y (
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
                                                        _len x
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! y (
                                                          append y (
                                                            _list (
                                                              sin (
                                                                list-ref x i
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
                                            tree (
                                              train_tree x y 10 10
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                test_cases (
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
                                                              < i 10
                                                            )
                                                             (
                                                              begin (
                                                                set! test_cases (
                                                                  append test_cases (
                                                                    _list (
                                                                      - (
                                                                        * (
                                                                          rand
                                                                        )
                                                                         2.0
                                                                      )
                                                                       1.0
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
                                                let (
                                                  (
                                                    predictions (
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
                                                                    _len test_cases
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! predictions (
                                                                      append predictions (
                                                                        _list (
                                                                          predict tree (
                                                                            list-ref test_cases i
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
                                                        sum_err 0.0
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
                                                                        _len test_cases
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            diff (
                                                                              - (
                                                                                list-ref predictions i
                                                                              )
                                                                               (
                                                                                list-ref test_cases i
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! sum_err (
                                                                              _add sum_err (
                                                                                * diff diff
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
                                                        let (
                                                          (
                                                            avg_error (
                                                              _div sum_err (
                                                                _len test_cases
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            _display (
                                                              if (
                                                                string? (
                                                                  string-append "Test values: " (
                                                                    to-str-space test_cases
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                string-append "Test values: " (
                                                                  to-str-space test_cases
                                                                )
                                                              )
                                                               (
                                                                to-str (
                                                                  string-append "Test values: " (
                                                                    to-str-space test_cases
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
                                                                  string-append "Predictions: " (
                                                                    to-str-space predictions
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                string-append "Predictions: " (
                                                                  to-str-space predictions
                                                                )
                                                              )
                                                               (
                                                                to-str (
                                                                  string-append "Predictions: " (
                                                                    to-str-space predictions
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
                                                                  string-append "Average error: " (
                                                                    to-str-space avg_error
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                string-append "Average error: " (
                                                                  to-str-space avg_error
                                                                )
                                                              )
                                                               (
                                                                to-str (
                                                                  string-append "Average error: " (
                                                                    to-str-space avg_error
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
                  main
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
