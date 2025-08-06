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
      start13 (
        current-jiffy
      )
    )
     (
      jps16 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        make_set ds x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                p (
                  hash-table-ref ds "parent"
                )
              )
            )
             (
              begin (
                let (
                  (
                    r (
                      hash-table-ref ds "rank"
                    )
                  )
                )
                 (
                  begin (
                    list-set! p x x
                  )
                   (
                    list-set! r x 0
                  )
                   (
                    ret1 (
                      alist->hash-table (
                        _list (
                          cons "parent" p
                        )
                         (
                          cons "rank" r
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
        find_set ds x
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                equal? (
                  list-ref (
                    hash-table-ref ds "parent"
                  )
                   x
                )
                 x
              )
               (
                begin (
                  ret2 (
                    alist->hash-table (
                      _list (
                        cons "ds" ds
                      )
                       (
                        cons "root" x
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
                  res (
                    find_set ds (
                      list-ref (
                        hash-table-ref ds "parent"
                      )
                       x
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      p (
                        hash-table-ref (
                          hash-table-ref res "ds"
                        )
                         "parent"
                      )
                    )
                  )
                   (
                    begin (
                      list-set! p x (
                        hash-table-ref res "root"
                      )
                    )
                     (
                      ret2 (
                        alist->hash-table (
                          _list (
                            cons "ds" (
                              alist->hash-table (
                                _list (
                                  cons "parent" p
                                )
                                 (
                                  cons "rank" (
                                    hash-table-ref (
                                      hash-table-ref res "ds"
                                    )
                                     "rank"
                                  )
                                )
                              )
                            )
                          )
                           (
                            cons "root" (
                              hash-table-ref res "root"
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
        union_set ds x y
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            let (
              (
                fx (
                  find_set ds x
                )
              )
            )
             (
              begin (
                let (
                  (
                    ds1 (
                      hash-table-ref fx "ds"
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        x_root (
                          hash-table-ref fx "root"
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            fy (
                              find_set ds1 y
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                ds2 (
                                  hash-table-ref fy "ds"
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    y_root (
                                      hash-table-ref fy "root"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? x_root y_root
                                    )
                                     (
                                      begin (
                                        ret3 ds2
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
                                        p (
                                          hash-table-ref ds2 "parent"
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            r (
                                              hash-table-ref ds2 "rank"
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              _gt (
                                                cond (
                                                  (
                                                    string? r
                                                  )
                                                   (
                                                    _substring r x_root (
                                                      + x_root 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? r
                                                  )
                                                   (
                                                    hash-table-ref r x_root
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref r x_root
                                                  )
                                                )
                                              )
                                               (
                                                cond (
                                                  (
                                                    string? r
                                                  )
                                                   (
                                                    _substring r y_root (
                                                      + y_root 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? r
                                                  )
                                                   (
                                                    hash-table-ref r y_root
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref r y_root
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                list-set! p y_root x_root
                                              )
                                            )
                                             (
                                              begin (
                                                list-set! p x_root y_root
                                              )
                                               (
                                                if (
                                                  equal? (
                                                    cond (
                                                      (
                                                        string? r
                                                      )
                                                       (
                                                        _substring r x_root (
                                                          + x_root 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        hash-table? r
                                                      )
                                                       (
                                                        hash-table-ref r x_root
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        list-ref r x_root
                                                      )
                                                    )
                                                  )
                                                   (
                                                    cond (
                                                      (
                                                        string? r
                                                      )
                                                       (
                                                        _substring r y_root (
                                                          + y_root 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        hash-table? r
                                                      )
                                                       (
                                                        hash-table-ref r y_root
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        list-ref r y_root
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    list-set! r y_root (
                                                      _add (
                                                        cond (
                                                          (
                                                            string? r
                                                          )
                                                           (
                                                            _substring r y_root (
                                                              + y_root 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          (
                                                            hash-table? r
                                                          )
                                                           (
                                                            hash-table-ref r y_root
                                                          )
                                                        )
                                                         (
                                                          else (
                                                            list-ref r y_root
                                                          )
                                                        )
                                                      )
                                                       1
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
                                            ret3 (
                                              alist->hash-table (
                                                _list (
                                                  cons "parent" p
                                                )
                                                 (
                                                  cons "rank" r
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
    )
     (
      define (
        same_python_set a b
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                and (
                  < a 3
                )
                 (
                  < b 3
                )
              )
               (
                begin (
                  ret4 #t
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                and (
                  and (
                    and (
                      >= a 3
                    )
                     (
                      < a 6
                    )
                  )
                   (
                    >= b 3
                  )
                )
                 (
                  < b 6
                )
              )
               (
                begin (
                  ret4 #t
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret4 #f
            )
          )
        )
      )
    )
     (
      let (
        (
          ds (
            alist->hash-table (
              _list (
                cons "parent" (
                  _list
                )
              )
               (
                cons "rank" (
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
                            < i 6
                          )
                           (
                            begin (
                              hash-table-set! ds "parent" (
                                append (
                                  hash-table-ref ds "parent"
                                )
                                 (
                                  _list 0
                                )
                              )
                            )
                             (
                              hash-table-set! ds "rank" (
                                append (
                                  hash-table-ref ds "rank"
                                )
                                 (
                                  _list 0
                                )
                              )
                            )
                             (
                              set! ds (
                                make_set ds i
                              )
                            )
                             (
                              set! i (
                                + i 1
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
              set! ds (
                union_set ds 0 1
              )
            )
             (
              set! ds (
                union_set ds 1 2
              )
            )
             (
              set! ds (
                union_set ds 3 4
              )
            )
             (
              set! ds (
                union_set ds 3 5
              )
            )
             (
              set! i 0
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
                            < i 6
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
                                                < j 6
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      res_i (
                                                        find_set ds i
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! ds (
                                                        hash-table-ref res_i "ds"
                                                      )
                                                    )
                                                     (
                                                      let (
                                                        (
                                                          root_i (
                                                            hash-table-ref res_i "root"
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              res_j (
                                                                find_set ds j
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              set! ds (
                                                                hash-table-ref res_j "ds"
                                                              )
                                                            )
                                                             (
                                                              let (
                                                                (
                                                                  root_j (
                                                                    hash-table-ref res_j "root"
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      same (
                                                                        same_python_set i j
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      let (
                                                                        (
                                                                          root_same (
                                                                            equal? root_i root_j
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          if same (
                                                                            begin (
                                                                              if (
                                                                                not root_same
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  panic "nodes should be in same set"
                                                                                )
                                                                              )
                                                                               (
                                                                                quote (
                                                                                  
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              if root_same (
                                                                                begin (
                                                                                  panic "nodes should be in different sets"
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
                                                                          set! j (
                                                                            + j 1
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
             (
              set! i 0
            )
             (
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
                            < i 6
                          )
                           (
                            begin (
                              let (
                                (
                                  res (
                                    find_set ds i
                                  )
                                )
                              )
                               (
                                begin (
                                  set! ds (
                                    hash-table-ref res "ds"
                                  )
                                )
                                 (
                                  _display (
                                    if (
                                      string? (
                                        to-str-space (
                                          hash-table-ref res "root"
                                        )
                                      )
                                    )
                                     (
                                      to-str-space (
                                        hash-table-ref res "root"
                                      )
                                    )
                                     (
                                      to-str (
                                        to-str-space (
                                          hash-table-ref res "root"
                                        )
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
                              loop11
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
                    loop11
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
          end14 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur15 (
              quotient (
                * (
                  - end14 start13
                )
                 1000000
              )
               jps16
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur15
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
