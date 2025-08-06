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
      start9 (
        current-jiffy
      )
    )
     (
      jps12 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        max_list xs
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                m (
                  list-ref xs 0
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
                                    if (
                                      > (
                                        list-ref xs i
                                      )
                                       m
                                    )
                                     (
                                      begin (
                                        set! m (
                                          list-ref xs i
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
                    ret1 m
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
        disjoint_set_new set_counts
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                max_set (
                  max_list set_counts
                )
              )
            )
             (
              begin (
                let (
                  (
                    num_sets (
                      _len set_counts
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        ranks (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            parents (
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
                                              < i num_sets
                                            )
                                             (
                                              begin (
                                                set! ranks (
                                                  append ranks (
                                                    _list 1
                                                  )
                                                )
                                              )
                                               (
                                                set! parents (
                                                  append parents (
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
                                ret4 (
                                  alist->hash-table (
                                    _list (
                                      cons "set_counts" set_counts
                                    )
                                     (
                                      cons "max_set" max_set
                                    )
                                     (
                                      cons "ranks" ranks
                                    )
                                     (
                                      cons "parents" parents
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        get_parent ds idx
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              if (
                equal? (
                  list-ref (
                    hash-table-ref ds "parents"
                  )
                   idx
                )
                 idx
              )
               (
                begin (
                  ret7 idx
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
                  parents (
                    hash-table-ref ds "parents"
                  )
                )
              )
               (
                begin (
                  list-set! parents idx (
                    get_parent ds (
                      list-ref parents idx
                    )
                  )
                )
                 (
                  hash-table-set! ds "parents" parents
                )
                 (
                  ret7 (
                    list-ref (
                      hash-table-ref ds "parents"
                    )
                     idx
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
        merge ds src dst
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                src_parent (
                  get_parent ds src
                )
              )
            )
             (
              begin (
                let (
                  (
                    dst_parent (
                      get_parent ds dst
                    )
                  )
                )
                 (
                  begin (
                    if (
                      equal? src_parent dst_parent
                    )
                     (
                      begin (
                        ret8 #f
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
                        list-ref (
                          hash-table-ref ds "ranks"
                        )
                         dst_parent
                      )
                       (
                        list-ref (
                          hash-table-ref ds "ranks"
                        )
                         src_parent
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            counts (
                              hash-table-ref ds "set_counts"
                            )
                          )
                        )
                         (
                          begin (
                            list-set! counts dst_parent (
                              + (
                                list-ref counts dst_parent
                              )
                               (
                                list-ref counts src_parent
                              )
                            )
                          )
                           (
                            list-set! counts src_parent 0
                          )
                           (
                            hash-table-set! ds "set_counts" counts
                          )
                           (
                            let (
                              (
                                parents (
                                  hash-table-ref ds "parents"
                                )
                              )
                            )
                             (
                              begin (
                                list-set! parents src_parent dst_parent
                              )
                               (
                                hash-table-set! ds "parents" parents
                              )
                               (
                                if (
                                  equal? (
                                    list-ref (
                                      hash-table-ref ds "ranks"
                                    )
                                     dst_parent
                                  )
                                   (
                                    list-ref (
                                      hash-table-ref ds "ranks"
                                    )
                                     src_parent
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        ranks (
                                          hash-table-ref ds "ranks"
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        list-set! ranks dst_parent (
                                          + (
                                            list-ref ranks dst_parent
                                          )
                                           1
                                        )
                                      )
                                       (
                                        hash-table-set! ds "ranks" ranks
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
                                    joined (
                                      list-ref (
                                        hash-table-ref ds "set_counts"
                                      )
                                       dst_parent
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      > joined (
                                        hash-table-ref ds "max_set"
                                      )
                                    )
                                     (
                                      begin (
                                        hash-table-set! ds "max_set" joined
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
                     (
                      begin (
                        let (
                          (
                            counts (
                              hash-table-ref ds "set_counts"
                            )
                          )
                        )
                         (
                          begin (
                            list-set! counts src_parent (
                              + (
                                list-ref counts src_parent
                              )
                               (
                                list-ref counts dst_parent
                              )
                            )
                          )
                           (
                            list-set! counts dst_parent 0
                          )
                           (
                            hash-table-set! ds "set_counts" counts
                          )
                           (
                            let (
                              (
                                parents (
                                  hash-table-ref ds "parents"
                                )
                              )
                            )
                             (
                              begin (
                                list-set! parents dst_parent src_parent
                              )
                               (
                                hash-table-set! ds "parents" parents
                              )
                               (
                                let (
                                  (
                                    joined (
                                      list-ref (
                                        hash-table-ref ds "set_counts"
                                      )
                                       src_parent
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      > joined (
                                        hash-table-ref ds "max_set"
                                      )
                                    )
                                     (
                                      begin (
                                        hash-table-set! ds "max_set" joined
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
                    ret8 #t
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
          ds (
            disjoint_set_new (
              _list 1 1 1
            )
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                merge ds 1 2
              )
            )
             (
              merge ds 1 2
            )
             (
              to-str (
                merge ds 1 2
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
                merge ds 0 2
              )
            )
             (
              merge ds 0 2
            )
             (
              to-str (
                merge ds 0 2
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
                merge ds 0 1
              )
            )
             (
              merge ds 0 1
            )
             (
              to-str (
                merge ds 0 1
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
                get_parent ds 0
              )
            )
             (
              get_parent ds 0
            )
             (
              to-str (
                get_parent ds 0
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
                get_parent ds 1
              )
            )
             (
              get_parent ds 1
            )
             (
              to-str (
                get_parent ds 1
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
                hash-table-ref ds "max_set"
              )
            )
             (
              hash-table-ref ds "max_set"
            )
             (
              to-str (
                hash-table-ref ds "max_set"
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
          end10 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur11 (
              quotient (
                * (
                  - end10 start9
                )
                 1000000
              )
               jps12
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur11
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
