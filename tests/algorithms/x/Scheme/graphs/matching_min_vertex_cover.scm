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
        contains xs v
      )
       (
        call/cc (
          lambda (
            ret1
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
                                  x (
                                    car xs
                                  )
                                )
                              )
                               (
                                begin (
                                  if (
                                    equal? x v
                                  )
                                   (
                                    begin (
                                      ret1 #t
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
                    loop2 xs
                  )
                )
              )
            )
             (
              ret1 #f
            )
          )
        )
      )
    )
     (
      define (
        get_edges graph
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                n (
                  _len graph
                )
              )
            )
             (
              begin (
                let (
                  (
                    edges (
                      _list
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
                                i
                              )
                               (
                                if (
                                  < i n
                                )
                                 (
                                  begin (
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
                                                          j (
                                                            car xs
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          set! edges (
                                                            append edges (
                                                              _list (
                                                                _list i j
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      loop7 (
                                                        cdr xs
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop7 (
                                              hash-table-ref/default graph i (
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
                                    loop5 (
                                      + i 1
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
                         (
                          loop5 0
                        )
                      )
                    )
                  )
                   (
                    ret4 edges
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
        matching_min_vertex_cover graph
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                chosen (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    edges (
                      get_edges graph
                    )
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
                                  > (
                                    _len edges
                                  )
                                   0
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        idx (
                                          - (
                                            _len edges
                                          )
                                           1
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
                                                  string? edges
                                                )
                                                 (
                                                  _substring edges idx (
                                                    + idx 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? edges
                                                )
                                                 (
                                                  hash-table-ref edges idx
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref edges idx
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! edges (
                                              slice edges 0 idx
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
                                                    if (
                                                      not (
                                                        contains chosen u
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! chosen (
                                                          append chosen (
                                                            _list u
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
                                                      not (
                                                        contains chosen v
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! chosen (
                                                          append chosen (
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
                                                        filtered (
                                                          _list
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
                                                                            edge (
                                                                              car xs
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                a (
                                                                                  cond (
                                                                                    (
                                                                                      string? edge
                                                                                    )
                                                                                     (
                                                                                      _substring edge 0 (
                                                                                        + 0 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? edge
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref edge 0
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref edge 0
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    b (
                                                                                      cond (
                                                                                        (
                                                                                          string? edge
                                                                                        )
                                                                                         (
                                                                                          _substring edge 1 (
                                                                                            + 1 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? edge
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref edge 1
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref edge 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    if (
                                                                                      and (
                                                                                        and (
                                                                                          and (
                                                                                            not (
                                                                                              equal? a u
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            not (
                                                                                              equal? b u
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          not (
                                                                                            equal? a v
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        not (
                                                                                          equal? b v
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! filtered (
                                                                                          append filtered (
                                                                                            _list edge
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
                                                        set! edges filtered
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
                    ret9 chosen
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
          graph (
            alist->hash-table (
              _list (
                cons 0 (
                  _list 1 3
                )
              )
               (
                cons 1 (
                  _list 0 3
                )
              )
               (
                cons 2 (
                  _list 0 3 4
                )
              )
               (
                cons 3 (
                  _list 0 1 2
                )
              )
               (
                cons 4 (
                  _list 2 3
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
              cover (
                matching_min_vertex_cover graph
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    to-str-space cover
                  )
                )
                 (
                  to-str-space cover
                )
                 (
                  to-str (
                    to-str-space cover
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
