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
      start16 (
        current-jiffy
      )
    )
     (
      jps19 (
        jiffies-per-second
      )
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
          define (
            print_dist dist
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                begin (
                  _display (
                    if (
                      string? "Vertex Distance"
                    )
                     "Vertex Distance" (
                      to-str "Vertex Distance"
                    )
                  )
                )
                 (
                  newline
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
                                      _len dist
                                    )
                                  )
                                   (
                                    begin (
                                      if (
                                        >= (
                                          list-ref dist i
                                        )
                                         INF
                                      )
                                       (
                                        begin (
                                          _display (
                                            if (
                                              string? i
                                            )
                                             i (
                                              to-str i
                                            )
                                          )
                                        )
                                         (
                                          _display " "
                                        )
                                         (
                                          _display (
                                            if (
                                              string? "\tINF"
                                            )
                                             "\tINF" (
                                              to-str "\tINF"
                                            )
                                          )
                                        )
                                         (
                                          newline
                                        )
                                      )
                                       (
                                        begin (
                                          _display (
                                            if (
                                              string? i
                                            )
                                             i (
                                              to-str i
                                            )
                                          )
                                        )
                                         (
                                          _display " "
                                        )
                                         (
                                          _display (
                                            if (
                                              string? "\t"
                                            )
                                             "\t" (
                                              to-str "\t"
                                            )
                                          )
                                        )
                                         (
                                          _display " "
                                        )
                                         (
                                          _display (
                                            if (
                                              string? (
                                                let (
                                                  (
                                                    v4 (
                                                      list-ref dist i
                                                    )
                                                  )
                                                )
                                                 (
                                                  cond (
                                                    (
                                                      string? v4
                                                    )
                                                     (
                                                      inexact->exact (
                                                        floor (
                                                          string->number v4
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      boolean? v4
                                                    )
                                                     (
                                                      if v4 1 0
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      inexact->exact (
                                                        floor v4
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              let (
                                                (
                                                  v4 (
                                                    list-ref dist i
                                                  )
                                                )
                                              )
                                               (
                                                cond (
                                                  (
                                                    string? v4
                                                  )
                                                   (
                                                    inexact->exact (
                                                      floor (
                                                        string->number v4
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    boolean? v4
                                                  )
                                                   (
                                                    if v4 1 0
                                                  )
                                                )
                                                 (
                                                  else (
                                                    inexact->exact (
                                                      floor v4
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              to-str (
                                                let (
                                                  (
                                                    v4 (
                                                      list-ref dist i
                                                    )
                                                  )
                                                )
                                                 (
                                                  cond (
                                                    (
                                                      string? v4
                                                    )
                                                     (
                                                      inexact->exact (
                                                        floor (
                                                          string->number v4
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      boolean? v4
                                                    )
                                                     (
                                                      if v4 1 0
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      inexact->exact (
                                                        floor v4
                                                      )
                                                    )
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
                  )
                )
              )
            )
          )
        )
         (
          define (
            min_dist mdist vset
          )
           (
            call/cc (
              lambda (
                ret5
              )
               (
                let (
                  (
                    min_val INF
                  )
                )
                 (
                  begin (
                    let (
                      (
                        min_ind (
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
                                            _len mdist
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              and (
                                                not (
                                                  list-ref vset i
                                                )
                                              )
                                               (
                                                < (
                                                  list-ref mdist i
                                                )
                                                 min_val
                                              )
                                            )
                                             (
                                              begin (
                                                set! min_val (
                                                  list-ref mdist i
                                                )
                                              )
                                               (
                                                set! min_ind i
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
                            ret5 min_ind
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
            dijkstra graph src
          )
           (
            call/cc (
              lambda (
                ret8
              )
               (
                let (
                  (
                    v (
                      _len graph
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        mdist (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            vset (
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
                                              < i v
                                            )
                                             (
                                              begin (
                                                set! mdist (
                                                  append mdist (
                                                    _list INF
                                                  )
                                                )
                                              )
                                               (
                                                set! vset (
                                                  append vset (
                                                    _list #f
                                                  )
                                                )
                                              )
                                               (
                                                set! i (
                                                  + i 1
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
                                list-set! mdist src 0.0
                              )
                               (
                                let (
                                  (
                                    count 0
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
                                                  < count (
                                                    - v 1
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        u (
                                                          min_dist mdist vset
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        list-set! vset u #t
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
                                                                break14
                                                              )
                                                               (
                                                                letrec (
                                                                  (
                                                                    loop13 (
                                                                      lambda (
                                                                        
                                                                      )
                                                                       (
                                                                        if (
                                                                          < i v
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                alt (
                                                                                  + (
                                                                                    list-ref mdist u
                                                                                  )
                                                                                   (
                                                                                    cond (
                                                                                      (
                                                                                        string? (
                                                                                          list-ref graph u
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        _substring (
                                                                                          list-ref graph u
                                                                                        )
                                                                                         i (
                                                                                          + i 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? (
                                                                                          list-ref graph u
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref (
                                                                                          list-ref graph u
                                                                                        )
                                                                                         i
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        list-ref (
                                                                                          list-ref graph u
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
                                                                                if (
                                                                                  and (
                                                                                    and (
                                                                                      not (
                                                                                        list-ref vset i
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      < (
                                                                                        cond (
                                                                                          (
                                                                                            string? (
                                                                                              list-ref graph u
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            _substring (
                                                                                              list-ref graph u
                                                                                            )
                                                                                             i (
                                                                                              + i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? (
                                                                                              list-ref graph u
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref (
                                                                                              list-ref graph u
                                                                                            )
                                                                                             i
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref (
                                                                                              list-ref graph u
                                                                                            )
                                                                                             i
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       INF
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    < alt (
                                                                                      list-ref mdist i
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    list-set! mdist i alt
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
                                                                            loop13
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
                                                                  loop13
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
                                   (
                                    ret8 mdist
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
            main
          )
           (
            call/cc (
              lambda (
                ret15
              )
               (
                let (
                  (
                    graph (
                      _list (
                        _list 0.0 10.0 INF INF 5.0
                      )
                       (
                        _list INF 0.0 1.0 INF 2.0
                      )
                       (
                        _list INF INF 0.0 4.0 INF
                      )
                       (
                        _list INF INF 6.0 0.0 INF
                      )
                       (
                        _list INF 3.0 9.0 2.0 0.0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        dist (
                          dijkstra graph 0
                        )
                      )
                    )
                     (
                      begin (
                        print_dist dist
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
     (
      let (
        (
          end17 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur18 (
              quotient (
                * (
                  - end17 start16
                )
                 1000000
              )
               jps19
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur18
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
