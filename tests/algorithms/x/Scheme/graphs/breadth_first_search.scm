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
      start12 (
        current-jiffy
      )
    )
     (
      jps15 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        add_edge graph from to
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            if (
              cond (
                (
                  string? graph
                )
                 (
                  if (
                    string-contains graph from
                  )
                   #t #f
                )
              )
               (
                (
                  hash-table? graph
                )
                 (
                  if (
                    hash-table-exists? graph from
                  )
                   #t #f
                )
              )
               (
                else (
                  if (
                    member from graph
                  )
                   #t #f
                )
              )
            )
             (
              begin (
                hash-table-set! graph from (
                  append (
                    hash-table-ref/default graph from (
                      quote (
                        
                      )
                    )
                  )
                   (
                    _list to
                  )
                )
              )
            )
             (
              begin (
                hash-table-set! graph from (
                  _list to
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        print_graph graph
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            call/cc (
              lambda (
                break4
              )
               (
                letrec (
                  (
                    loop3 (
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
                                v (
                                  car xs
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    adj (
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
                                        line (
                                          string-append (
                                            to-str-space v
                                          )
                                           "  :  "
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
                                                          < i (
                                                            _len adj
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! line (
                                                              string-append line (
                                                                to-str-space (
                                                                  list-ref adj i
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            if (
                                                              < i (
                                                                - (
                                                                  _len adj
                                                                )
                                                                 1
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! line (
                                                                  string-append line " -> "
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
                                            _display (
                                              if (
                                                string? line
                                              )
                                               line (
                                                to-str line
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
                           (
                            loop3 (
                              cdr xs
                            )
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  loop3 (
                    hash-table-keys graph
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
        bfs graph start
      )
       (
        call/cc (
          lambda (
            ret7
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
                let (
                  (
                    queue (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        order (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        set! queue (
                          append queue (
                            _list start
                          )
                        )
                      )
                       (
                        hash-table-set! visited start #t
                      )
                       (
                        let (
                          (
                            head 0
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
                                          < head (
                                            _len queue
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                vertex (
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
                                                set! order (
                                                  append order (
                                                    _list vertex
                                                  )
                                                )
                                              )
                                               (
                                                let (
                                                  (
                                                    neighbors (
                                                      hash-table-ref/default graph vertex (
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
                                                                      < i (
                                                                        _len neighbors
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            neighbor (
                                                                              list-ref neighbors i
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
                                                                                      string-contains visited neighbor
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
                                                                                      hash-table-exists? visited neighbor
                                                                                    )
                                                                                     #t #f
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    if (
                                                                                      member neighbor visited
                                                                                    )
                                                                                     #t #f
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                hash-table-set! visited neighbor #t
                                                                              )
                                                                               (
                                                                                set! queue (
                                                                                  append queue (
                                                                                    _list neighbor
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
                            ret7 order
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
          g (
            alist->hash-table (
              _list
            )
          )
        )
      )
       (
        begin (
          add_edge g 0 1
        )
         (
          add_edge g 0 2
        )
         (
          add_edge g 1 2
        )
         (
          add_edge g 2 0
        )
         (
          add_edge g 2 3
        )
         (
          add_edge g 3 3
        )
         (
          print_graph g
        )
         (
          _display (
            if (
              string? (
                bfs g 2
              )
            )
             (
              bfs g 2
            )
             (
              to-str (
                bfs g 2
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
          end13 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur14 (
              quotient (
                * (
                  - end13 start12
                )
                 1000000
              )
               jps15
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur14
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
