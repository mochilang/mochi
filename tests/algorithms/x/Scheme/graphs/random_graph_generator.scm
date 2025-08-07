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
          seed 1
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
                ret1
              )
               (
                begin (
                  set! seed (
                    _mod (
                      + (
                        * seed 1103515245
                      )
                       12345
                    )
                     2147483648
                  )
                )
                 (
                  ret1 seed
                )
              )
            )
          )
        )
         (
          define (
            random
          )
           (
            call/cc (
              lambda (
                ret2
              )
               (
                ret2 (
                  _div (
                    * 1.0 (
                      rand
                    )
                  )
                   2147483648.0
                )
              )
            )
          )
        )
         (
          define (
            complete_graph vertices_number
          )
           (
            call/cc (
              lambda (
                ret3
              )
               (
                let (
                  (
                    graph (
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
                                      < i vertices_number
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            neighbors (
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
                                                              < j vertices_number
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  not (
                                                                    equal? j i
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! neighbors (
                                                                      append neighbors (
                                                                        _list j
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
                                                hash-table-set! graph i neighbors
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
                        ret3 graph
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
            random_graph vertices_number probability directed
          )
           (
            call/cc (
              lambda (
                ret8
              )
               (
                let (
                  (
                    graph (
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
                                      < i vertices_number
                                    )
                                     (
                                      begin (
                                        hash-table-set! graph i (
                                          _list
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
                        if (
                          >= probability 1.0
                        )
                         (
                          begin (
                            ret8 (
                              complete_graph vertices_number
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
                          <= probability 0.0
                        )
                         (
                          begin (
                            ret8 graph
                          )
                        )
                         (
                          quote (
                            
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
                                      < i vertices_number
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            j (
                                              + i 1
                                            )
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
                                                          < j vertices_number
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              _lt (
                                                                random
                                                              )
                                                               probability
                                                            )
                                                             (
                                                              begin (
                                                                hash-table-set! graph i (
                                                                  append (
                                                                    hash-table-ref/default graph i (
                                                                      quote (
                                                                        
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    _list j
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                if (
                                                                  not directed
                                                                )
                                                                 (
                                                                  begin (
                                                                    hash-table-set! graph j (
                                                                      append (
                                                                        hash-table-ref/default graph j (
                                                                          quote (
                                                                            
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        _list i
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
                       (
                        ret8 graph
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
                begin (
                  set! seed 1
                )
                 (
                  let (
                    (
                      g1 (
                        random_graph 4 0.5 #f
                      )
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? g1
                        )
                         g1 (
                          to-str g1
                        )
                      )
                    )
                     (
                      newline
                    )
                     (
                      set! seed 1
                    )
                     (
                      let (
                        (
                          g2 (
                            random_graph 4 0.5 #t
                          )
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? g2
                            )
                             g2 (
                              to-str g2
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
