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
      define (
        abs x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                < x 0.0
              )
               (
                begin (
                  ret1 (
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
              ret1 x
            )
          )
        )
      )
    )
     (
      define (
        pow_int base exp
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                result 1.0
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
                                  < i exp
                                )
                                 (
                                  begin (
                                    set! result (
                                      * result base
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
                    ret2 result
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
        nth_root x n
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              if (
                equal? x 0.0
              )
               (
                begin (
                  ret5 0.0
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
                                    < i 10
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          denom (
                                            pow_int guess (
                                              - n 1
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          set! guess (
                                            _div (
                                              _add (
                                                * (
                                                  + 0.0 (
                                                    - n 1
                                                  )
                                                )
                                                 guess
                                              )
                                               (
                                                _div x denom
                                              )
                                            )
                                             (
                                              + 0.0 n
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
                      ret5 guess
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
        round_nearest x
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            begin (
              if (
                >= x 0.0
              )
               (
                begin (
                  let (
                    (
                      n (
                        let (
                          (
                            v9 (
                              + x 0.5
                            )
                          )
                        )
                         (
                          cond (
                            (
                              string? v9
                            )
                             (
                              inexact->exact (
                                floor (
                                  string->number v9
                                )
                              )
                            )
                          )
                           (
                            (
                              boolean? v9
                            )
                             (
                              if v9 1 0
                            )
                          )
                           (
                            else (
                              inexact->exact (
                                floor v9
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      ret8 (
                        + 0.0 n
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
                  n (
                    let (
                      (
                        v10 (
                          - x 0.5
                        )
                      )
                    )
                     (
                      cond (
                        (
                          string? v10
                        )
                         (
                          inexact->exact (
                            floor (
                              string->number v10
                            )
                          )
                        )
                      )
                       (
                        (
                          boolean? v10
                        )
                         (
                          if v10 1 0
                        )
                      )
                       (
                        else (
                          inexact->exact (
                            floor v10
                          )
                        )
                      )
                    )
                  )
                )
              )
               (
                begin (
                  ret8 (
                    + 0.0 n
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
        compute_geometric_mean nums
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            begin (
              if (
                equal? (
                  _len nums
                )
                 0
              )
               (
                begin (
                  panic "no numbers"
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
                  product 1.0
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
                                      _len nums
                                    )
                                  )
                                   (
                                    begin (
                                      set! product (
                                        * product (
                                          list-ref nums i
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
                        and (
                          < product 0.0
                        )
                         (
                          equal? (
                            _mod (
                              _len nums
                            )
                             2
                          )
                           0
                        )
                      )
                       (
                        begin (
                          panic "Cannot Compute Geometric Mean for these numbers."
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
                          mean (
                            nth_root (
                              abs product
                            )
                             (
                              _len nums
                            )
                          )
                        )
                      )
                       (
                        begin (
                          if (
                            < product 0.0
                          )
                           (
                            begin (
                              set! mean (
                                - mean
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
                              possible (
                                round_nearest mean
                              )
                            )
                          )
                           (
                            begin (
                              if (
                                equal? (
                                  pow_int possible (
                                    _len nums
                                  )
                                )
                                 product
                              )
                               (
                                begin (
                                  set! mean possible
                                )
                              )
                               (
                                quote (
                                  
                                )
                              )
                            )
                             (
                              ret11 mean
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
        test_compute_geometric_mean
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            let (
              (
                eps 0.0001
              )
            )
             (
              begin (
                let (
                  (
                    m1 (
                      compute_geometric_mean (
                        _list 2.0 8.0
                      )
                    )
                  )
                )
                 (
                  begin (
                    if (
                      _gt (
                        abs (
                          - m1 4.0
                        )
                      )
                       eps
                    )
                     (
                      begin (
                        panic "test1 failed"
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
                        m2 (
                          compute_geometric_mean (
                            _list 5.0 125.0
                          )
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          _gt (
                            abs (
                              - m2 25.0
                            )
                          )
                           eps
                        )
                         (
                          begin (
                            panic "test2 failed"
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
                            m3 (
                              compute_geometric_mean (
                                _list 1.0 0.0
                              )
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              _gt (
                                abs (
                                  - m3 0.0
                                )
                              )
                               eps
                            )
                             (
                              begin (
                                panic "test3 failed"
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
                                m4 (
                                  compute_geometric_mean (
                                    _list 1.0 5.0 25.0 5.0
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                if (
                                  _gt (
                                    abs (
                                      - m4 5.0
                                    )
                                  )
                                   eps
                                )
                                 (
                                  begin (
                                    panic "test4 failed"
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
                                    m5 (
                                      compute_geometric_mean (
                                        _list (
                                          - 5.0
                                        )
                                         25.0 1.0
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      _gt (
                                        abs (
                                          _add m5 5.0
                                        )
                                      )
                                       eps
                                    )
                                     (
                                      begin (
                                        panic "test5 failed"
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
              test_compute_geometric_mean
            )
             (
              _display (
                if (
                  string? (
                    compute_geometric_mean (
                      _list (
                        - 3.0
                      )
                       (
                        - 27.0
                      )
                    )
                  )
                )
                 (
                  compute_geometric_mean (
                    _list (
                      - 3.0
                    )
                     (
                      - 27.0
                    )
                  )
                )
                 (
                  to-str (
                    compute_geometric_mean (
                      _list (
                        - 3.0
                      )
                       (
                        - 27.0
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
      )
    )
     (
      main
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
