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
      start25 (
        current-jiffy
      )
    )
     (
      jps28 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        log2 x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                k 0.0
              )
            )
             (
              begin (
                let (
                  (
                    v x
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
                                  >= v 2.0
                                )
                                 (
                                  begin (
                                    set! v (
                                      _div v 2.0
                                    )
                                  )
                                   (
                                    set! k (
                                      + k 1.0
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
                                  < v 1.0
                                )
                                 (
                                  begin (
                                    set! v (
                                      * v 2.0
                                    )
                                  )
                                   (
                                    set! k (
                                      - k 1.0
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
                    let (
                      (
                        z (
                          _div (
                            - v 1.0
                          )
                           (
                            + v 1.0
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            zpow z
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                sum z
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    i 3
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
                                                  <= i 9
                                                )
                                                 (
                                                  begin (
                                                    set! zpow (
                                                      * (
                                                        * zpow z
                                                      )
                                                       z
                                                    )
                                                  )
                                                   (
                                                    set! sum (
                                                      _add sum (
                                                        _div zpow (
                                                          + 0.0 i
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! i (
                                                      + i 2
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
                                        ln2 0.6931471805599453
                                      )
                                    )
                                     (
                                      begin (
                                        ret1 (
                                          _add k (
                                            _div (
                                              * 2.0 sum
                                            )
                                             ln2
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
        analyze_text text
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                single (
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
                    double (
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
                        n (
                          _len text
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          equal? n 0
                        )
                         (
                          begin (
                            ret8 (
                              alist->hash-table (
                                _list (
                                  cons "single" single
                                )
                                 (
                                  cons "double" double
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
                            last (
                              _substring text (
                                - n 1
                              )
                               n
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              cond (
                                (
                                  string? single
                                )
                                 (
                                  if (
                                    string-contains single last
                                  )
                                   #t #f
                                )
                              )
                               (
                                (
                                  hash-table? single
                                )
                                 (
                                  if (
                                    hash-table-exists? single last
                                  )
                                   #t #f
                                )
                              )
                               (
                                else (
                                  if (
                                    member last single
                                  )
                                   #t #f
                                )
                              )
                            )
                             (
                              begin (
                                hash-table-set! single last (
                                  + (
                                    hash-table-ref/default single last (
                                      quote (
                                        
                                      )
                                    )
                                  )
                                   1
                                )
                              )
                            )
                             (
                              begin (
                                hash-table-set! single last 1
                              )
                            )
                          )
                           (
                            let (
                              (
                                first (
                                  _substring text 0 1
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    pair0 (
                                      string-append " " first
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    hash-table-set! double pair0 1
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
                                                        - n 1
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            ch (
                                                              _substring text i (
                                                                + i 1
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              cond (
                                                                (
                                                                  string? single
                                                                )
                                                                 (
                                                                  if (
                                                                    string-contains single ch
                                                                  )
                                                                   #t #f
                                                                )
                                                              )
                                                               (
                                                                (
                                                                  hash-table? single
                                                                )
                                                                 (
                                                                  if (
                                                                    hash-table-exists? single ch
                                                                  )
                                                                   #t #f
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  if (
                                                                    member ch single
                                                                  )
                                                                   #t #f
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                hash-table-set! single ch (
                                                                  + (
                                                                    hash-table-ref/default single ch (
                                                                      quote (
                                                                        
                                                                      )
                                                                    )
                                                                  )
                                                                   1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                hash-table-set! single ch 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            let (
                                                              (
                                                                seq (
                                                                  _substring text i (
                                                                    + i 2
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  cond (
                                                                    (
                                                                      string? double
                                                                    )
                                                                     (
                                                                      if (
                                                                        string-contains double seq
                                                                      )
                                                                       #t #f
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? double
                                                                    )
                                                                     (
                                                                      if (
                                                                        hash-table-exists? double seq
                                                                      )
                                                                       #t #f
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      if (
                                                                        member seq double
                                                                      )
                                                                       #t #f
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    hash-table-set! double seq (
                                                                      + (
                                                                        hash-table-ref/default double seq (
                                                                          quote (
                                                                            
                                                                          )
                                                                        )
                                                                      )
                                                                       1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    hash-table-set! double seq 1
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
                                          alist->hash-table (
                                            _list (
                                              cons "single" single
                                            )
                                             (
                                              cons "double" double
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
        round_to_int x
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            begin (
              if (
                < x 0.0
              )
               (
                begin (
                  ret11 (
                    let (
                      (
                        v12 (
                          - x 0.5
                        )
                      )
                    )
                     (
                      cond (
                        (
                          string? v12
                        )
                         (
                          inexact->exact (
                            floor (
                              string->number v12
                            )
                          )
                        )
                      )
                       (
                        (
                          boolean? v12
                        )
                         (
                          if v12 1 0
                        )
                      )
                       (
                        else (
                          inexact->exact (
                            floor v12
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
                let (
                  (
                    v13 (
                      + x 0.5
                    )
                  )
                )
                 (
                  cond (
                    (
                      string? v13
                    )
                     (
                      inexact->exact (
                        floor (
                          string->number v13
                        )
                      )
                    )
                  )
                   (
                    (
                      boolean? v13
                    )
                     (
                      if v13 1 0
                    )
                  )
                   (
                    else (
                      inexact->exact (
                        floor v13
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
        calculate_entropy text
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            let (
              (
                counts (
                  analyze_text text
                )
              )
            )
             (
              begin (
                let (
                  (
                    alphas " abcdefghijklmnopqrstuvwxyz"
                  )
                )
                 (
                  begin (
                    let (
                      (
                        total1 0
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
                                            ch (
                                              car xs
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! total1 (
                                              _add total1 (
                                                cond (
                                                  (
                                                    string? (
                                                      hash-table-ref counts "single"
                                                    )
                                                  )
                                                   (
                                                    _substring (
                                                      hash-table-ref counts "single"
                                                    )
                                                     ch (
                                                      + ch 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? (
                                                      hash-table-ref counts "single"
                                                    )
                                                  )
                                                   (
                                                    hash-table-ref (
                                                      hash-table-ref counts "single"
                                                    )
                                                     ch
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref (
                                                      hash-table-ref counts "single"
                                                    )
                                                     ch
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop15 (
                                          cdr xs
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop15 (
                                hash-table-ref counts "single"
                              )
                            )
                          )
                        )
                      )
                       (
                        let (
                          (
                            h1 0.0
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
                                                _len alphas
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    ch (
                                                      _substring alphas i (
                                                        + i 1
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      cond (
                                                        (
                                                          string? (
                                                            hash-table-ref counts "single"
                                                          )
                                                        )
                                                         (
                                                          if (
                                                            string-contains (
                                                              hash-table-ref counts "single"
                                                            )
                                                             ch
                                                          )
                                                           #t #f
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? (
                                                            hash-table-ref counts "single"
                                                          )
                                                        )
                                                         (
                                                          if (
                                                            hash-table-exists? (
                                                              hash-table-ref counts "single"
                                                            )
                                                             ch
                                                          )
                                                           #t #f
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          if (
                                                            member ch (
                                                              hash-table-ref counts "single"
                                                            )
                                                          )
                                                           #t #f
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            prob (
                                                              _div (
                                                                + 0.0 (
                                                                  cond (
                                                                    (
                                                                      string? (
                                                                        hash-table-ref counts "single"
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        hash-table-ref counts "single"
                                                                      )
                                                                       ch (
                                                                        + ch 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        hash-table-ref counts "single"
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        hash-table-ref counts "single"
                                                                      )
                                                                       ch
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref (
                                                                        hash-table-ref counts "single"
                                                                      )
                                                                       ch
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                + 0.0 total1
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! h1 (
                                                              _add h1 (
                                                                * prob (
                                                                  log2 prob
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
                                                    set! i (
                                                      + i 1
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
                                let (
                                  (
                                    first_entropy (
                                      - h1
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    _display (
                                      if (
                                        string? (
                                          string-append (
                                            to-str-space (
                                              round_to_int first_entropy
                                            )
                                          )
                                           ".0"
                                        )
                                      )
                                       (
                                        string-append (
                                          to-str-space (
                                            round_to_int first_entropy
                                          )
                                        )
                                         ".0"
                                      )
                                       (
                                        to-str (
                                          string-append (
                                            to-str-space (
                                              round_to_int first_entropy
                                            )
                                          )
                                           ".0"
                                        )
                                      )
                                    )
                                  )
                                   (
                                    newline
                                  )
                                   (
                                    let (
                                      (
                                        total2 0
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
                                                            seq (
                                                              car xs
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! total2 (
                                                              _add total2 (
                                                                cond (
                                                                  (
                                                                    string? (
                                                                      hash-table-ref counts "double"
                                                                    )
                                                                  )
                                                                   (
                                                                    _substring (
                                                                      hash-table-ref counts "double"
                                                                    )
                                                                     seq (
                                                                      + seq 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? (
                                                                      hash-table-ref counts "double"
                                                                    )
                                                                  )
                                                                   (
                                                                    hash-table-ref (
                                                                      hash-table-ref counts "double"
                                                                    )
                                                                     seq
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref (
                                                                      hash-table-ref counts "double"
                                                                    )
                                                                     seq
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        loop19 (
                                                          cdr xs
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              loop19 (
                                                hash-table-ref counts "double"
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        let (
                                          (
                                            h2 0.0
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                a0 0
                                              )
                                            )
                                             (
                                              begin (
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
                                                              < a0 (
                                                                _len alphas
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    ch0 (
                                                                      _substring alphas a0 (
                                                                        + a0 1
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        a1 0
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
                                                                                      < a1 (
                                                                                        _len alphas
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            ch1 (
                                                                                              _substring alphas a1 (
                                                                                                + a1 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                seq (
                                                                                                  string-append ch0 ch1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                if (
                                                                                                  cond (
                                                                                                    (
                                                                                                      string? (
                                                                                                        hash-table-ref counts "double"
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      if (
                                                                                                        string-contains (
                                                                                                          hash-table-ref counts "double"
                                                                                                        )
                                                                                                         seq
                                                                                                      )
                                                                                                       #t #f
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    (
                                                                                                      hash-table? (
                                                                                                        hash-table-ref counts "double"
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      if (
                                                                                                        hash-table-exists? (
                                                                                                          hash-table-ref counts "double"
                                                                                                        )
                                                                                                         seq
                                                                                                      )
                                                                                                       #t #f
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    else (
                                                                                                      if (
                                                                                                        member seq (
                                                                                                          hash-table-ref counts "double"
                                                                                                        )
                                                                                                      )
                                                                                                       #t #f
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    let (
                                                                                                      (
                                                                                                        prob (
                                                                                                          _div (
                                                                                                            + 0.0 (
                                                                                                              cond (
                                                                                                                (
                                                                                                                  string? (
                                                                                                                    hash-table-ref counts "double"
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  _substring (
                                                                                                                    hash-table-ref counts "double"
                                                                                                                  )
                                                                                                                   seq (
                                                                                                                    + seq 1
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                (
                                                                                                                  hash-table? (
                                                                                                                    hash-table-ref counts "double"
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  hash-table-ref (
                                                                                                                    hash-table-ref counts "double"
                                                                                                                  )
                                                                                                                   seq
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                else (
                                                                                                                  list-ref (
                                                                                                                    hash-table-ref counts "double"
                                                                                                                  )
                                                                                                                   seq
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            + 0.0 total2
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        set! h2 (
                                                                                                          _add h2 (
                                                                                                            * prob (
                                                                                                              log2 prob
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
                                                                                                set! a1 (
                                                                                                  + a1 1
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
                                                                        set! a0 (
                                                                          + a0 1
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
                                                    second_entropy (
                                                      - h2
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    _display (
                                                      if (
                                                        string? (
                                                          string-append (
                                                            to-str-space (
                                                              round_to_int second_entropy
                                                            )
                                                          )
                                                           ".0"
                                                        )
                                                      )
                                                       (
                                                        string-append (
                                                          to-str-space (
                                                            round_to_int second_entropy
                                                          )
                                                        )
                                                         ".0"
                                                      )
                                                       (
                                                        to-str (
                                                          string-append (
                                                            to-str-space (
                                                              round_to_int second_entropy
                                                            )
                                                          )
                                                           ".0"
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    newline
                                                  )
                                                   (
                                                    let (
                                                      (
                                                        diff (
                                                          - second_entropy first_entropy
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        _display (
                                                          if (
                                                            string? (
                                                              string-append (
                                                                to-str-space (
                                                                  round_to_int diff
                                                                )
                                                              )
                                                               ".0"
                                                            )
                                                          )
                                                           (
                                                            string-append (
                                                              to-str-space (
                                                                round_to_int diff
                                                              )
                                                            )
                                                             ".0"
                                                          )
                                                           (
                                                            to-str (
                                                              string-append (
                                                                to-str-space (
                                                                  round_to_int diff
                                                                )
                                                              )
                                                               ".0"
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
          )
        )
      )
    )
     (
      let (
        (
          text1 "Behind Winston's back the voice from the telescreen was still babbling and the overfulfilment"
        )
      )
       (
        begin (
          calculate_entropy text1
        )
         (
          let (
            (
              text3 "Had repulsive dashwoods suspicion sincerity but advantage now him. Remark easily garret nor nay.  Civil those mrs enjoy shy fat merry. You greatest jointure saw horrible. He private he on be imagine suppose. Fertile beloved evident through no service elderly is. Blind there if every no so at. Own neglected you preferred way sincerity delivered his attempted. To of message cottage windows do besides against uncivil.  Delightful unreserved impossible few estimating men favourable see entreaties. She propriety immediate was improving. He or entrance humoured likewise moderate. Much nor game son say feel. Fat make met can must form into gate. Me we offending prevailed discovery."
            )
          )
           (
            begin (
              calculate_entropy text3
            )
          )
        )
      )
    )
     (
      let (
        (
          end26 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur27 (
              quotient (
                * (
                  - end26 start25
                )
                 1000000
              )
               jps28
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur27
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
