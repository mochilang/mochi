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
      start32 (
        current-jiffy
      )
    )
     (
      jps35 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define OP_NODE 0
    )
     (
      define OP_LEAF 1
    )
     (
      define (
        get_freq n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              let (
                (
                  match2 n
                )
              )
               (
                if (
                  equal? (
                    hash-table-ref match2 "__tag"
                  )
                   OP_LEAF
                )
                 (
                  let (
                    (
                      f (
                        hash-table-ref match2 "freq"
                      )
                    )
                  )
                   f
                )
                 (
                  if (
                    equal? (
                      hash-table-ref match2 "__tag"
                    )
                     OP_NODE
                  )
                   (
                    let (
                      (
                        f (
                          hash-table-ref match2 "freq"
                        )
                      )
                    )
                     f
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
        sort_nodes nodes
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            let (
              (
                arr nodes
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
                                  < i (
                                    _len arr
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        key (
                                          list-ref arr i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            j (
                                              - i 1
                                            )
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
                                                          and (
                                                            >= j 0
                                                          )
                                                           (
                                                            _gt (
                                                              get_freq (
                                                                list-ref arr j
                                                              )
                                                            )
                                                             (
                                                              get_freq key
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            list-set! arr (
                                                              + j 1
                                                            )
                                                             (
                                                              list-ref arr j
                                                            )
                                                          )
                                                           (
                                                            set! j (
                                                              - j 1
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
                                            list-set! arr (
                                              + j 1
                                            )
                                             key
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
                    ret3 arr
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
        rest nodes
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                res (
                  _list
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
                                    _len nodes
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list (
                                          list-ref nodes i
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
                    ret8 res
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
        count_freq text
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            let (
              (
                chars (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    freqs (
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
                                        _len text
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            c (
                                              _substring text i (
                                                + i 1
                                              )
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
                                                let (
                                                  (
                                                    found #f
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
                                                                  < j (
                                                                    _len chars
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      string=? (
                                                                        list-ref chars j
                                                                      )
                                                                       c
                                                                    )
                                                                     (
                                                                      begin (
                                                                        list-set! freqs j (
                                                                          + (
                                                                            list-ref freqs j
                                                                          )
                                                                           1
                                                                        )
                                                                      )
                                                                       (
                                                                        set! found #t
                                                                      )
                                                                       (
                                                                        break15 (
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
                                                    if (
                                                      not found
                                                    )
                                                     (
                                                      begin (
                                                        set! chars (
                                                          append chars (
                                                            _list c
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! freqs (
                                                          append freqs (
                                                            _list 1
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
                                            )
                                          )
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
                        let (
                          (
                            leaves (
                              _list
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
                                    break17
                                  )
                                   (
                                    letrec (
                                      (
                                        loop16 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < k (
                                                _len chars
                                              )
                                            )
                                             (
                                              begin (
                                                set! leaves (
                                                  append leaves (
                                                    _list (
                                                      alist->hash-table (
                                                        _list (
                                                          cons "__tag" OP_LEAF
                                                        )
                                                         (
                                                          cons "symbol" (
                                                            list-ref chars k
                                                          )
                                                        )
                                                         (
                                                          cons "freq" (
                                                            list-ref freqs k
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! k (
                                                  + k 1
                                                )
                                              )
                                               (
                                                loop16
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
                                      loop16
                                    )
                                  )
                                )
                              )
                               (
                                ret11 (
                                  sort_nodes leaves
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
        build_tree nodes
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            let (
              (
                arr nodes
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
                              > (
                                _len arr
                              )
                               1
                            )
                             (
                              begin (
                                let (
                                  (
                                    left (
                                      list-ref arr 0
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! arr (
                                      rest arr
                                    )
                                  )
                                   (
                                    let (
                                      (
                                        right (
                                          list-ref arr 0
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! arr (
                                          rest arr
                                        )
                                      )
                                       (
                                        let (
                                          (
                                            node (
                                              alist->hash-table (
                                                _list (
                                                  cons "__tag" OP_NODE
                                                )
                                                 (
                                                  cons "freq" (
                                                    _add (
                                                      get_freq left
                                                    )
                                                     (
                                                      get_freq right
                                                    )
                                                  )
                                                )
                                                 (
                                                  cons "left" left
                                                )
                                                 (
                                                  cons "right" right
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! arr (
                                              append arr (
                                                _list node
                                              )
                                            )
                                          )
                                           (
                                            set! arr (
                                              sort_nodes arr
                                            )
                                          )
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
                ret18 (
                  list-ref arr 0
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        concat_pairs a b
      )
       (
        call/cc (
          lambda (
            ret21
          )
           (
            let (
              (
                res a
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
                        break23
                      )
                       (
                        letrec (
                          (
                            loop22 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len b
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list (
                                          list-ref b i
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
                                    loop22
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
                          loop22
                        )
                      )
                    )
                  )
                   (
                    ret21 res
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
        collect_codes tree prefix
      )
       (
        call/cc (
          lambda (
            ret24
          )
           (
            ret24 (
              let (
                (
                  match25 tree
                )
              )
               (
                if (
                  equal? (
                    hash-table-ref match25 "__tag"
                  )
                   OP_LEAF
                )
                 (
                  let (
                    (
                      s (
                        hash-table-ref match25 "symbol"
                      )
                    )
                  )
                   (
                    _list (
                      _list s prefix
                    )
                  )
                )
                 (
                  if (
                    equal? (
                      hash-table-ref match25 "__tag"
                    )
                     OP_NODE
                  )
                   (
                    let (
                      (
                        l (
                          hash-table-ref match25 "left"
                        )
                      )
                       (
                        r (
                          hash-table-ref match25 "right"
                        )
                      )
                    )
                     (
                      concat_pairs (
                        collect_codes l (
                          string-append prefix "0"
                        )
                      )
                       (
                        collect_codes r (
                          string-append prefix "1"
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
      define (
        find_code pairs ch
      )
       (
        call/cc (
          lambda (
            ret26
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
                    break28
                  )
                   (
                    letrec (
                      (
                        loop27 (
                          lambda (
                            
                          )
                           (
                            if (
                              < i (
                                _len pairs
                              )
                            )
                             (
                              begin (
                                if (
                                  string=? (
                                    cond (
                                      (
                                        string? (
                                          list-ref pairs i
                                        )
                                      )
                                       (
                                        _substring (
                                          list-ref pairs i
                                        )
                                         0 (
                                          + 0 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? (
                                          list-ref pairs i
                                        )
                                      )
                                       (
                                        hash-table-ref (
                                          list-ref pairs i
                                        )
                                         0
                                      )
                                    )
                                     (
                                      else (
                                        list-ref (
                                          list-ref pairs i
                                        )
                                         0
                                      )
                                    )
                                  )
                                   ch
                                )
                                 (
                                  begin (
                                    ret26 (
                                      cond (
                                        (
                                          string? (
                                            list-ref pairs i
                                          )
                                        )
                                         (
                                          _substring (
                                            list-ref pairs i
                                          )
                                           1 (
                                            + 1 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? (
                                            list-ref pairs i
                                          )
                                        )
                                         (
                                          hash-table-ref (
                                            list-ref pairs i
                                          )
                                           1
                                        )
                                      )
                                       (
                                        else (
                                          list-ref (
                                            list-ref pairs i
                                          )
                                           1
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
                                loop27
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
                      loop27
                    )
                  )
                )
              )
               (
                ret26 ""
              )
            )
          )
        )
      )
    )
     (
      define (
        huffman_encode text
      )
       (
        call/cc (
          lambda (
            ret29
          )
           (
            begin (
              if (
                string=? text ""
              )
               (
                begin (
                  ret29 ""
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
                  leaves (
                    count_freq text
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      tree (
                        build_tree leaves
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          codes (
                            collect_codes tree ""
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              encoded ""
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
                                      break31
                                    )
                                     (
                                      letrec (
                                        (
                                          loop30 (
                                            lambda (
                                              
                                            )
                                             (
                                              if (
                                                < i (
                                                  _len text
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      c (
                                                        _substring text i (
                                                          + i 1
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! encoded (
                                                        string-append (
                                                          string-append encoded (
                                                            find_code codes c
                                                          )
                                                        )
                                                         " "
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
                                                  loop30
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
                                        loop30
                                      )
                                    )
                                  )
                                )
                                 (
                                  ret29 encoded
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
      _display (
        if (
          string? (
            huffman_encode "beep boop beer!"
          )
        )
         (
          huffman_encode "beep boop beer!"
        )
         (
          to-str (
            huffman_encode "beep boop beer!"
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
          end33 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur34 (
              quotient (
                * (
                  - end33 start32
                )
                 1000000
              )
               jps35
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur34
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
