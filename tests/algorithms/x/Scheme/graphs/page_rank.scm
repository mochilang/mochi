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
      start21 (
        current-jiffy
      )
    )
     (
      jps24 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        node_to_string n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              string-append (
                string-append (
                  string-append (
                    string-append (
                      string-append (
                        string-append "<node=" (
                          hash-table-ref n "name"
                        )
                      )
                       " inbound="
                    )
                     (
                      hash-table-ref n "inbound"
                    )
                  )
                   " outbound="
                )
                 (
                  hash-table-ref n "outbound"
                )
              )
               ">"
            )
          )
        )
      )
    )
     (
      define (
        page_rank nodes limit d
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                ranks (
                  alist->hash-table (
                    _list
                  )
                )
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
                                    n (
                                      car xs
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    hash-table-set! ranks (
                                      hash-table-ref n "name"
                                    )
                                     1.0
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
                      loop3 nodes
                    )
                  )
                )
              )
               (
                let (
                  (
                    outbounds (
                      alist->hash-table (
                        _list
                      )
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
                                        n (
                                          car xs
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        hash-table-set! outbounds (
                                          hash-table-ref n "name"
                                        )
                                         (
                                          * 1.0 (
                                            _len (
                                              hash-table-ref n "outbound"
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop5 (
                                      cdr xs
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          loop5 nodes
                        )
                      )
                    )
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
                                      < i limit
                                    )
                                     (
                                      begin (
                                        _display (
                                          if (
                                            string? (
                                              string-append (
                                                string-append "======= Iteration " (
                                                  to-str-space (
                                                    + i 1
                                                  )
                                                )
                                              )
                                               " ======="
                                            )
                                          )
                                           (
                                            string-append (
                                              string-append "======= Iteration " (
                                                to-str-space (
                                                  + i 1
                                                )
                                              )
                                            )
                                             " ======="
                                          )
                                           (
                                            to-str (
                                              string-append (
                                                string-append "======= Iteration " (
                                                  to-str-space (
                                                    + i 1
                                                  )
                                                )
                                              )
                                               " ======="
                                            )
                                          )
                                        )
                                      )
                                       (
                                        newline
                                      )
                                       (
                                        call/cc (
                                          lambda (
                                            break10
                                          )
                                           (
                                            letrec (
                                              (
                                                loop9 (
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
                                                            n (
                                                              car xs
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                sum_val 0.0
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
                                                                                    ib (
                                                                                      car xs
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! sum_val (
                                                                                      _add sum_val (
                                                                                        _div (
                                                                                          hash-table-ref/default ranks ib (
                                                                                            quote (
                                                                                              
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref/default outbounds ib (
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
                                                                                loop11 (
                                                                                  cdr xs
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      loop11 (
                                                                        hash-table-ref n "inbound"
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                hash-table-set! ranks (
                                                                  hash-table-ref n "name"
                                                                )
                                                                 (
                                                                  _add (
                                                                    - 1.0 d
                                                                  )
                                                                   (
                                                                    * d sum_val
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        loop9 (
                                                          cdr xs
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              loop9 nodes
                                            )
                                          )
                                        )
                                      )
                                       (
                                        _display (
                                          if (
                                            string? ranks
                                          )
                                           ranks (
                                            to-str ranks
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
                        ret2 ranks
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
          names (
            _list "A" "B" "C"
          )
        )
      )
       (
        begin (
          let (
            (
              graph (
                _list (
                  _list 0 1 1
                )
                 (
                  _list 0 0 1
                )
                 (
                  _list 1 0 0
                )
              )
            )
          )
           (
            begin (
              let (
                (
                  nodes (
                    _list
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
                                      name (
                                        car xs
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      set! nodes (
                                        append nodes (
                                          _list (
                                            alist->hash-table (
                                              _list (
                                                cons "name" name
                                              )
                                               (
                                                cons "inbound" (
                                                  _list
                                                )
                                              )
                                               (
                                                cons "outbound" (
                                                  _list
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
                                  loop13 (
                                    cdr xs
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                       (
                        loop13 names
                      )
                    )
                  )
                )
                 (
                  let (
                    (
                      ri 0
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
                                  
                                )
                                 (
                                  if (
                                    < ri (
                                      _len graph
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          row (
                                            list-ref graph ri
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              ci 0
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
                                                            < ci (
                                                              _len row
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              if (
                                                                equal? (
                                                                  list-ref row ci
                                                                )
                                                                 1
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      n_in (
                                                                        list-ref nodes ci
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      hash-table-set! n_in "inbound" (
                                                                        append (
                                                                          hash-table-ref n_in "inbound"
                                                                        )
                                                                         (
                                                                          _list (
                                                                            list-ref names ri
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      list-set! nodes ci n_in
                                                                    )
                                                                     (
                                                                      let (
                                                                        (
                                                                          n_out (
                                                                            list-ref nodes ri
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          hash-table-set! n_out "outbound" (
                                                                            append (
                                                                              hash-table-ref n_out "outbound"
                                                                            )
                                                                             (
                                                                              _list (
                                                                                list-ref names ci
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          list-set! nodes ri n_out
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
                                                              set! ci (
                                                                + ci 1
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
                                              set! ri (
                                                + ri 1
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      loop15
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
                            loop15
                          )
                        )
                      )
                    )
                     (
                      _display (
                        if (
                          string? "======= Nodes ======="
                        )
                         "======= Nodes =======" (
                          to-str "======= Nodes ======="
                        )
                      )
                    )
                     (
                      newline
                    )
                     (
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
                                          n (
                                            car xs
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          _display (
                                            if (
                                              string? n
                                            )
                                             n (
                                              to-str n
                                            )
                                          )
                                        )
                                         (
                                          newline
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
                            loop19 nodes
                          )
                        )
                      )
                    )
                     (
                      page_rank nodes 3 0.85
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
          end22 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur23 (
              quotient (
                * (
                  - end22 start21
                )
                 1000000
              )
               jps24
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur23
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
