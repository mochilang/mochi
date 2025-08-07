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
      start31 (
        current-jiffy
      )
    )
     (
      jps34 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        create_graph vertices edges directed
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                adj (
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
                                    v (
                                      car xs
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    hash-table-set! adj v (
                                      _list
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
                      loop2 vertices
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
                                    e (
                                      car xs
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        s (
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
                                            d (
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
                                                cond (
                                                  (
                                                    string? adj
                                                  )
                                                   (
                                                    if (
                                                      string-contains adj s
                                                    )
                                                     #t #f
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? adj
                                                  )
                                                   (
                                                    if (
                                                      hash-table-exists? adj s
                                                    )
                                                     #t #f
                                                  )
                                                )
                                                 (
                                                  else (
                                                    if (
                                                      member s adj
                                                    )
                                                     #t #f
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                hash-table-set! adj s (
                                                  _list
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
                                                cond (
                                                  (
                                                    string? adj
                                                  )
                                                   (
                                                    if (
                                                      string-contains adj d
                                                    )
                                                     #t #f
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? adj
                                                  )
                                                   (
                                                    if (
                                                      hash-table-exists? adj d
                                                    )
                                                     #t #f
                                                  )
                                                )
                                                 (
                                                  else (
                                                    if (
                                                      member d adj
                                                    )
                                                     #t #f
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                hash-table-set! adj d (
                                                  _list
                                                )
                                              )
                                            )
                                             (
                                              quote (
                                                
                                              )
                                            )
                                          )
                                           (
                                            hash-table-set! adj s (
                                              append (
                                                hash-table-ref/default adj s (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                               (
                                                _list d
                                              )
                                            )
                                          )
                                           (
                                            if (
                                              not directed
                                            )
                                             (
                                              begin (
                                                hash-table-set! adj d (
                                                  append (
                                                    hash-table-ref/default adj d (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                  )
                                                   (
                                                    _list s
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
                                loop4 (
                                  cdr xs
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      loop4 edges
                    )
                  )
                )
              )
               (
                ret1 (
                  alist->hash-table (
                    _list (
                      cons "adj" adj
                    )
                     (
                      cons "directed" directed
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
        add_vertex graph v
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            begin (
              if (
                cond (
                  (
                    string? (
                      hash-table-ref graph "adj"
                    )
                  )
                   (
                    if (
                      string-contains (
                        hash-table-ref graph "adj"
                      )
                       v
                    )
                     #t #f
                  )
                )
                 (
                  (
                    hash-table? (
                      hash-table-ref graph "adj"
                    )
                  )
                   (
                    if (
                      hash-table-exists? (
                        hash-table-ref graph "adj"
                      )
                       v
                    )
                     #t #f
                  )
                )
                 (
                  else (
                    if (
                      member v (
                        hash-table-ref graph "adj"
                      )
                    )
                     #t #f
                  )
                )
              )
               (
                begin (
                  panic "vertex exists"
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
                  adj (
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
                                      k (
                                        car xs
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      hash-table-set! adj k (
                                        hash-table-ref/default (
                                          hash-table-ref graph "adj"
                                        )
                                         k (
                                          quote (
                                            
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
                          hash-table-keys (
                            hash-table-ref graph "adj"
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  hash-table-set! adj v (
                    _list
                  )
                )
                 (
                  ret6 (
                    alist->hash-table (
                      _list (
                        cons "adj" adj
                      )
                       (
                        cons "directed" (
                          hash-table-ref graph "directed"
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
        remove_from_list lst value
      )
       (
        call/cc (
          lambda (
            ret9
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
                                    _len lst
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      not (
                                        string=? (
                                          list-ref lst i
                                        )
                                         value
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              list-ref lst i
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
                    ret9 res
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
        remove_key m key
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                res (
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
                                    k (
                                      car xs
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      not (
                                        string=? k key
                                      )
                                    )
                                     (
                                      begin (
                                        hash-table-set! res k (
                                          hash-table-ref/default m k (
                                            quote (
                                              
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
                      loop13 (
                        hash-table-keys m
                      )
                    )
                  )
                )
              )
               (
                ret12 res
              )
            )
          )
        )
      )
    )
     (
      define (
        add_edge graph s d
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            begin (
              if (
                or (
                  not (
                    cond (
                      (
                        string? (
                          hash-table-ref graph "adj"
                        )
                      )
                       (
                        if (
                          string-contains (
                            hash-table-ref graph "adj"
                          )
                           s
                        )
                         #t #f
                      )
                    )
                     (
                      (
                        hash-table? (
                          hash-table-ref graph "adj"
                        )
                      )
                       (
                        if (
                          hash-table-exists? (
                            hash-table-ref graph "adj"
                          )
                           s
                        )
                         #t #f
                      )
                    )
                     (
                      else (
                        if (
                          member s (
                            hash-table-ref graph "adj"
                          )
                        )
                         #t #f
                      )
                    )
                  )
                )
                 (
                  not (
                    cond (
                      (
                        string? (
                          hash-table-ref graph "adj"
                        )
                      )
                       (
                        if (
                          string-contains (
                            hash-table-ref graph "adj"
                          )
                           d
                        )
                         #t #f
                      )
                    )
                     (
                      (
                        hash-table? (
                          hash-table-ref graph "adj"
                        )
                      )
                       (
                        if (
                          hash-table-exists? (
                            hash-table-ref graph "adj"
                          )
                           d
                        )
                         #t #f
                      )
                    )
                     (
                      else (
                        if (
                          member d (
                            hash-table-ref graph "adj"
                          )
                        )
                         #t #f
                      )
                    )
                  )
                )
              )
               (
                begin (
                  panic "vertex missing"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                contains_edge graph s d
              )
               (
                begin (
                  panic "edge exists"
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
                  adj (
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
                      break17
                    )
                     (
                      letrec (
                        (
                          loop16 (
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
                                      k (
                                        car xs
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      hash-table-set! adj k (
                                        hash-table-ref/default (
                                          hash-table-ref graph "adj"
                                        )
                                         k (
                                          quote (
                                            
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  loop16 (
                                    cdr xs
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                       (
                        loop16 (
                          hash-table-keys (
                            hash-table-ref graph "adj"
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  let (
                    (
                      list_s (
                        hash-table-ref/default adj s (
                          quote (
                            
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      set! list_s (
                        append list_s (
                          _list d
                        )
                      )
                    )
                     (
                      hash-table-set! adj s list_s
                    )
                     (
                      if (
                        not (
                          hash-table-ref graph "directed"
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              list_d (
                                hash-table-ref/default adj d (
                                  quote (
                                    
                                  )
                                )
                              )
                            )
                          )
                           (
                            begin (
                              set! list_d (
                                append list_d (
                                  _list s
                                )
                              )
                            )
                             (
                              hash-table-set! adj d list_d
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
                      ret15 (
                        alist->hash-table (
                          _list (
                            cons "adj" adj
                          )
                           (
                            cons "directed" (
                              hash-table-ref graph "directed"
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
        remove_edge graph s d
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            begin (
              if (
                or (
                  not (
                    cond (
                      (
                        string? (
                          hash-table-ref graph "adj"
                        )
                      )
                       (
                        if (
                          string-contains (
                            hash-table-ref graph "adj"
                          )
                           s
                        )
                         #t #f
                      )
                    )
                     (
                      (
                        hash-table? (
                          hash-table-ref graph "adj"
                        )
                      )
                       (
                        if (
                          hash-table-exists? (
                            hash-table-ref graph "adj"
                          )
                           s
                        )
                         #t #f
                      )
                    )
                     (
                      else (
                        if (
                          member s (
                            hash-table-ref graph "adj"
                          )
                        )
                         #t #f
                      )
                    )
                  )
                )
                 (
                  not (
                    cond (
                      (
                        string? (
                          hash-table-ref graph "adj"
                        )
                      )
                       (
                        if (
                          string-contains (
                            hash-table-ref graph "adj"
                          )
                           d
                        )
                         #t #f
                      )
                    )
                     (
                      (
                        hash-table? (
                          hash-table-ref graph "adj"
                        )
                      )
                       (
                        if (
                          hash-table-exists? (
                            hash-table-ref graph "adj"
                          )
                           d
                        )
                         #t #f
                      )
                    )
                     (
                      else (
                        if (
                          member d (
                            hash-table-ref graph "adj"
                          )
                        )
                         #t #f
                      )
                    )
                  )
                )
              )
               (
                begin (
                  panic "vertex missing"
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
                  contains_edge graph s d
                )
              )
               (
                begin (
                  panic "edge missing"
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
                  adj (
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
                                      k (
                                        car xs
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      hash-table-set! adj k (
                                        hash-table-ref/default (
                                          hash-table-ref graph "adj"
                                        )
                                         k (
                                          quote (
                                            
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
                          hash-table-keys (
                            hash-table-ref graph "adj"
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  hash-table-set! adj s (
                    remove_from_list (
                      hash-table-ref/default adj s (
                        quote (
                          
                        )
                      )
                    )
                     d
                  )
                )
                 (
                  if (
                    not (
                      hash-table-ref graph "directed"
                    )
                  )
                   (
                    begin (
                      hash-table-set! adj d (
                        remove_from_list (
                          hash-table-ref/default adj d (
                            quote (
                              
                            )
                          )
                        )
                         s
                      )
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  ret18 (
                    alist->hash-table (
                      _list (
                        cons "adj" adj
                      )
                       (
                        cons "directed" (
                          hash-table-ref graph "directed"
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
        remove_vertex graph v
      )
       (
        call/cc (
          lambda (
            ret21
          )
           (
            begin (
              if (
                not (
                  cond (
                    (
                      string? (
                        hash-table-ref graph "adj"
                      )
                    )
                     (
                      if (
                        string-contains (
                          hash-table-ref graph "adj"
                        )
                         v
                      )
                       #t #f
                    )
                  )
                   (
                    (
                      hash-table? (
                        hash-table-ref graph "adj"
                      )
                    )
                     (
                      if (
                        hash-table-exists? (
                          hash-table-ref graph "adj"
                        )
                         v
                      )
                       #t #f
                    )
                  )
                   (
                    else (
                      if (
                        member v (
                          hash-table-ref graph "adj"
                        )
                      )
                       #t #f
                    )
                  )
                )
              )
               (
                begin (
                  panic "vertex missing"
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
                  adj (
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
                      break23
                    )
                     (
                      letrec (
                        (
                          loop22 (
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
                                      k (
                                        car xs
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      if (
                                        not (
                                          string=? k v
                                        )
                                      )
                                       (
                                        begin (
                                          hash-table-set! adj k (
                                            remove_from_list (
                                              hash-table-ref/default (
                                                hash-table-ref graph "adj"
                                              )
                                               k (
                                                quote (
                                                  
                                                )
                                              )
                                            )
                                             v
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
                                  loop22 (
                                    cdr xs
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                       (
                        loop22 (
                          hash-table-keys (
                            hash-table-ref graph "adj"
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  ret21 (
                    alist->hash-table (
                      _list (
                        cons "adj" adj
                      )
                       (
                        cons "directed" (
                          hash-table-ref graph "directed"
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
        contains_vertex graph v
      )
       (
        call/cc (
          lambda (
            ret24
          )
           (
            ret24 (
              cond (
                (
                  string? (
                    hash-table-ref graph "adj"
                  )
                )
                 (
                  if (
                    string-contains (
                      hash-table-ref graph "adj"
                    )
                     v
                  )
                   #t #f
                )
              )
               (
                (
                  hash-table? (
                    hash-table-ref graph "adj"
                  )
                )
                 (
                  if (
                    hash-table-exists? (
                      hash-table-ref graph "adj"
                    )
                     v
                  )
                   #t #f
                )
              )
               (
                else (
                  if (
                    member v (
                      hash-table-ref graph "adj"
                    )
                  )
                   #t #f
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        contains_edge graph s d
      )
       (
        call/cc (
          lambda (
            ret25
          )
           (
            begin (
              if (
                or (
                  not (
                    cond (
                      (
                        string? (
                          hash-table-ref graph "adj"
                        )
                      )
                       (
                        if (
                          string-contains (
                            hash-table-ref graph "adj"
                          )
                           s
                        )
                         #t #f
                      )
                    )
                     (
                      (
                        hash-table? (
                          hash-table-ref graph "adj"
                        )
                      )
                       (
                        if (
                          hash-table-exists? (
                            hash-table-ref graph "adj"
                          )
                           s
                        )
                         #t #f
                      )
                    )
                     (
                      else (
                        if (
                          member s (
                            hash-table-ref graph "adj"
                          )
                        )
                         #t #f
                      )
                    )
                  )
                )
                 (
                  not (
                    cond (
                      (
                        string? (
                          hash-table-ref graph "adj"
                        )
                      )
                       (
                        if (
                          string-contains (
                            hash-table-ref graph "adj"
                          )
                           d
                        )
                         #t #f
                      )
                    )
                     (
                      (
                        hash-table? (
                          hash-table-ref graph "adj"
                        )
                      )
                       (
                        if (
                          hash-table-exists? (
                            hash-table-ref graph "adj"
                          )
                           d
                        )
                         #t #f
                      )
                    )
                     (
                      else (
                        if (
                          member d (
                            hash-table-ref graph "adj"
                          )
                        )
                         #t #f
                      )
                    )
                  )
                )
              )
               (
                begin (
                  panic "vertex missing"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              call/cc (
                lambda (
                  break27
                )
                 (
                  letrec (
                    (
                      loop26 (
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
                                    string=? x d
                                  )
                                   (
                                    begin (
                                      ret25 #t
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
                              loop26 (
                                cdr xs
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                   (
                    loop26 (
                      hash-table-ref/default (
                        hash-table-ref graph "adj"
                      )
                       s (
                        quote (
                          
                        )
                      )
                    )
                  )
                )
              )
            )
             (
              ret25 #f
            )
          )
        )
      )
    )
     (
      define (
        clear_graph graph
      )
       (
        call/cc (
          lambda (
            ret28
          )
           (
            ret28 (
              alist->hash-table (
                _list (
                  cons "adj" (
                    alist->hash-table (
                      _list
                    )
                  )
                )
                 (
                  cons "directed" (
                    hash-table-ref graph "directed"
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
        to_string graph
      )
       (
        call/cc (
          lambda (
            ret29
          )
           (
            ret29 (
              to-str-space (
                hash-table-ref graph "adj"
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
            ret30
          )
           (
            let (
              (
                vertices (
                  _list "1" "2" "3" "4"
                )
              )
            )
             (
              begin (
                let (
                  (
                    edges (
                      _list (
                        _list "1" "2"
                      )
                       (
                        _list "2" "3"
                      )
                       (
                        _list "3" "4"
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        g (
                          create_graph vertices edges #f
                        )
                      )
                    )
                     (
                      begin (
                        _display (
                          if (
                            string? (
                              to_string g
                            )
                          )
                           (
                            to_string g
                          )
                           (
                            to-str (
                              to_string g
                            )
                          )
                        )
                      )
                       (
                        newline
                      )
                       (
                        set! g (
                          add_vertex g "5"
                        )
                      )
                       (
                        set! g (
                          add_edge g "4" "5"
                        )
                      )
                       (
                        _display (
                          if (
                            string? (
                              to-str-space (
                                contains_edge g "4" "5"
                              )
                            )
                          )
                           (
                            to-str-space (
                              contains_edge g "4" "5"
                            )
                          )
                           (
                            to-str (
                              to-str-space (
                                contains_edge g "4" "5"
                              )
                            )
                          )
                        )
                      )
                       (
                        newline
                      )
                       (
                        set! g (
                          remove_edge g "1" "2"
                        )
                      )
                       (
                        set! g (
                          remove_vertex g "3"
                        )
                      )
                       (
                        _display (
                          if (
                            string? (
                              to_string g
                            )
                          )
                           (
                            to_string g
                          )
                           (
                            to-str (
                              to_string g
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
     (
      main
    )
     (
      let (
        (
          end32 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur33 (
              quotient (
                * (
                  - end32 start31
                )
                 1000000
              )
               jps34
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur33
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
