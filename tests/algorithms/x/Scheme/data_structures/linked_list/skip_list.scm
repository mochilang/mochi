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
      start40 (
        current-jiffy
      )
    )
     (
      jps43 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          NIL (
            - 0 1
          )
        )
      )
       (
        begin (
          let (
            (
              MAX_LEVEL 6
            )
          )
           (
            begin (
              let (
                (
                  P 0.5
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
                        random
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
                                    * seed 13
                                  )
                                   7
                                )
                                 100
                              )
                            )
                             (
                              ret1 (
                                _div (
                                  + 0.0 seed
                                )
                                 100.0
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      define (
                        random_level
                      )
                       (
                        call/cc (
                          lambda (
                            ret2
                          )
                           (
                            let (
                              (
                                lvl 1
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
                                              and (
                                                _lt (
                                                  random
                                                )
                                                 P
                                              )
                                               (
                                                < lvl MAX_LEVEL
                                              )
                                            )
                                             (
                                              begin (
                                                set! lvl (
                                                  + lvl 1
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
                                ret2 lvl
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      define (
                        empty_forward
                      )
                       (
                        call/cc (
                          lambda (
                            ret5
                          )
                           (
                            let (
                              (
                                f (
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
                                                  < i MAX_LEVEL
                                                )
                                                 (
                                                  begin (
                                                    set! f (
                                                      append f (
                                                        _list NIL
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
                                    ret5 f
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
                          node_keys (
                            _list
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              node_vals (
                                _list
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  node_forwards (
                                    _list
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      level 1
                                    )
                                  )
                                   (
                                    begin (
                                      define (
                                        init
                                      )
                                       (
                                        call/cc (
                                          lambda (
                                            ret8
                                          )
                                           (
                                            begin (
                                              set! node_keys (
                                                _list (
                                                  - 1
                                                )
                                              )
                                            )
                                             (
                                              set! node_vals (
                                                _list 0
                                              )
                                            )
                                             (
                                              set! node_forwards (
                                                _list (
                                                  empty_forward
                                                )
                                              )
                                            )
                                             (
                                              set! level 1
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      define (
                                        insert key value
                                      )
                                       (
                                        call/cc (
                                          lambda (
                                            ret9
                                          )
                                           (
                                            let (
                                              (
                                                update (
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
                                                                  < i MAX_LEVEL
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! update (
                                                                      append update (
                                                                        _list 0
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
                                                    let (
                                                      (
                                                        x 0
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! i (
                                                          - level 1
                                                        )
                                                      )
                                                       (
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
                                                                      >= i 0
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
                                                                                      and (
                                                                                        not (
                                                                                          equal? (
                                                                                            cond (
                                                                                              (
                                                                                                string? (
                                                                                                  list-ref node_forwards x
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _substring (
                                                                                                  list-ref node_forwards x
                                                                                                )
                                                                                                 i (
                                                                                                  + i 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? (
                                                                                                  list-ref node_forwards x
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref (
                                                                                                  list-ref node_forwards x
                                                                                                )
                                                                                                 i
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref (
                                                                                                  list-ref node_forwards x
                                                                                                )
                                                                                                 i
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           NIL
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        < (
                                                                                          list-ref node_keys (
                                                                                            cond (
                                                                                              (
                                                                                                string? (
                                                                                                  list-ref node_forwards x
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _substring (
                                                                                                  list-ref node_forwards x
                                                                                                )
                                                                                                 i (
                                                                                                  + i 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? (
                                                                                                  list-ref node_forwards x
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref (
                                                                                                  list-ref node_forwards x
                                                                                                )
                                                                                                 i
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref (
                                                                                                  list-ref node_forwards x
                                                                                                )
                                                                                                 i
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         key
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! x (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                list-ref node_forwards x
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref node_forwards x
                                                                                              )
                                                                                               i (
                                                                                                + i 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref node_forwards x
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref node_forwards x
                                                                                              )
                                                                                               i
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref node_forwards x
                                                                                              )
                                                                                               i
                                                                                            )
                                                                                          )
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
                                                                        list-set! update i x
                                                                      )
                                                                       (
                                                                        set! i (
                                                                          - i 1
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
                                                        set! x (
                                                          cond (
                                                            (
                                                              string? (
                                                                list-ref node_forwards x
                                                              )
                                                            )
                                                             (
                                                              _substring (
                                                                list-ref node_forwards x
                                                              )
                                                               0 (
                                                                + 0 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? (
                                                                list-ref node_forwards x
                                                              )
                                                            )
                                                             (
                                                              hash-table-ref (
                                                                list-ref node_forwards x
                                                              )
                                                               0
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref (
                                                                list-ref node_forwards x
                                                              )
                                                               0
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        if (
                                                          and (
                                                            not (
                                                              equal? x NIL
                                                            )
                                                          )
                                                           (
                                                            equal? (
                                                              list-ref node_keys x
                                                            )
                                                             key
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            list-set! node_vals x value
                                                          )
                                                           (
                                                            ret9 (
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
                                                        let (
                                                          (
                                                            lvl (
                                                              random_level
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              _gt lvl level
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    j level
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
                                                                                  _lt j lvl
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    list-set! update j 0
                                                                                  )
                                                                                   (
                                                                                    set! j (
                                                                                      + j 1
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
                                                                    set! level lvl
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
                                                            set! node_keys (
                                                              append node_keys (
                                                                _list key
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! node_vals (
                                                              append node_vals (
                                                                _list value
                                                              )
                                                            )
                                                          )
                                                           (
                                                            let (
                                                              (
                                                                forwards (
                                                                  empty_forward
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    idx (
                                                                      - (
                                                                        _len node_keys
                                                                      )
                                                                       1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! i 0
                                                                  )
                                                                   (
                                                                    call/cc (
                                                                      lambda (
                                                                        break19
                                                                      )
                                                                       (
                                                                        letrec (
                                                                          (
                                                                            loop18 (
                                                                              lambda (
                                                                                
                                                                              )
                                                                               (
                                                                                if (
                                                                                  _lt i lvl
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    list-set! forwards i (
                                                                                      cond (
                                                                                        (
                                                                                          string? (
                                                                                            list-ref node_forwards (
                                                                                              list-ref update i
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          _substring (
                                                                                            list-ref node_forwards (
                                                                                              list-ref update i
                                                                                            )
                                                                                          )
                                                                                           i (
                                                                                            + i 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? (
                                                                                            list-ref node_forwards (
                                                                                              list-ref update i
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref (
                                                                                            list-ref node_forwards (
                                                                                              list-ref update i
                                                                                            )
                                                                                          )
                                                                                           i
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref (
                                                                                            list-ref node_forwards (
                                                                                              list-ref update i
                                                                                            )
                                                                                          )
                                                                                           i
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    list-set! (
                                                                                      list-ref node_forwards (
                                                                                        list-ref update i
                                                                                      )
                                                                                    )
                                                                                     i idx
                                                                                  )
                                                                                   (
                                                                                    set! i (
                                                                                      + i 1
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    loop18
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
                                                                          loop18
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! node_forwards (
                                                                      append node_forwards (
                                                                        _list forwards
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
                                        find key
                                      )
                                       (
                                        call/cc (
                                          lambda (
                                            ret20
                                          )
                                           (
                                            let (
                                              (
                                                x 0
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    i (
                                                      - level 1
                                                    )
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
                                                                  >= i 0
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
                                                                                  and (
                                                                                    not (
                                                                                      equal? (
                                                                                        cond (
                                                                                          (
                                                                                            string? (
                                                                                              list-ref node_forwards x
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            _substring (
                                                                                              list-ref node_forwards x
                                                                                            )
                                                                                             i (
                                                                                              + i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? (
                                                                                              list-ref node_forwards x
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref (
                                                                                              list-ref node_forwards x
                                                                                            )
                                                                                             i
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref (
                                                                                              list-ref node_forwards x
                                                                                            )
                                                                                             i
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       NIL
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    < (
                                                                                      list-ref node_keys (
                                                                                        cond (
                                                                                          (
                                                                                            string? (
                                                                                              list-ref node_forwards x
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            _substring (
                                                                                              list-ref node_forwards x
                                                                                            )
                                                                                             i (
                                                                                              + i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? (
                                                                                              list-ref node_forwards x
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref (
                                                                                              list-ref node_forwards x
                                                                                            )
                                                                                             i
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref (
                                                                                              list-ref node_forwards x
                                                                                            )
                                                                                             i
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     key
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! x (
                                                                                      cond (
                                                                                        (
                                                                                          string? (
                                                                                            list-ref node_forwards x
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          _substring (
                                                                                            list-ref node_forwards x
                                                                                          )
                                                                                           i (
                                                                                            + i 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? (
                                                                                            list-ref node_forwards x
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref (
                                                                                            list-ref node_forwards x
                                                                                          )
                                                                                           i
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref (
                                                                                            list-ref node_forwards x
                                                                                          )
                                                                                           i
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
                                                                    set! i (
                                                                      - i 1
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
                                                    set! x (
                                                      cond (
                                                        (
                                                          string? (
                                                            list-ref node_forwards x
                                                          )
                                                        )
                                                         (
                                                          _substring (
                                                            list-ref node_forwards x
                                                          )
                                                           0 (
                                                            + 0 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? (
                                                            list-ref node_forwards x
                                                          )
                                                        )
                                                         (
                                                          hash-table-ref (
                                                            list-ref node_forwards x
                                                          )
                                                           0
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref (
                                                            list-ref node_forwards x
                                                          )
                                                           0
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    if (
                                                      and (
                                                        not (
                                                          equal? x NIL
                                                        )
                                                      )
                                                       (
                                                        equal? (
                                                          list-ref node_keys x
                                                        )
                                                         key
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        ret20 (
                                                          list-ref node_vals x
                                                        )
                                                      )
                                                    )
                                                     (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                  )
                                                   (
                                                    ret20 (
                                                      - 1
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
                                        delete key
                                      )
                                       (
                                        call/cc (
                                          lambda (
                                            ret25
                                          )
                                           (
                                            let (
                                              (
                                                update (
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
                                                        break27
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop26 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < i MAX_LEVEL
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! update (
                                                                      append update (
                                                                        _list 0
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! i (
                                                                      + i 1
                                                                    )
                                                                  )
                                                                   (
                                                                    loop26
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
                                                          loop26
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    let (
                                                      (
                                                        x 0
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! i (
                                                          - level 1
                                                        )
                                                      )
                                                       (
                                                        call/cc (
                                                          lambda (
                                                            break29
                                                          )
                                                           (
                                                            letrec (
                                                              (
                                                                loop28 (
                                                                  lambda (
                                                                    
                                                                  )
                                                                   (
                                                                    if (
                                                                      >= i 0
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
                                                                                      and (
                                                                                        not (
                                                                                          equal? (
                                                                                            cond (
                                                                                              (
                                                                                                string? (
                                                                                                  list-ref node_forwards x
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _substring (
                                                                                                  list-ref node_forwards x
                                                                                                )
                                                                                                 i (
                                                                                                  + i 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? (
                                                                                                  list-ref node_forwards x
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref (
                                                                                                  list-ref node_forwards x
                                                                                                )
                                                                                                 i
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref (
                                                                                                  list-ref node_forwards x
                                                                                                )
                                                                                                 i
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           NIL
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        < (
                                                                                          list-ref node_keys (
                                                                                            cond (
                                                                                              (
                                                                                                string? (
                                                                                                  list-ref node_forwards x
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _substring (
                                                                                                  list-ref node_forwards x
                                                                                                )
                                                                                                 i (
                                                                                                  + i 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? (
                                                                                                  list-ref node_forwards x
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref (
                                                                                                  list-ref node_forwards x
                                                                                                )
                                                                                                 i
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref (
                                                                                                  list-ref node_forwards x
                                                                                                )
                                                                                                 i
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         key
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! x (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                list-ref node_forwards x
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref node_forwards x
                                                                                              )
                                                                                               i (
                                                                                                + i 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref node_forwards x
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref node_forwards x
                                                                                              )
                                                                                               i
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref node_forwards x
                                                                                              )
                                                                                               i
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
                                                                        list-set! update i x
                                                                      )
                                                                       (
                                                                        set! i (
                                                                          - i 1
                                                                        )
                                                                      )
                                                                       (
                                                                        loop28
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
                                                              loop28
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! x (
                                                          cond (
                                                            (
                                                              string? (
                                                                list-ref node_forwards x
                                                              )
                                                            )
                                                             (
                                                              _substring (
                                                                list-ref node_forwards x
                                                              )
                                                               0 (
                                                                + 0 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? (
                                                                list-ref node_forwards x
                                                              )
                                                            )
                                                             (
                                                              hash-table-ref (
                                                                list-ref node_forwards x
                                                              )
                                                               0
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref (
                                                                list-ref node_forwards x
                                                              )
                                                               0
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        if (
                                                          or (
                                                            equal? x NIL
                                                          )
                                                           (
                                                            not (
                                                              equal? (
                                                                list-ref node_keys x
                                                              )
                                                               key
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            ret25 (
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
                                                        set! i 0
                                                      )
                                                       (
                                                        call/cc (
                                                          lambda (
                                                            break33
                                                          )
                                                           (
                                                            letrec (
                                                              (
                                                                loop32 (
                                                                  lambda (
                                                                    
                                                                  )
                                                                   (
                                                                    if (
                                                                      < i level
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          equal? (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref node_forwards (
                                                                                    list-ref update i
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref node_forwards (
                                                                                    list-ref update i
                                                                                  )
                                                                                )
                                                                                 i (
                                                                                  + i 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref node_forwards (
                                                                                    list-ref update i
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref node_forwards (
                                                                                    list-ref update i
                                                                                  )
                                                                                )
                                                                                 i
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref node_forwards (
                                                                                    list-ref update i
                                                                                  )
                                                                                )
                                                                                 i
                                                                              )
                                                                            )
                                                                          )
                                                                           x
                                                                        )
                                                                         (
                                                                          begin (
                                                                            list-set! (
                                                                              list-ref node_forwards (
                                                                                list-ref update i
                                                                              )
                                                                            )
                                                                             i (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref node_forwards x
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref node_forwards x
                                                                                  )
                                                                                   i (
                                                                                    + i 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref node_forwards x
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref node_forwards x
                                                                                  )
                                                                                   i
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref node_forwards x
                                                                                  )
                                                                                   i
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
                                                                        loop32
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
                                                              loop32
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        call/cc (
                                                          lambda (
                                                            break35
                                                          )
                                                           (
                                                            letrec (
                                                              (
                                                                loop34 (
                                                                  lambda (
                                                                    
                                                                  )
                                                                   (
                                                                    if (
                                                                      and (
                                                                        > level 1
                                                                      )
                                                                       (
                                                                        equal? (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref node_forwards 0
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref node_forwards 0
                                                                              )
                                                                               (
                                                                                - level 1
                                                                              )
                                                                               (
                                                                                + (
                                                                                  - level 1
                                                                                )
                                                                                 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref node_forwards 0
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref node_forwards 0
                                                                              )
                                                                               (
                                                                                - level 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref node_forwards 0
                                                                              )
                                                                               (
                                                                                - level 1
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         NIL
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! level (
                                                                          - level 1
                                                                        )
                                                                      )
                                                                       (
                                                                        loop34
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
                                                              loop34
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
                                        to_string
                                      )
                                       (
                                        call/cc (
                                          lambda (
                                            ret36
                                          )
                                           (
                                            let (
                                              (
                                                s ""
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    x (
                                                      cond (
                                                        (
                                                          string? (
                                                            list-ref node_forwards 0
                                                          )
                                                        )
                                                         (
                                                          _substring (
                                                            list-ref node_forwards 0
                                                          )
                                                           0 (
                                                            + 0 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? (
                                                            list-ref node_forwards 0
                                                          )
                                                        )
                                                         (
                                                          hash-table-ref (
                                                            list-ref node_forwards 0
                                                          )
                                                           0
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref (
                                                            list-ref node_forwards 0
                                                          )
                                                           0
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    call/cc (
                                                      lambda (
                                                        break38
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop37 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  not (
                                                                    equal? x NIL
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      not (
                                                                        string=? s ""
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! s (
                                                                          string-append s " -> "
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      quote (
                                                                        
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! s (
                                                                      string-append (
                                                                        string-append (
                                                                          string-append s (
                                                                            to-str-space (
                                                                              list-ref node_keys x
                                                                            )
                                                                          )
                                                                        )
                                                                         ":"
                                                                      )
                                                                       (
                                                                        to-str-space (
                                                                          list-ref node_vals x
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! x (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            list-ref node_forwards x
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref node_forwards x
                                                                          )
                                                                           0 (
                                                                            + 0 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref node_forwards x
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref node_forwards x
                                                                          )
                                                                           0
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            list-ref node_forwards x
                                                                          )
                                                                           0
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    loop37
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
                                                          loop37
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    ret36 s
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
                                            ret39
                                          )
                                           (
                                            begin (
                                              init
                                            )
                                             (
                                              insert 2 2
                                            )
                                             (
                                              insert 4 4
                                            )
                                             (
                                              insert 6 4
                                            )
                                             (
                                              insert 4 5
                                            )
                                             (
                                              insert 8 4
                                            )
                                             (
                                              insert 9 4
                                            )
                                             (
                                              delete 4
                                            )
                                             (
                                              _display (
                                                if (
                                                  string? (
                                                    to_string
                                                  )
                                                )
                                                 (
                                                  to_string
                                                )
                                                 (
                                                  to-str (
                                                    to_string
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
          end41 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur42 (
              quotient (
                * (
                  - end41 start40
                )
                 1000000
              )
               jps43
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur42
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
