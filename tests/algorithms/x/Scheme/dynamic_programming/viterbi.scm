;; Generated on 2025-08-07 08:40 +0700
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
      start20 (
        current-jiffy
      )
    )
     (
      jps23 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        key state obs
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              string-append (
                string-append state "|"
              )
               obs
            )
          )
        )
      )
    )
     (
      define (
        viterbi observations states start_p trans_p emit_p
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                or (
                  equal? (
                    _len observations
                  )
                   0
                )
                 (
                  equal? (
                    _len states
                  )
                   0
                )
              )
               (
                begin (
                  panic "empty parameters"
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
                  probs (
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
                      ptrs (
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
                          first_obs (
                            list-ref observations 0
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
                                            < i (
                                              _len states
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  state (
                                                    list-ref states i
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  hash-table-set! probs (
                                                    key state first_obs
                                                  )
                                                   (
                                                    * (
                                                      hash-table-ref/default start_p state (
                                                        quote (
                                                          
                                                        )
                                                      )
                                                    )
                                                     (
                                                      cond (
                                                        (
                                                          string? (
                                                            hash-table-ref/default emit_p state (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          _substring (
                                                            hash-table-ref/default emit_p state (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                           first_obs (
                                                            + first_obs 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? (
                                                            hash-table-ref/default emit_p state (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          hash-table-ref (
                                                            hash-table-ref/default emit_p state (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                           first_obs
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref (
                                                            hash-table-ref/default emit_p state (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                           first_obs
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  hash-table-set! ptrs (
                                                    key state first_obs
                                                  )
                                                   ""
                                                )
                                                 (
                                                  set! i (
                                                    + i 1
                                                  )
                                                )
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
                              let (
                                (
                                  t 1
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
                                                < t (
                                                  _len observations
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      obs (
                                                        list-ref observations t
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
                                                                        < j (
                                                                          _len states
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          let (
                                                                            (
                                                                              state (
                                                                                list-ref states j
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              let (
                                                                                (
                                                                                  max_prob (
                                                                                    - 1.0
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  let (
                                                                                    (
                                                                                      prev_state ""
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
                                                                                                        < k (
                                                                                                          _len states
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        begin (
                                                                                                          let (
                                                                                                            (
                                                                                                              state0 (
                                                                                                                list-ref states k
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            begin (
                                                                                                              let (
                                                                                                                (
                                                                                                                  obs0 (
                                                                                                                    list-ref observations (
                                                                                                                      - t 1
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                begin (
                                                                                                                  let (
                                                                                                                    (
                                                                                                                      prob_prev (
                                                                                                                        hash-table-ref/default probs (
                                                                                                                          key state0 obs0
                                                                                                                        )
                                                                                                                         (
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
                                                                                                                          prob (
                                                                                                                            * (
                                                                                                                              * prob_prev (
                                                                                                                                cond (
                                                                                                                                  (
                                                                                                                                    string? (
                                                                                                                                      hash-table-ref/default trans_p state0 (
                                                                                                                                        quote (
                                                                                                                                          
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    _substring (
                                                                                                                                      hash-table-ref/default trans_p state0 (
                                                                                                                                        quote (
                                                                                                                                          
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     state (
                                                                                                                                      + state 1
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  (
                                                                                                                                    hash-table? (
                                                                                                                                      hash-table-ref/default trans_p state0 (
                                                                                                                                        quote (
                                                                                                                                          
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    hash-table-ref (
                                                                                                                                      hash-table-ref/default trans_p state0 (
                                                                                                                                        quote (
                                                                                                                                          
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     state
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  else (
                                                                                                                                    list-ref (
                                                                                                                                      hash-table-ref/default trans_p state0 (
                                                                                                                                        quote (
                                                                                                                                          
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     state
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              cond (
                                                                                                                                (
                                                                                                                                  string? (
                                                                                                                                    hash-table-ref/default emit_p state (
                                                                                                                                      quote (
                                                                                                                                        
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  _substring (
                                                                                                                                    hash-table-ref/default emit_p state (
                                                                                                                                      quote (
                                                                                                                                        
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   obs (
                                                                                                                                    + obs 1
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                (
                                                                                                                                  hash-table? (
                                                                                                                                    hash-table-ref/default emit_p state (
                                                                                                                                      quote (
                                                                                                                                        
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  hash-table-ref (
                                                                                                                                    hash-table-ref/default emit_p state (
                                                                                                                                      quote (
                                                                                                                                        
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   obs
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                else (
                                                                                                                                  list-ref (
                                                                                                                                    hash-table-ref/default emit_p state (
                                                                                                                                      quote (
                                                                                                                                        
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   obs
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        begin (
                                                                                                                          if (
                                                                                                                            > prob max_prob
                                                                                                                          )
                                                                                                                           (
                                                                                                                            begin (
                                                                                                                              set! max_prob prob
                                                                                                                            )
                                                                                                                             (
                                                                                                                              set! prev_state state0
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            quote (
                                                                                                                              
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          set! k (
                                                                                                                            + k 1
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
                                                                                          hash-table-set! probs (
                                                                                            key state obs
                                                                                          )
                                                                                           max_prob
                                                                                        )
                                                                                         (
                                                                                          hash-table-set! ptrs (
                                                                                            key state obs
                                                                                          )
                                                                                           prev_state
                                                                                        )
                                                                                         (
                                                                                          set! j (
                                                                                            + j 1
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
                                                          set! t (
                                                            + t 1
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
                                  let (
                                    (
                                      path (
                                        _list
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          n 0
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
                                                        < n (
                                                          _len observations
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          set! path (
                                                            append path (
                                                              _list ""
                                                            )
                                                          )
                                                        )
                                                         (
                                                          set! n (
                                                            + n 1
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
                                          let (
                                            (
                                              last_obs (
                                                list-ref observations (
                                                  - (
                                                    _len observations
                                                  )
                                                   1
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  max_final (
                                                    - 1.0
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      last_state ""
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          m 0
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
                                                                        < m (
                                                                          _len states
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          let (
                                                                            (
                                                                              state (
                                                                                list-ref states m
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              let (
                                                                                (
                                                                                  prob (
                                                                                    hash-table-ref/default probs (
                                                                                      key state last_obs
                                                                                    )
                                                                                     (
                                                                                      quote (
                                                                                        
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  if (
                                                                                    > prob max_final
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      set! max_final prob
                                                                                    )
                                                                                     (
                                                                                      set! last_state state
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    quote (
                                                                                      
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  set! m (
                                                                                    + m 1
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
                                                          let (
                                                            (
                                                              last_index (
                                                                - (
                                                                  _len observations
                                                                )
                                                                 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              list-set! path last_index last_state
                                                            )
                                                             (
                                                              let (
                                                                (
                                                                  idx last_index
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
                                                                                > idx 0
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  let (
                                                                                    (
                                                                                      obs (
                                                                                        list-ref observations idx
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      let (
                                                                                        (
                                                                                          prev (
                                                                                            hash-table-ref/default ptrs (
                                                                                              key (
                                                                                                list-ref path idx
                                                                                              )
                                                                                               obs
                                                                                            )
                                                                                             (
                                                                                              quote (
                                                                                                
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        begin (
                                                                                          list-set! path (
                                                                                            - idx 1
                                                                                          )
                                                                                           prev
                                                                                        )
                                                                                         (
                                                                                          set! idx (
                                                                                            - idx 1
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
                                                                  ret2 path
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
            )
          )
        )
      )
    )
     (
      define (
        join_words words
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            let (
              (
                res ""
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
                                  < i (
                                    _len words
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      > i 0
                                    )
                                     (
                                      begin (
                                        set! res (
                                          string-append res " "
                                        )
                                      )
                                    )
                                     (
                                      quote (
                                        
                                      )
                                    )
                                  )
                                   (
                                    set! res (
                                      string-append res (
                                        list-ref words i
                                      )
                                    )
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
                    ret17 res
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
          observations (
            _list "normal" "cold" "dizzy"
          )
        )
      )
       (
        begin (
          let (
            (
              states (
                _list "Healthy" "Fever"
              )
            )
          )
           (
            begin (
              let (
                (
                  start_p (
                    alist->hash-table (
                      _list (
                        cons "Healthy" 0.6
                      )
                       (
                        cons "Fever" 0.4
                      )
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      trans_p (
                        alist->hash-table (
                          _list (
                            cons "Healthy" (
                              alist->hash-table (
                                _list (
                                  cons "Healthy" 0.7
                                )
                                 (
                                  cons "Fever" 0.3
                                )
                              )
                            )
                          )
                           (
                            cons "Fever" (
                              alist->hash-table (
                                _list (
                                  cons "Healthy" 0.4
                                )
                                 (
                                  cons "Fever" 0.6
                                )
                              )
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
                          emit_p (
                            alist->hash-table (
                              _list (
                                cons "Healthy" (
                                  alist->hash-table (
                                    _list (
                                      cons "normal" 0.5
                                    )
                                     (
                                      cons "cold" 0.4
                                    )
                                     (
                                      cons "dizzy" 0.1
                                    )
                                  )
                                )
                              )
                               (
                                cons "Fever" (
                                  alist->hash-table (
                                    _list (
                                      cons "normal" 0.1
                                    )
                                     (
                                      cons "cold" 0.3
                                    )
                                     (
                                      cons "dizzy" 0.6
                                    )
                                  )
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
                              result (
                                viterbi observations states start_p trans_p emit_p
                              )
                            )
                          )
                           (
                            begin (
                              _display (
                                if (
                                  string? (
                                    join_words result
                                  )
                                )
                                 (
                                  join_words result
                                )
                                 (
                                  to-str (
                                    join_words result
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
     (
      let (
        (
          end21 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur22 (
              quotient (
                * (
                  - end21 start20
                )
                 1000000
              )
               jps23
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur22
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
