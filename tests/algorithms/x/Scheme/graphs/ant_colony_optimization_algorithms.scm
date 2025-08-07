;; Generated on 2025-08-07 08:56 +0700
(import (scheme base))
(import (scheme time))
(import (chibi string))
(import (only (scheme char) string-upcase string-downcase))
(import (srfi 69))
(import (srfi 1))
(define _list list)
(import (chibi time) (srfi 98))
(define _now_seeded #f)
(define _now_seed 0)
(define (now)
  (when (not _now_seeded)
    (let ((s (get-environment-variable "MOCHI_NOW_SEED")))
      (when (and s (string->number s))
        (set! _now_seed (string->number s))
        (set! _now_seeded #t))))
  (if _now_seeded
      (begin
        (set! _now_seed (modulo (+ (* _now_seed 1664525) 1013904223) 2147483647))
        _now_seed)
      (exact (floor (* (current-second) 1000000000))))
)
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
      start49 (
        current-jiffy
      )
    )
     (
      jps52 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        sqrtApprox x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                guess (
                  _div x 2.0
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
                                  < i 20
                                )
                                 (
                                  begin (
                                    set! guess (
                                      _div (
                                        _add guess (
                                          _div x guess
                                        )
                                      )
                                       2.0
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
                   (
                    ret1 guess
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
        rand_float
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            ret4 (
              _div (
                + 0.0 (
                  _mod (
                    now
                  )
                   1000000
                )
              )
               1000000.0
            )
          )
        )
      )
    )
     (
      define (
        pow_float base exp
      )
       (
        call/cc (
          lambda (
            ret5
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
                    let (
                      (
                        e (
                          let (
                            (
                              v6 exp
                            )
                          )
                           (
                            cond (
                              (
                                string? v6
                              )
                               (
                                inexact->exact (
                                  floor (
                                    string->number v6
                                  )
                                )
                              )
                            )
                             (
                              (
                                boolean? v6
                              )
                               (
                                if v6 1 0
                              )
                            )
                             (
                              else (
                                inexact->exact (
                                  floor v6
                                )
                              )
                            )
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
                                    
                                  )
                                   (
                                    if (
                                      < i e
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
                        ret5 result
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
        distance city1 city2
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                dx (
                  + 0.0 (
                    - (
                      list-ref city1 0
                    )
                     (
                      list-ref city2 0
                    )
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    dy (
                      + 0.0 (
                        - (
                          list-ref city1 1
                        )
                         (
                          list-ref city2 1
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    ret9 (
                      sqrtApprox (
                        _add (
                          * dx dx
                        )
                         (
                          * dy dy
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
        choose_weighted options weights
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                total 0.0
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
                                  < i (
                                    _len weights
                                  )
                                )
                                 (
                                  begin (
                                    set! total (
                                      + total (
                                        list-ref weights i
                                      )
                                    )
                                  )
                                   (
                                    set! i (
                                      + i 1
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
                        r (
                          * (
                            rand_float
                          )
                           total
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            accum 0.0
                          )
                        )
                         (
                          begin (
                            set! i 0
                          )
                           (
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
                                          < i (
                                            _len weights
                                          )
                                        )
                                         (
                                          begin (
                                            set! accum (
                                              + accum (
                                                list-ref weights i
                                              )
                                            )
                                          )
                                           (
                                            if (
                                              _le r accum
                                            )
                                             (
                                              begin (
                                                ret10 (
                                                  list-ref options i
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
                            ret10 (
                              list-ref options (
                                - (
                                  _len options
                                )
                                 1
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
        city_select pheromone current unvisited alpha beta cities
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            let (
              (
                probs (
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
                                  < i (
                                    _len unvisited
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        city (
                                          list-ref unvisited i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            dist (
                                              distance (
                                                hash-table-ref/default cities city (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                               (
                                                hash-table-ref/default cities current (
                                                  quote (
                                                    
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
                                                trail (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref pheromone city
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref pheromone city
                                                      )
                                                       current (
                                                        + current 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref pheromone city
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref pheromone city
                                                      )
                                                       current
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref (
                                                        list-ref pheromone city
                                                      )
                                                       current
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
                                                        pow_float trail alpha
                                                      )
                                                       (
                                                        pow_float (
                                                          _div 1.0 dist
                                                        )
                                                         beta
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! probs (
                                                      append probs (
                                                        _list prob
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
                    ret15 (
                      choose_weighted unvisited probs
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
        pheromone_update pheromone cities evaporation ants_route q
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            let (
              (
                n (
                  _len pheromone
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
                                  < i n
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
                                                      < j n
                                                    )
                                                     (
                                                      begin (
                                                        list-set! (
                                                          list-ref pheromone i
                                                        )
                                                         j (
                                                          * (
                                                            cond (
                                                              (
                                                                string? (
                                                                  list-ref pheromone i
                                                                )
                                                              )
                                                               (
                                                                _substring (
                                                                  list-ref pheromone i
                                                                )
                                                                 j (
                                                                  + j 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? (
                                                                  list-ref pheromone i
                                                                )
                                                              )
                                                               (
                                                                hash-table-ref (
                                                                  list-ref pheromone i
                                                                )
                                                                 j
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref (
                                                                  list-ref pheromone i
                                                                )
                                                                 j
                                                              )
                                                            )
                                                          )
                                                           evaporation
                                                        )
                                                      )
                                                       (
                                                        set! j (
                                                          + j 1
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
                                        set! i (
                                          + i 1
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
                    let (
                      (
                        a 0
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
                                      < a (
                                        _len ants_route
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            route (
                                              list-ref ants_route a
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                total 0.0
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    r 0
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    call/cc (
                                                      lambda (
                                                        break26
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop25 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < r (
                                                                    - (
                                                                      _len route
                                                                    )
                                                                     1
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! total (
                                                                      _add total (
                                                                        distance (
                                                                          hash-table-ref/default cities (
                                                                            list-ref route r
                                                                          )
                                                                           (
                                                                            quote (
                                                                              
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref/default cities (
                                                                            list-ref route (
                                                                              + r 1
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
                                                                    set! r (
                                                                      + r 1
                                                                    )
                                                                  )
                                                                   (
                                                                    loop25
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
                                                          loop25
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    let (
                                                      (
                                                        delta (
                                                          _div q total
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! r 0
                                                      )
                                                       (
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
                                                                      < r (
                                                                        - (
                                                                          _len route
                                                                        )
                                                                         1
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            u (
                                                                              list-ref route r
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                v (
                                                                                  list-ref route (
                                                                                    + r 1
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                list-set! (
                                                                                  list-ref pheromone u
                                                                                )
                                                                                 v (
                                                                                  + (
                                                                                    cond (
                                                                                      (
                                                                                        string? (
                                                                                          list-ref pheromone u
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        _substring (
                                                                                          list-ref pheromone u
                                                                                        )
                                                                                         v (
                                                                                          + v 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? (
                                                                                          list-ref pheromone u
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref (
                                                                                          list-ref pheromone u
                                                                                        )
                                                                                         v
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        list-ref (
                                                                                          list-ref pheromone u
                                                                                        )
                                                                                         v
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   delta
                                                                                )
                                                                              )
                                                                               (
                                                                                list-set! (
                                                                                  list-ref pheromone v
                                                                                )
                                                                                 u (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref pheromone u
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref pheromone u
                                                                                      )
                                                                                       v (
                                                                                        + v 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref pheromone u
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref pheromone u
                                                                                      )
                                                                                       v
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref pheromone u
                                                                                      )
                                                                                       v
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! r (
                                                                                  + r 1
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
                                                        set! a (
                                                          + a 1
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
                        ret18 pheromone
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
        remove_value lst val
      )
       (
        call/cc (
          lambda (
            ret29
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
                                    _len lst
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      not (
                                        equal? (
                                          list-ref lst i
                                        )
                                         val
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
                    ret29 res
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
        ant_colony cities ants_num iterations evaporation alpha beta q
      )
       (
        call/cc (
          lambda (
            ret32
          )
           (
            let (
              (
                n (
                  _len cities
                )
              )
            )
             (
              begin (
                let (
                  (
                    pheromone (
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
                            break34
                          )
                           (
                            letrec (
                              (
                                loop33 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i n
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            row (
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
                                                    break36
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop35 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              < j n
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list 1.0
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! j (
                                                                  + j 1
                                                                )
                                                              )
                                                               (
                                                                loop35
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
                                                      loop35
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! pheromone (
                                                  append pheromone (
                                                    _list row
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
                                        loop33
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
                              loop33
                            )
                          )
                        )
                      )
                       (
                        let (
                          (
                            best_path (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                best_distance 1000000000.0
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    iter 0
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
                                                  < iter iterations
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        ants_route (
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
                                                                break40
                                                              )
                                                               (
                                                                letrec (
                                                                  (
                                                                    loop39 (
                                                                      lambda (
                                                                        
                                                                      )
                                                                       (
                                                                        if (
                                                                          < k ants_num
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                route (
                                                                                  _list 0
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    unvisited (
                                                                                      _list
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    call/cc (
                                                                                      lambda (
                                                                                        break42
                                                                                      )
                                                                                       (
                                                                                        letrec (
                                                                                          (
                                                                                            loop41 (
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
                                                                                                        key (
                                                                                                          car xs
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        if (
                                                                                                          not (
                                                                                                            equal? key 0
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            set! unvisited (
                                                                                                              append unvisited (
                                                                                                                _list key
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
                                                                                                    loop41 (
                                                                                                      cdr xs
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          loop41 (
                                                                                            hash-table-keys cities
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    let (
                                                                                      (
                                                                                        current 0
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        call/cc (
                                                                                          lambda (
                                                                                            break44
                                                                                          )
                                                                                           (
                                                                                            letrec (
                                                                                              (
                                                                                                loop43 (
                                                                                                  lambda (
                                                                                                    
                                                                                                  )
                                                                                                   (
                                                                                                    if (
                                                                                                      > (
                                                                                                        _len unvisited
                                                                                                      )
                                                                                                       0
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        let (
                                                                                                          (
                                                                                                            next_city (
                                                                                                              city_select pheromone current unvisited alpha beta cities
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            set! route (
                                                                                                              append route (
                                                                                                                _list next_city
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            set! unvisited (
                                                                                                              remove_value unvisited next_city
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            set! current next_city
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        loop43
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
                                                                                              loop43
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        set! route (
                                                                                          append route (
                                                                                            _list 0
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        set! ants_route (
                                                                                          append ants_route (
                                                                                            _list route
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
                                                                           (
                                                                            loop39
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
                                                                  loop39
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! pheromone (
                                                              pheromone_update pheromone cities evaporation ants_route q
                                                            )
                                                          )
                                                           (
                                                            let (
                                                              (
                                                                a 0
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                call/cc (
                                                                  lambda (
                                                                    break46
                                                                  )
                                                                   (
                                                                    letrec (
                                                                      (
                                                                        loop45 (
                                                                          lambda (
                                                                            
                                                                          )
                                                                           (
                                                                            if (
                                                                              < a (
                                                                                _len ants_route
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    route (
                                                                                      list-ref ants_route a
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        dist 0.0
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            r 0
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            call/cc (
                                                                                              lambda (
                                                                                                break48
                                                                                              )
                                                                                               (
                                                                                                letrec (
                                                                                                  (
                                                                                                    loop47 (
                                                                                                      lambda (
                                                                                                        
                                                                                                      )
                                                                                                       (
                                                                                                        if (
                                                                                                          < r (
                                                                                                            - (
                                                                                                              _len route
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            set! dist (
                                                                                                              _add dist (
                                                                                                                distance (
                                                                                                                  hash-table-ref/default cities (
                                                                                                                    list-ref route r
                                                                                                                  )
                                                                                                                   (
                                                                                                                    quote (
                                                                                                                      
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  hash-table-ref/default cities (
                                                                                                                    list-ref route (
                                                                                                                      + r 1
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
                                                                                                            set! r (
                                                                                                              + r 1
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            loop47
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
                                                                                                  loop47
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            if (
                                                                                              < dist best_distance
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! best_distance dist
                                                                                              )
                                                                                               (
                                                                                                set! best_path route
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              quote (
                                                                                                
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! a (
                                                                                              + a 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                loop45
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
                                                                      loop45
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! iter (
                                                                  + iter 1
                                                                )
                                                              )
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
                                    _display (
                                      if (
                                        string? (
                                          string-append "best_path = " (
                                            to-str-space best_path
                                          )
                                        )
                                      )
                                       (
                                        string-append "best_path = " (
                                          to-str-space best_path
                                        )
                                      )
                                       (
                                        to-str (
                                          string-append "best_path = " (
                                            to-str-space best_path
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    newline
                                  )
                                   (
                                    _display (
                                      if (
                                        string? (
                                          string-append "best_distance = " (
                                            to-str-space best_distance
                                          )
                                        )
                                      )
                                       (
                                        string-append "best_distance = " (
                                          to-str-space best_distance
                                        )
                                      )
                                       (
                                        to-str (
                                          string-append "best_distance = " (
                                            to-str-space best_distance
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
          cities (
            alist->hash-table (
              _list (
                cons 0 (
                  _list 0 0
                )
              )
               (
                cons 1 (
                  _list 0 5
                )
              )
               (
                cons 2 (
                  _list 3 8
                )
              )
               (
                cons 3 (
                  _list 8 10
                )
              )
               (
                cons 4 (
                  _list 12 8
                )
              )
               (
                cons 5 (
                  _list 12 4
                )
              )
               (
                cons 6 (
                  _list 8 0
                )
              )
               (
                cons 7 (
                  _list 6 2
                )
              )
            )
          )
        )
      )
       (
        begin (
          ant_colony cities 10 20 0.7 1.0 5.0 10.0
        )
      )
    )
     (
      let (
        (
          end50 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur51 (
              quotient (
                * (
                  - end50 start49
                )
                 1000000
              )
               jps52
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur51
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
