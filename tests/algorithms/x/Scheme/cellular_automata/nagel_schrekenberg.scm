;; Generated on 2025-08-06 22:04 +0700
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
      let (
        (
          seed 1
        )
      )
       (
        begin (
          let (
            (
              NEG_ONE (
                - 1
              )
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
                        modulo (
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
                randint a b
              )
               (
                call/cc (
                  lambda (
                    ret2
                  )
                   (
                    let (
                      (
                        r (
                          rand
                        )
                      )
                    )
                     (
                      begin (
                        ret2 (
                          _add a (
                            fmod r (
                              + (
                                - b a
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
             (
              define (
                random
              )
               (
                call/cc (
                  lambda (
                    ret3
                  )
                   (
                    ret3 (
                      / (
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
                construct_highway number_of_cells frequency initial_speed random_frequency random_speed max_speed
              )
               (
                call/cc (
                  lambda (
                    ret4
                  )
                   (
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
                                          < i number_of_cells
                                        )
                                         (
                                          begin (
                                            set! row (
                                              append row (
                                                _list (
                                                  - 1
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
                                highway (
                                  _list
                                )
                              )
                            )
                             (
                              begin (
                                set! highway (
                                  append highway (
                                    _list row
                                  )
                                )
                              )
                               (
                                set! i 0
                              )
                               (
                                if (
                                  < initial_speed 0
                                )
                                 (
                                  begin (
                                    set! initial_speed 0
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
                                              < i number_of_cells
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    speed initial_speed
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if random_speed (
                                                      begin (
                                                        set! speed (
                                                          randint 0 max_speed
                                                        )
                                                      )
                                                    )
                                                     (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                  )
                                                   (
                                                    list-set! (
                                                      list-ref highway 0
                                                    )
                                                     i speed
                                                  )
                                                   (
                                                    let (
                                                      (
                                                        step frequency
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if random_frequency (
                                                          begin (
                                                            set! step (
                                                              randint 1 (
                                                                * max_speed 2
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
                                                          + i step
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
                                ret4 highway
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
                get_distance highway_now car_index
              )
               (
                call/cc (
                  lambda (
                    ret9
                  )
                   (
                    let (
                      (
                        distance 0
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            i (
                              + car_index 1
                            )
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
                                            _len highway_now
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              > (
                                                list-ref highway_now i
                                              )
                                               NEG_ONE
                                            )
                                             (
                                              begin (
                                                ret9 distance
                                              )
                                            )
                                             (
                                              quote (
                                                
                                              )
                                            )
                                          )
                                           (
                                            set! distance (
                                              + distance 1
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
                            ret9 (
                              _add distance (
                                get_distance highway_now (
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
              )
            )
             (
              define (
                update highway_now probability max_speed
              )
               (
                call/cc (
                  lambda (
                    ret12
                  )
                   (
                    let (
                      (
                        number_of_cells (
                          _len highway_now
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            next_highway (
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
                                              < i number_of_cells
                                            )
                                             (
                                              begin (
                                                set! next_highway (
                                                  append next_highway (
                                                    _list (
                                                      - 1
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
                                    car_index 0
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
                                                  < car_index number_of_cells
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        speed (
                                                          list-ref highway_now car_index
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          > speed NEG_ONE
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                new_speed (
                                                                  + speed 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  > new_speed max_speed
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! new_speed max_speed
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
                                                                    dn (
                                                                      - (
                                                                        get_distance highway_now car_index
                                                                      )
                                                                       1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      _gt new_speed dn
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! new_speed dn
                                                                      )
                                                                    )
                                                                     (
                                                                      quote (
                                                                        
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    if (
                                                                      _lt (
                                                                        random
                                                                      )
                                                                       probability
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! new_speed (
                                                                          - new_speed 1
                                                                        )
                                                                      )
                                                                       (
                                                                        if (
                                                                          < new_speed 0
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! new_speed 0
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
                                                                    list-set! next_highway car_index new_speed
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
                                                        set! car_index (
                                                          + car_index 1
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
                                    ret12 next_highway
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
                simulate highway number_of_update probability max_speed
              )
               (
                call/cc (
                  lambda (
                    ret17
                  )
                   (
                    let (
                      (
                        number_of_cells (
                          _len (
                            list-ref highway 0
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
                                          < i number_of_update
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                next_speeds (
                                                  update (
                                                    list-ref highway i
                                                  )
                                                   probability max_speed
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    real_next (
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
                                                            break21
                                                          )
                                                           (
                                                            letrec (
                                                              (
                                                                loop20 (
                                                                  lambda (
                                                                    
                                                                  )
                                                                   (
                                                                    if (
                                                                      < j number_of_cells
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! real_next (
                                                                          append real_next (
                                                                            _list (
                                                                              - 1
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        set! j (
                                                                          + j 1
                                                                        )
                                                                      )
                                                                       (
                                                                        loop20
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
                                                              loop20
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        let (
                                                          (
                                                            k 0
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
                                                                          < k number_of_cells
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                speed (
                                                                                  cond (
                                                                                    (
                                                                                      string? next_speeds
                                                                                    )
                                                                                     (
                                                                                      _substring next_speeds k (
                                                                                        + k 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? next_speeds
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref next_speeds k
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref next_speeds k
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                if (
                                                                                  _gt speed NEG_ONE
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        index (
                                                                                          fmod (
                                                                                            _add k speed
                                                                                          )
                                                                                           number_of_cells
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        list-set! real_next index speed
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
                                                                                set! k (
                                                                                  + k 1
                                                                                )
                                                                              )
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
                                                            set! highway (
                                                              append highway (
                                                                _list real_next
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
                            ret17 highway
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
                    ret24
                  )
                   (
                    let (
                      (
                        ex1 (
                          simulate (
                            construct_highway 6 3 0 #f #f 2
                          )
                           2 0.0 2
                        )
                      )
                    )
                     (
                      begin (
                        _display (
                          if (
                            string? (
                              to-str-space ex1
                            )
                          )
                           (
                            to-str-space ex1
                          )
                           (
                            to-str (
                              to-str-space ex1
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
                            ex2 (
                              simulate (
                                construct_highway 5 2 (
                                  - 2
                                )
                                 #f #f 2
                              )
                               3 0.0 2
                            )
                          )
                        )
                         (
                          begin (
                            _display (
                              if (
                                string? (
                                  to-str-space ex2
                                )
                              )
                               (
                                to-str-space ex2
                              )
                               (
                                to-str (
                                  to-str-space ex2
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
             (
              main
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
