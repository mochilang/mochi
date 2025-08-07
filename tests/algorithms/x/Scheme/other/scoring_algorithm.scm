;; Generated on 2025-08-07 16:45 +0700
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
        ((number? x)
         (if (integer? x)
             (number->string (inexact->exact x))
             (number->string x)))
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
(define (_div a b) (if (and (integer? a) (integer? b) (exact? a) (exact? b)) (quotient a b) (/ a b)))
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
(define (list-ref-safe lst idx) (if (and (integer? idx) (>= idx 0) (< idx (length lst))) (list-ref lst idx) '()))
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
        get_data source_data
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                data_lists (
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
                                  < i (
                                    _len source_data
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        row (
                                          list-ref-safe source_data i
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
                                                          < j (
                                                            _len row
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              < (
                                                                _len data_lists
                                                              )
                                                               (
                                                                + j 1
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    empty (
                                                                      _list
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! data_lists (
                                                                      append data_lists (
                                                                        _list empty
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             '(
                                                              
                                                            )
                                                          )
                                                           (
                                                            list-set! data_lists j (
                                                              append (
                                                                list-ref-safe data_lists j
                                                              )
                                                               (
                                                                _list (
                                                                  list-ref-safe row j
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
                                                            loop4
                                                          )
                                                        )
                                                         '(
                                                          
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
                                            set! i (
                                              + i 1
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
                                 '(
                                  
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
                    ret1 data_lists
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
        calculate_each_score data_lists weights
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                score_lists (
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
                                  < i (
                                    _len data_lists
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        dlist (
                                          list-ref-safe data_lists i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            weight (
                                              list-ref-safe weights i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                mind (
                                                  list-ref-safe dlist 0
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    maxd (
                                                      list-ref-safe dlist 0
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        j 1
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
                                                                      < j (
                                                                        _len dlist
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            val (
                                                                              list-ref-safe dlist j
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            if (
                                                                              < val mind
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! mind val
                                                                              )
                                                                            )
                                                                             '(
                                                                              
                                                                            )
                                                                          )
                                                                           (
                                                                            if (
                                                                              > val maxd
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! maxd val
                                                                              )
                                                                            )
                                                                             '(
                                                                              
                                                                            )
                                                                          )
                                                                           (
                                                                            set! j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        loop9
                                                                      )
                                                                    )
                                                                     '(
                                                                      
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
                                                        let (
                                                          (
                                                            score (
                                                              _list
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! j 0
                                                          )
                                                           (
                                                            if (
                                                              equal? weight 0
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
                                                                              < j (
                                                                                _len dlist
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    item (
                                                                                      list-ref-safe dlist j
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    if (
                                                                                      equal? (
                                                                                        - maxd mind
                                                                                      )
                                                                                       0.0
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! score (
                                                                                          append score (
                                                                                            _list 1.0
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! score (
                                                                                          append score (
                                                                                            _list (
                                                                                              - 1.0 (
                                                                                                _div (
                                                                                                  - item mind
                                                                                                )
                                                                                                 (
                                                                                                  - maxd mind
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! j (
                                                                                      + j 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                loop11
                                                                              )
                                                                            )
                                                                             '(
                                                                              
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
                                                                              < j (
                                                                                _len dlist
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    item (
                                                                                      list-ref-safe dlist j
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    if (
                                                                                      equal? (
                                                                                        - maxd mind
                                                                                      )
                                                                                       0.0
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! score (
                                                                                          append score (
                                                                                            _list 0.0
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! score (
                                                                                          append score (
                                                                                            _list (
                                                                                              _div (
                                                                                                - item mind
                                                                                              )
                                                                                               (
                                                                                                - maxd mind
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! j (
                                                                                      + j 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                loop13
                                                                              )
                                                                            )
                                                                             '(
                                                                              
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
                                                            )
                                                          )
                                                           (
                                                            set! score_lists (
                                                              append score_lists (
                                                                _list score
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
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop7
                                  )
                                )
                                 '(
                                  
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
                    ret6 score_lists
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
        generate_final_scores score_lists
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            let (
              (
                count (
                  _len (
                    list-ref-safe score_lists 0
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    final_scores (
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
                                      < i count
                                    )
                                     (
                                      begin (
                                        set! final_scores (
                                          append final_scores (
                                            _list 0.0
                                          )
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
                                        )
                                      )
                                       (
                                        loop16
                                      )
                                    )
                                     '(
                                      
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
                                      < i (
                                        _len score_lists
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            slist (
                                              list-ref-safe score_lists i
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
                                                              < j (
                                                                _len slist
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                list-set! final_scores j (
                                                                  + (
                                                                    list-ref-safe final_scores j
                                                                  )
                                                                   (
                                                                    list-ref-safe slist j
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
                                                             '(
                                                              
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
                                                set! i (
                                                  + i 1
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
                                     '(
                                      
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
                        ret15 final_scores
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
        procentual_proximity source_data weights
      )
       (
        call/cc (
          lambda (
            ret22
          )
           (
            let (
              (
                data_lists (
                  get_data source_data
                )
              )
            )
             (
              begin (
                let (
                  (
                    score_lists (
                      calculate_each_score data_lists weights
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        final_scores (
                          generate_final_scores score_lists
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
                                          < i (
                                            _len final_scores
                                          )
                                        )
                                         (
                                          begin (
                                            list-set! source_data i (
                                              append (
                                                list-ref-safe source_data i
                                              )
                                               (
                                                _list (
                                                  cond (
                                                    (
                                                      string? final_scores
                                                    )
                                                     (
                                                      _substring final_scores i (
                                                        + i 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? final_scores
                                                    )
                                                     (
                                                      hash-table-ref final_scores i
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref-safe final_scores i
                                                    )
                                                  )
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
                                            loop23
                                          )
                                        )
                                         '(
                                          
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
                            ret22 source_data
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
          vehicles (
            _list
          )
        )
      )
       (
        begin (
          set! vehicles (
            append vehicles (
              _list (
                _list 20.0 60.0 2012.0
              )
            )
          )
        )
         (
          set! vehicles (
            append vehicles (
              _list (
                _list 23.0 90.0 2015.0
              )
            )
          )
        )
         (
          set! vehicles (
            append vehicles (
              _list (
                _list 22.0 50.0 2011.0
              )
            )
          )
        )
         (
          let (
            (
              weights (
                _list 0 0 1
              )
            )
          )
           (
            begin (
              let (
                (
                  result (
                    procentual_proximity vehicles weights
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        to-str-space result
                      )
                    )
                     (
                      to-str-space result
                    )
                     (
                      to-str (
                        to-str-space result
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
