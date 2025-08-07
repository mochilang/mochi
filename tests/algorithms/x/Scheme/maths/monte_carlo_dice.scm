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
      let (
        (
          lcg_seed 1
        )
      )
       (
        begin (
          define (
            lcg_rand
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                begin (
                  set! lcg_seed (
                    _mod (
                      + (
                        * lcg_seed 1103515245
                      )
                       12345
                    )
                     2147483648
                  )
                )
                 (
                  ret1 lcg_seed
                )
              )
            )
          )
        )
         (
          define (
            roll
          )
           (
            call/cc (
              lambda (
                ret2
              )
               (
                let (
                  (
                    rv (
                      + 0.0 (
                        lcg_rand
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        r (
                          _div (
                            * rv 6.0
                          )
                           2147483648.0
                        )
                      )
                    )
                     (
                      begin (
                        ret2 (
                          + 1 (
                            let (
                              (
                                v3 r
                              )
                            )
                             (
                              cond (
                                (
                                  string? v3
                                )
                                 (
                                  inexact->exact (
                                    floor (
                                      string->number v3
                                    )
                                  )
                                )
                              )
                               (
                                (
                                  boolean? v3
                                )
                                 (
                                  if v3 1 0
                                )
                              )
                               (
                                else (
                                  inexact->exact (
                                    floor v3
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
            round2 x
          )
           (
            call/cc (
              lambda (
                ret4
              )
               (
                let (
                  (
                    y (
                      _add (
                        * x 100.0
                      )
                       0.5
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        z (
                          let (
                            (
                              v5 y
                            )
                          )
                           (
                            cond (
                              (
                                string? v5
                              )
                               (
                                inexact->exact (
                                  floor (
                                    string->number v5
                                  )
                                )
                              )
                            )
                             (
                              (
                                boolean? v5
                              )
                               (
                                if v5 1 0
                              )
                            )
                             (
                              else (
                                inexact->exact (
                                  floor v5
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        ret4 (
                          _div (
                            + 0.0 z
                          )
                           100.0
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
            throw_dice num_throws num_dice
          )
           (
            call/cc (
              lambda (
                ret6
              )
               (
                let (
                  (
                    count_of_sum (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        max_sum (
                          + (
                            * num_dice 6
                          )
                           1
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
                                          < i max_sum
                                        )
                                         (
                                          begin (
                                            set! count_of_sum (
                                              append count_of_sum (
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
                            let (
                              (
                                t 0
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
                                              < t num_throws
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    s 0
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        d 0
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
                                                                      < d num_dice
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! s (
                                                                          _add s (
                                                                            roll
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        set! d (
                                                                          + d 1
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
                                                        list-set! count_of_sum s (
                                                          + (
                                                            list-ref count_of_sum s
                                                          )
                                                           1
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
                                let (
                                  (
                                    probability (
                                      _list
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! i num_dice
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
                                                  < i max_sum
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        p (
                                                          _div (
                                                            * (
                                                              + 0.0 (
                                                                list-ref count_of_sum i
                                                              )
                                                            )
                                                             100.0
                                                          )
                                                           (
                                                            + 0.0 num_throws
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! probability (
                                                          append probability (
                                                            _list (
                                                              round2 p
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
                                    ret6 probability
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
                  set! lcg_seed 1
                )
                 (
                  let (
                    (
                      result (
                        throw_dice 10000 2
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
          main
        )
      )
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
