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
      start12 (
        current-jiffy
      )
    )
     (
      jps15 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        mod_pow base exp mod
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                result 1
              )
            )
             (
              begin (
                let (
                  (
                    b (
                      modulo base mod
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        e exp
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
                                      > e 0
                                    )
                                     (
                                      begin (
                                        if (
                                          equal? (
                                            modulo e 2
                                          )
                                           1
                                        )
                                         (
                                          begin (
                                            set! result (
                                              modulo (
                                                * result b
                                              )
                                               mod
                                            )
                                          )
                                        )
                                         (
                                          quote (
                                            
                                          )
                                        )
                                      )
                                       (
                                        set! e (
                                          quotient e 2
                                        )
                                      )
                                       (
                                        set! b (
                                          modulo (
                                            * b b
                                          )
                                           mod
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
                        ret1 result
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
        contains xs val
      )
       (
        call/cc (
          lambda (
            ret4
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
                              < i (
                                _len xs
                              )
                            )
                             (
                              begin (
                                if (
                                  equal? (
                                    list-ref xs i
                                  )
                                   val
                                )
                                 (
                                  begin (
                                    ret4 #t
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
                ret4 #f
              )
            )
          )
        )
      )
    )
     (
      define (
        find_primitive modulus
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                r 1
              )
            )
             (
              begin (
                call/cc (
                  lambda (
                    break9
                  )
                   (
                    letrec (
                      (
                        loop8 (
                          lambda (
                            
                          )
                           (
                            if (
                              < r modulus
                            )
                             (
                              begin (
                                let (
                                  (
                                    li (
                                      _list
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        x 0
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            valid #t
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
                                                          and (
                                                            < x (
                                                              - modulus 1
                                                            )
                                                          )
                                                           valid
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                value (
                                                                  mod_pow r x modulus
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  contains li value
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! valid #f
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! li (
                                                                      append li (
                                                                        _list value
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! x (
                                                                      + x 1
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
                                            if valid (
                                              begin (
                                                ret7 r
                                              )
                                            )
                                             (
                                              quote (
                                                
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
                                )
                              )
                               (
                                loop8
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
                      loop8
                    )
                  )
                )
              )
               (
                ret7 (
                  - 1
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
            find_primitive 7
          )
        )
         (
          find_primitive 7
        )
         (
          to-str (
            find_primitive 7
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
            find_primitive 11
          )
        )
         (
          find_primitive 11
        )
         (
          to-str (
            find_primitive 11
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
            find_primitive 8
          )
        )
         (
          find_primitive 8
        )
         (
          to-str (
            find_primitive 8
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
          prime 23
        )
      )
       (
        begin (
          let (
            (
              primitive_root (
                find_primitive prime
              )
            )
          )
           (
            begin (
              if (
                equal? primitive_root (
                  - 0 1
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? "Cannot find the primitive for the value: -1"
                    )
                     "Cannot find the primitive for the value: -1" (
                      to-str "Cannot find the primitive for the value: -1"
                    )
                  )
                )
                 (
                  newline
                )
              )
               (
                begin (
                  let (
                    (
                      a_private 6
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          b_private 15
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              a_public (
                                mod_pow primitive_root a_private prime
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  b_public (
                                    mod_pow primitive_root b_private prime
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      a_secret (
                                        mod_pow b_public a_private prime
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          b_secret (
                                            mod_pow a_public b_private prime
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          _display (
                                            if (
                                              string? (
                                                string-append "The key value generated by A is: " (
                                                  to-str-space a_secret
                                                )
                                              )
                                            )
                                             (
                                              string-append "The key value generated by A is: " (
                                                to-str-space a_secret
                                              )
                                            )
                                             (
                                              to-str (
                                                string-append "The key value generated by A is: " (
                                                  to-str-space a_secret
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
                                                string-append "The key value generated by B is: " (
                                                  to-str-space b_secret
                                                )
                                              )
                                            )
                                             (
                                              string-append "The key value generated by B is: " (
                                                to-str-space b_secret
                                              )
                                            )
                                             (
                                              to-str (
                                                string-append "The key value generated by B is: " (
                                                  to-str-space b_secret
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
        )
      )
    )
     (
      let (
        (
          end13 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur14 (
              quotient (
                * (
                  - end13 start12
                )
                 1000000
              )
               jps15
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur14
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
