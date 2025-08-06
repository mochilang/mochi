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
      start13 (
        current-jiffy
      )
    )
     (
      jps16 (
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
          define (
            set_seed s
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                set! seed s
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
                  ret2 (
                    + (
                      modulo seed (
                        + (
                          - b a
                        )
                         1
                      )
                    )
                     a
                  )
                )
              )
            )
          )
        )
         (
          let (
            (
              ascii_chars " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
            )
          )
           (
            begin (
              define (
                ord ch
              )
               (
                call/cc (
                  lambda (
                    ret3
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
                                        _len ascii_chars
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          string=? (
                                            _substring ascii_chars i (
                                              + i 1
                                            )
                                          )
                                           ch
                                        )
                                         (
                                          begin (
                                            ret3 (
                                              + 32 i
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
                        ret3 0
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                chr code
              )
               (
                call/cc (
                  lambda (
                    ret6
                  )
                   (
                    begin (
                      if (
                        or (
                          < code 32
                        )
                         (
                          > code 126
                        )
                      )
                       (
                        begin (
                          ret6 ""
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret6 (
                        _substring ascii_chars (
                          - code 32
                        )
                         (
                          + (
                            - code 32
                          )
                           1
                        )
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                encrypt text
              )
               (
                call/cc (
                  lambda (
                    ret7
                  )
                   (
                    let (
                      (
                        cipher (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            key (
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
                                              < i (
                                                _len text
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    p (
                                                      ord (
                                                        _substring text i (
                                                          + i 1
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        k (
                                                          randint 1 300
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            c (
                                                              * (
                                                                _add p k
                                                              )
                                                               k
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! cipher (
                                                              append cipher (
                                                                _list c
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! key (
                                                              append key (
                                                                _list k
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
                                    hash-table-set! res "cipher" cipher
                                  )
                                   (
                                    hash-table-set! res "key" key
                                  )
                                   (
                                    ret7 res
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
                decrypt cipher key
              )
               (
                call/cc (
                  lambda (
                    ret10
                  )
                   (
                    let (
                      (
                        plain ""
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
                                            _len key
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                p (
                                                  quotient (
                                                    - (
                                                      list-ref cipher i
                                                    )
                                                     (
                                                      * (
                                                        list-ref key i
                                                      )
                                                       (
                                                        list-ref key i
                                                      )
                                                    )
                                                  )
                                                   (
                                                    list-ref key i
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! plain (
                                                  string-append plain (
                                                    chr p
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
                            ret10 plain
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
             (
              set_seed 1
            )
             (
              let (
                (
                  res (
                    encrypt "Hello"
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      cipher (
                        cond (
                          (
                            string? res
                          )
                           (
                            _substring res "cipher" (
                              + "cipher" 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? res
                          )
                           (
                            hash-table-ref res "cipher"
                          )
                        )
                         (
                          else (
                            list-ref res "cipher"
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          key (
                            cond (
                              (
                                string? res
                              )
                               (
                                _substring res "key" (
                                  + "key" 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? res
                              )
                               (
                                hash-table-ref res "key"
                              )
                            )
                             (
                              else (
                                list-ref res "key"
                              )
                            )
                          )
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? cipher
                            )
                             cipher (
                              to-str cipher
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          _display (
                            if (
                              string? key
                            )
                             key (
                              to-str key
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
                                decrypt cipher key
                              )
                            )
                             (
                              decrypt cipher key
                            )
                             (
                              to-str (
                                decrypt cipher key
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
     (
      let (
        (
          end14 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur15 (
              quotient (
                * (
                  - end14 start13
                )
                 1000000
              )
               jps16
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur15
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
