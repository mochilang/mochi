;; Generated on 2025-08-09 10:22 +0700
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
(define _floor floor)
(define (fmod a b) (- a (* (_floor (/ a b)) b)))
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
      start4 (
        current-jiffy
      )
    )
     (
      jps7 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          N "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
        )
      )
       (
        begin (
          define (
            solution n
          )
           (
            let (
              (
                max_product 0
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
                    letrec (
                      (
                        loop1 (
                          lambda (
                            
                          )
                           (
                            if (
                              <= i (
                                - (
                                  _len n
                                )
                                 13
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    product 1
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
                                        letrec (
                                          (
                                            loop2 (
                                              lambda (
                                                
                                              )
                                               (
                                                if (
                                                  < j 13
                                                )
                                                 (
                                                  begin (
                                                    set! product (
                                                      * product (
                                                        let (
                                                          (
                                                            v3 (
                                                              _substring n (
                                                                + i j
                                                              )
                                                               (
                                                                + (
                                                                  + i j
                                                                )
                                                                 1
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          cond (
                                                            (
                                                              string? v3
                                                            )
                                                             (
                                                              inexact->exact (
                                                                _floor (
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
                                                                _floor v3
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
                                       (
                                        if (
                                          > product max_product
                                        )
                                         (
                                          begin (
                                            set! max_product product
                                          )
                                        )
                                         '(
                                          
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
                                loop1
                              )
                            )
                             '(
                              
                            )
                          )
                        )
                      )
                    )
                     (
                      loop1
                    )
                  )
                   max_product
                )
              )
            )
          )
        )
         (
          _display (
            if (
              string? (
                solution "13978431290823798458352374"
              )
            )
             (
              solution "13978431290823798458352374"
            )
             (
              to-str (
                solution "13978431290823798458352374"
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
                solution "13978431295823798458352374"
              )
            )
             (
              solution "13978431295823798458352374"
            )
             (
              to-str (
                solution "13978431295823798458352374"
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
                solution "1397843129582379841238352374"
              )
            )
             (
              solution "1397843129582379841238352374"
            )
             (
              to-str (
                solution "1397843129582379841238352374"
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
                solution N
              )
            )
             (
              solution N
            )
             (
              to-str (
                solution N
              )
            )
          )
        )
         (
          newline
        )
      )
    )
     (
      let (
        (
          end5 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur6 (
              quotient (
                * (
                  - end5 start4
                )
                 1000000
              )
               jps7
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur6
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
