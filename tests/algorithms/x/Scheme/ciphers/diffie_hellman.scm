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
      start10 (
        current-jiffy
      )
    )
     (
      jps13 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        int_to_hex n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                equal? n 0
              )
               (
                begin (
                  ret1 "0"
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
                  digits "0123456789abcdef"
                )
              )
               (
                begin (
                  let (
                    (
                      num n
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          res ""
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
                                        > num 0
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              d (
                                                modulo num 16
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! res (
                                                string-append (
                                                  _substring digits d (
                                                    + d 1
                                                  )
                                                )
                                                 res
                                              )
                                            )
                                             (
                                              set! num (
                                                quotient num 16
                                              )
                                            )
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
                          ret1 res
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
          seed 123456789
        )
      )
       (
        begin (
          define (
            rand_int
          )
           (
            call/cc (
              lambda (
                ret4
              )
               (
                begin (
                  set! seed (
                    modulo (
                      + (
                        * 1103515245 seed
                      )
                       12345
                    )
                     2147483648
                  )
                )
                 (
                  ret4 seed
                )
              )
            )
          )
        )
         (
          let (
            (
              PRIME 23
            )
          )
           (
            begin (
              define (
                mod_pow base exp
              )
               (
                call/cc (
                  lambda (
                    ret5
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
                              modulo base PRIME
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
                                                       PRIME
                                                    )
                                                  )
                                                )
                                                 (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                               (
                                                set! b (
                                                  modulo (
                                                    * b b
                                                  )
                                                   PRIME
                                                )
                                              )
                                               (
                                                set! e (
                                                  quotient e 2
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
                is_valid_public_key key
              )
               (
                call/cc (
                  lambda (
                    ret8
                  )
                   (
                    begin (
                      if (
                        or (
                          < key 2
                        )
                         (
                          > key (
                            - PRIME 2
                          )
                        )
                      )
                       (
                        begin (
                          ret8 #f
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret8 (
                        equal? (
                          mod_pow key (
                            quotient (
                              - PRIME 1
                            )
                             2
                          )
                        )
                         1
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                generate_private_key
              )
               (
                call/cc (
                  lambda (
                    ret9
                  )
                   (
                    ret9 (
                      _add (
                        fmod (
                          rand_int
                        )
                         (
                          - PRIME 2
                        )
                      )
                       2
                    )
                  )
                )
              )
            )
             (
              let (
                (
                  generator 5
                )
              )
               (
                begin (
                  let (
                    (
                      alice_private (
                        generate_private_key
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          alice_public (
                            mod_pow generator alice_private
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              bob_private (
                                generate_private_key
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  bob_public (
                                    mod_pow generator bob_private
                                  )
                                )
                              )
                               (
                                begin (
                                  if (
                                    not (
                                      is_valid_public_key alice_public
                                    )
                                  )
                                   (
                                    begin (
                                      panic "Invalid public key"
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
                                      is_valid_public_key bob_public
                                    )
                                  )
                                   (
                                    begin (
                                      panic "Invalid public key"
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
                                      alice_shared (
                                        mod_pow bob_public alice_private
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          bob_shared (
                                            mod_pow alice_public bob_private
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          _display (
                                            if (
                                              string? (
                                                int_to_hex alice_shared
                                              )
                                            )
                                             (
                                              int_to_hex alice_shared
                                            )
                                             (
                                              to-str (
                                                int_to_hex alice_shared
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
                                                int_to_hex bob_shared
                                              )
                                            )
                                             (
                                              int_to_hex bob_shared
                                            )
                                             (
                                              to-str (
                                                int_to_hex bob_shared
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
          end11 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur12 (
              quotient (
                * (
                  - end11 start10
                )
                 1000000
              )
               jps13
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur12
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
