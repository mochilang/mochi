;; Generated on 2025-08-06 21:38 +0700
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
      start9 (
        current-jiffy
      )
    )
     (
      jps12 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        pow2 exp
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
                                  < i exp
                                )
                                 (
                                  begin (
                                    set! result (
                                      * result 2
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
                    ret1 result
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
        is_bit_set number position
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                shifted (
                  / number (
                    pow2 position
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    remainder (
                      fmod shifted 2
                    )
                  )
                )
                 (
                  begin (
                    ret4 (
                      equal? remainder 1
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
        set_bit number position
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              if (
                is_bit_set number position
              )
               (
                begin (
                  ret5 number
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret5 (
                _add number (
                  pow2 position
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        clear_bit number position
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            begin (
              if (
                is_bit_set number position
              )
               (
                begin (
                  ret6 (
                    - number (
                      pow2 position
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
              ret6 number
            )
          )
        )
      )
    )
     (
      define (
        flip_bit number position
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              if (
                is_bit_set number position
              )
               (
                begin (
                  ret7 (
                    - number (
                      pow2 position
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
              ret7 (
                _add number (
                  pow2 position
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        get_bit number position
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            begin (
              if (
                is_bit_set number position
              )
               (
                begin (
                  ret8 1
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret8 0
            )
          )
        )
      )
    )
     (
      _display (
        if (
          string? (
            to-str-space (
              set_bit 13 1
            )
          )
        )
         (
          to-str-space (
            set_bit 13 1
          )
        )
         (
          to-str (
            to-str-space (
              set_bit 13 1
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
            to-str-space (
              clear_bit 18 1
            )
          )
        )
         (
          to-str-space (
            clear_bit 18 1
          )
        )
         (
          to-str (
            to-str-space (
              clear_bit 18 1
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
            to-str-space (
              flip_bit 5 1
            )
          )
        )
         (
          to-str-space (
            flip_bit 5 1
          )
        )
         (
          to-str (
            to-str-space (
              flip_bit 5 1
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
            to-str-space (
              is_bit_set 10 3
            )
          )
        )
         (
          to-str-space (
            is_bit_set 10 3
          )
        )
         (
          to-str (
            to-str-space (
              is_bit_set 10 3
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
            to-str-space (
              get_bit 10 1
            )
          )
        )
         (
          to-str-space (
            get_bit 10 1
          )
        )
         (
          to-str (
            to-str-space (
              get_bit 10 1
            )
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
          end10 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur11 (
              quotient (
                * (
                  - end10 start9
                )
                 1000000
              )
               jps12
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur11
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
