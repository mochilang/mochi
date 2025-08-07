;; Generated on 2025-08-07 08:20 +0700
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
      define (
        make_stack limit
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              alist->hash-table (
                _list (
                  cons "items" (
                    _list
                  )
                )
                 (
                  cons "limit" limit
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        is_empty s
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            ret2 (
              equal? (
                _len (
                  hash-table-ref s "items"
                )
              )
               0
            )
          )
        )
      )
    )
     (
      define (
        size s
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            ret3 (
              _len (
                hash-table-ref s "items"
              )
            )
          )
        )
      )
    )
     (
      define (
        is_full s
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            ret4 (
              >= (
                _len (
                  hash-table-ref s "items"
                )
              )
               (
                hash-table-ref s "limit"
              )
            )
          )
        )
      )
    )
     (
      define (
        push s item
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              if (
                is_full s
              )
               (
                begin (
                  panic "stack overflow"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              hash-table-set! s "items" (
                append (
                  hash-table-ref s "items"
                )
                 (
                  _list item
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        pop s
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            begin (
              if (
                is_empty s
              )
               (
                begin (
                  panic "stack underflow"
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
                  n (
                    _len (
                      hash-table-ref s "items"
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      val (
                        list-ref (
                          hash-table-ref s "items"
                        )
                         (
                          - n 1
                        )
                      )
                    )
                  )
                   (
                    begin (
                      hash-table-set! s "items" (
                        take (
                          drop (
                            hash-table-ref s "items"
                          )
                           0
                        )
                         (
                          - (
                            - n 1
                          )
                           0
                        )
                      )
                    )
                     (
                      ret6 val
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
        peek s
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              if (
                is_empty s
              )
               (
                begin (
                  panic "peek from empty stack"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret7 (
                list-ref (
                  hash-table-ref s "items"
                )
                 (
                  - (
                    _len (
                      hash-table-ref s "items"
                    )
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
        contains s item
      )
       (
        call/cc (
          lambda (
            ret8
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
                              < i (
                                _len (
                                  hash-table-ref s "items"
                                )
                              )
                            )
                             (
                              begin (
                                if (
                                  equal? (
                                    list-ref (
                                      hash-table-ref s "items"
                                    )
                                     i
                                  )
                                   item
                                )
                                 (
                                  begin (
                                    ret8 #t
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
                ret8 #f
              )
            )
          )
        )
      )
    )
     (
      define (
        stack_repr s
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            ret11 (
              to-str-space (
                hash-table-ref s "items"
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
            ret12
          )
           (
            let (
              (
                s (
                  make_stack 5
                )
              )
            )
             (
              begin (
                _display (
                  if (
                    string? (
                      to-str-space (
                        is_empty s
                      )
                    )
                  )
                   (
                    to-str-space (
                      is_empty s
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        is_empty s
                      )
                    )
                  )
                )
              )
               (
                newline
              )
               (
                push s 0
              )
               (
                push s 1
              )
               (
                push s 2
              )
               (
                _display (
                  if (
                    string? (
                      to-str-space (
                        peek s
                      )
                    )
                  )
                   (
                    to-str-space (
                      peek s
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        peek s
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
                        size s
                      )
                    )
                  )
                   (
                    to-str-space (
                      size s
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        size s
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
                        is_full s
                      )
                    )
                  )
                   (
                    to-str-space (
                      is_full s
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        is_full s
                      )
                    )
                  )
                )
              )
               (
                newline
              )
               (
                push s 3
              )
               (
                push s 4
              )
               (
                _display (
                  if (
                    string? (
                      to-str-space (
                        is_full s
                      )
                    )
                  )
                   (
                    to-str-space (
                      is_full s
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        is_full s
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
                      stack_repr s
                    )
                  )
                   (
                    stack_repr s
                  )
                   (
                    to-str (
                      stack_repr s
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
                        pop s
                      )
                    )
                  )
                   (
                    to-str-space (
                      pop s
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        pop s
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
                        peek s
                      )
                    )
                  )
                   (
                    to-str-space (
                      peek s
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        peek s
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
                        contains s 1
                      )
                    )
                  )
                   (
                    to-str-space (
                      contains s 1
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        contains s 1
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
                        contains s 9
                      )
                    )
                  )
                   (
                    to-str-space (
                      contains s 9
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        contains s 9
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
     (
      main
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
