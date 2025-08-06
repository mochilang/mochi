;; Generated on 2025-08-06 23:57 +0700
(import (scheme base))
(import (scheme time))
(import (chibi string))
(import (only (scheme char) string-upcase string-downcase))
(import (srfi 69))
(import (srfi 1))
(define _list list)
(import (chibi io))
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
(define (_input)
  (let ((l (read-line)))
    (if (eof-object? l) "" l)))
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
        binomial_coefficient n k
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
                    kk k
                  )
                )
                 (
                  begin (
                    if (
                      > k (
                        - n k
                      )
                    )
                     (
                      begin (
                        set! kk (
                          - n k
                        )
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
                        break3
                      )
                       (
                        letrec (
                          (
                            loop2 (
                              lambda (
                                i
                              )
                               (
                                if (
                                  < i kk
                                )
                                 (
                                  begin (
                                    begin (
                                      set! result (
                                        * result (
                                          - n i
                                        )
                                      )
                                    )
                                     (
                                      set! result (
                                        _div result (
                                          _add i 1
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop2 (
                                      + i 1
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
                        )
                         (
                          loop2 0
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
        catalan_number node_count
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            ret4 (
              _div (
                binomial_coefficient (
                  * 2 node_count
                )
                 node_count
              )
               (
                + node_count 1
              )
            )
          )
        )
      )
    )
     (
      define (
        factorial n
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              if (
                < n 0
              )
               (
                begin (
                  _display (
                    if (
                      string? "factorial() not defined for negative values"
                    )
                     "factorial() not defined for negative values" (
                      to-str "factorial() not defined for negative values"
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  ret5 0
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
                  result 1
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
                              i
                            )
                             (
                              if (
                                < i (
                                  + n 1
                                )
                              )
                               (
                                begin (
                                  begin (
                                    set! result (
                                      * result i
                                    )
                                  )
                                )
                                 (
                                  loop6 (
                                    + i 1
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
                      )
                       (
                        loop6 1
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
     (
      define (
        binary_tree_count node_count
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            ret8 (
              * (
                catalan_number node_count
              )
               (
                factorial node_count
              )
            )
          )
        )
      )
    )
     (
      _display (
        if (
          string? "Enter the number of nodes:"
        )
         "Enter the number of nodes:" (
          to-str "Enter the number of nodes:"
        )
      )
    )
     (
      newline
    )
     (
      let (
        (
          input_str (
            _input
          )
        )
      )
       (
        begin (
          let (
            (
              node_count (
                let (
                  (
                    v9 input_str
                  )
                )
                 (
                  cond (
                    (
                      string? v9
                    )
                     (
                      exact (
                        floor (
                          string->number v9
                        )
                      )
                    )
                  )
                   (
                    (
                      boolean? v9
                    )
                     (
                      if v9 1 0
                    )
                  )
                   (
                    else (
                      exact (
                        floor v9
                      )
                    )
                  )
                )
              )
            )
          )
           (
            begin (
              if (
                <= node_count 0
              )
               (
                begin (
                  _display (
                    if (
                      string? "We need some nodes to work with."
                    )
                     "We need some nodes to work with." (
                      to-str "We need some nodes to work with."
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
                      bst (
                        catalan_number node_count
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          bt (
                            binary_tree_count node_count
                          )
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "Given"
                            )
                             "Given" (
                              to-str "Given"
                            )
                          )
                        )
                         (
                          _display " "
                        )
                         (
                          _display (
                            if (
                              string? node_count
                            )
                             node_count (
                              to-str node_count
                            )
                          )
                        )
                         (
                          _display " "
                        )
                         (
                          _display (
                            if (
                              string? "nodes, there are"
                            )
                             "nodes, there are" (
                              to-str "nodes, there are"
                            )
                          )
                        )
                         (
                          _display " "
                        )
                         (
                          _display (
                            if (
                              string? bt
                            )
                             bt (
                              to-str bt
                            )
                          )
                        )
                         (
                          _display " "
                        )
                         (
                          _display (
                            if (
                              string? "binary trees and"
                            )
                             "binary trees and" (
                              to-str "binary trees and"
                            )
                          )
                        )
                         (
                          _display " "
                        )
                         (
                          _display (
                            if (
                              string? bst
                            )
                             bst (
                              to-str bst
                            )
                          )
                        )
                         (
                          _display " "
                        )
                         (
                          _display (
                            if (
                              string? "binary search trees."
                            )
                             "binary search trees." (
                              to-str "binary search trees."
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
