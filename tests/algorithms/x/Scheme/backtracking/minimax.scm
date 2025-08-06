;; Generated on 2025-08-06 16:21 +0700
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
      start6 (
        current-jiffy
      )
    )
     (
      jps9 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        minimax depth node_index is_max scores height
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                < depth 0
              )
               (
                begin (
                  panic "Depth cannot be less than 0"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                equal? (
                  _len scores
                )
                 0
              )
               (
                begin (
                  panic "Scores cannot be empty"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                equal? depth height
              )
               (
                begin (
                  ret1 (
                    list-ref scores node_index
                  )
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if is_max (
                begin (
                  let (
                    (
                      left (
                        minimax (
                          + depth 1
                        )
                         (
                          * node_index 2
                        )
                         #f scores height
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          right (
                            minimax (
                              + depth 1
                            )
                             (
                              + (
                                * node_index 2
                              )
                               1
                            )
                             #f scores height
                          )
                        )
                      )
                       (
                        begin (
                          if (
                            _gt left right
                          )
                           (
                            begin (
                              ret1 left
                            )
                          )
                           (
                            begin (
                              ret1 right
                            )
                          )
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
              let (
                (
                  left (
                    minimax (
                      + depth 1
                    )
                     (
                      * node_index 2
                    )
                     #t scores height
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      right (
                        minimax (
                          + depth 1
                        )
                         (
                          + (
                            * node_index 2
                          )
                           1
                        )
                         #t scores height
                      )
                    )
                  )
                   (
                    begin (
                      if (
                        _lt left right
                      )
                       (
                        begin (
                          ret1 left
                        )
                      )
                       (
                        begin (
                          ret1 right
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
        tree_height n
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                h 0
              )
            )
             (
              begin (
                let (
                  (
                    v n
                  )
                )
                 (
                  begin (
                    call/cc (
                      lambda (
                        break4
                      )
                       (
                        letrec (
                          (
                            loop3 (
                              lambda (
                                
                              )
                               (
                                if (
                                  > v 1
                                )
                                 (
                                  begin (
                                    set! v (
                                      quotient v 2
                                    )
                                  )
                                   (
                                    set! h (
                                      + h 1
                                    )
                                  )
                                   (
                                    loop3
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
                          loop3
                        )
                      )
                    )
                  )
                   (
                    ret2 h
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
            ret5
          )
           (
            let (
              (
                scores (
                  _list 90 23 6 33 21 65 123 34423
                )
              )
            )
             (
              begin (
                let (
                  (
                    height (
                      tree_height (
                        _len scores
                      )
                    )
                  )
                )
                 (
                  begin (
                    _display (
                      if (
                        string? (
                          string-append "Optimal value : " (
                            to-str-space (
                              minimax 0 0 #t scores height
                            )
                          )
                        )
                      )
                       (
                        string-append "Optimal value : " (
                          to-str-space (
                            minimax 0 0 #t scores height
                          )
                        )
                      )
                       (
                        to-str (
                          string-append "Optimal value : " (
                            to-str-space (
                              minimax 0 0 #t scores height
                            )
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
     (
      main
    )
     (
      let (
        (
          end7 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur8 (
              quotient (
                * (
                  - end7 start6
                )
                 1000000
              )
               jps9
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur8
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
