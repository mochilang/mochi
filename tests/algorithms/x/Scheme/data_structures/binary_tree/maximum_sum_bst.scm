;; Generated on 2025-08-06 23:57 +0700
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
        min_int a b
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                < a b
              )
               (
                begin (
                  ret1 a
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret1 b
            )
          )
        )
      )
    )
     (
      define (
        max_int a b
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                > a b
              )
               (
                begin (
                  ret2 a
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret2 b
            )
          )
        )
      )
    )
     (
      define (
        solver nodes idx
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            begin (
              if (
                equal? idx (
                  - 0 1
                )
              )
               (
                begin (
                  ret3 (
                    alist->hash-table (
                      _list (
                        cons "is_bst" #t
                      )
                       (
                        cons "min_val" 2147483647
                      )
                       (
                        cons "max_val" (
                          - 2147483648
                        )
                      )
                       (
                        cons "total" 0
                      )
                       (
                        cons "best" 0
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
                  node (
                    list-ref nodes idx
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      left_info (
                        solver nodes (
                          hash-table-ref node "left"
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          right_info (
                            solver nodes (
                              hash-table-ref node "right"
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              current_best (
                                max_int (
                                  hash-table-ref left_info "best"
                                )
                                 (
                                  hash-table-ref right_info "best"
                                )
                              )
                            )
                          )
                           (
                            begin (
                              if (
                                and (
                                  and (
                                    and (
                                      hash-table-ref left_info "is_bst"
                                    )
                                     (
                                      hash-table-ref right_info "is_bst"
                                    )
                                  )
                                   (
                                    _lt (
                                      hash-table-ref left_info "max_val"
                                    )
                                     (
                                      hash-table-ref node "val"
                                    )
                                  )
                                )
                                 (
                                  _lt (
                                    hash-table-ref node "val"
                                  )
                                   (
                                    hash-table-ref right_info "min_val"
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      sum_val (
                                        _add (
                                          _add (
                                            hash-table-ref left_info "total"
                                          )
                                           (
                                            hash-table-ref right_info "total"
                                          )
                                        )
                                         (
                                          hash-table-ref node "val"
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      set! current_best (
                                        max_int current_best sum_val
                                      )
                                    )
                                     (
                                      ret3 (
                                        alist->hash-table (
                                          _list (
                                            cons "is_bst" #t
                                          )
                                           (
                                            cons "min_val" (
                                              min_int (
                                                hash-table-ref left_info "min_val"
                                              )
                                               (
                                                hash-table-ref node "val"
                                              )
                                            )
                                          )
                                           (
                                            cons "max_val" (
                                              max_int (
                                                hash-table-ref right_info "max_val"
                                              )
                                               (
                                                hash-table-ref node "val"
                                              )
                                            )
                                          )
                                           (
                                            cons "total" sum_val
                                          )
                                           (
                                            cons "best" current_best
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
                              ret3 (
                                alist->hash-table (
                                  _list (
                                    cons "is_bst" #f
                                  )
                                   (
                                    cons "min_val" 0
                                  )
                                   (
                                    cons "max_val" 0
                                  )
                                   (
                                    cons "total" 0
                                  )
                                   (
                                    cons "best" current_best
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
      define (
        max_sum_bst nodes root
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                info (
                  solver nodes root
                )
              )
            )
             (
              begin (
                ret4 (
                  hash-table-ref info "best"
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
                t1_nodes (
                  _list (
                    alist->hash-table (
                      _list (
                        cons "val" 4
                      )
                       (
                        cons "left" 1
                      )
                       (
                        cons "right" (
                          - 0 1
                        )
                      )
                    )
                  )
                   (
                    alist->hash-table (
                      _list (
                        cons "val" 3
                      )
                       (
                        cons "left" 2
                      )
                       (
                        cons "right" 3
                      )
                    )
                  )
                   (
                    alist->hash-table (
                      _list (
                        cons "val" 1
                      )
                       (
                        cons "left" (
                          - 0 1
                        )
                      )
                       (
                        cons "right" (
                          - 0 1
                        )
                      )
                    )
                  )
                   (
                    alist->hash-table (
                      _list (
                        cons "val" 2
                      )
                       (
                        cons "left" (
                          - 0 1
                        )
                      )
                       (
                        cons "right" (
                          - 0 1
                        )
                      )
                    )
                  )
                )
              )
            )
             (
              begin (
                _display (
                  if (
                    string? (
                      max_sum_bst t1_nodes 0
                    )
                  )
                   (
                    max_sum_bst t1_nodes 0
                  )
                   (
                    to-str (
                      max_sum_bst t1_nodes 0
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
                    t2_nodes (
                      _list (
                        alist->hash-table (
                          _list (
                            cons "val" (
                              - 4
                            )
                          )
                           (
                            cons "left" 1
                          )
                           (
                            cons "right" 2
                          )
                        )
                      )
                       (
                        alist->hash-table (
                          _list (
                            cons "val" (
                              - 2
                            )
                          )
                           (
                            cons "left" (
                              - 0 1
                            )
                          )
                           (
                            cons "right" (
                              - 0 1
                            )
                          )
                        )
                      )
                       (
                        alist->hash-table (
                          _list (
                            cons "val" (
                              - 5
                            )
                          )
                           (
                            cons "left" (
                              - 0 1
                            )
                          )
                           (
                            cons "right" (
                              - 0 1
                            )
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    _display (
                      if (
                        string? (
                          max_sum_bst t2_nodes 0
                        )
                      )
                       (
                        max_sum_bst t2_nodes 0
                      )
                       (
                        to-str (
                          max_sum_bst t2_nodes 0
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
                        t3_nodes (
                          _list (
                            alist->hash-table (
                              _list (
                                cons "val" 1
                              )
                               (
                                cons "left" 1
                              )
                               (
                                cons "right" 2
                              )
                            )
                          )
                           (
                            alist->hash-table (
                              _list (
                                cons "val" 4
                              )
                               (
                                cons "left" 3
                              )
                               (
                                cons "right" 4
                              )
                            )
                          )
                           (
                            alist->hash-table (
                              _list (
                                cons "val" 3
                              )
                               (
                                cons "left" 5
                              )
                               (
                                cons "right" 6
                              )
                            )
                          )
                           (
                            alist->hash-table (
                              _list (
                                cons "val" 2
                              )
                               (
                                cons "left" (
                                  - 0 1
                                )
                              )
                               (
                                cons "right" (
                                  - 0 1
                                )
                              )
                            )
                          )
                           (
                            alist->hash-table (
                              _list (
                                cons "val" 4
                              )
                               (
                                cons "left" (
                                  - 0 1
                                )
                              )
                               (
                                cons "right" (
                                  - 0 1
                                )
                              )
                            )
                          )
                           (
                            alist->hash-table (
                              _list (
                                cons "val" 2
                              )
                               (
                                cons "left" (
                                  - 0 1
                                )
                              )
                               (
                                cons "right" (
                                  - 0 1
                                )
                              )
                            )
                          )
                           (
                            alist->hash-table (
                              _list (
                                cons "val" 5
                              )
                               (
                                cons "left" 7
                              )
                               (
                                cons "right" 8
                              )
                            )
                          )
                           (
                            alist->hash-table (
                              _list (
                                cons "val" 4
                              )
                               (
                                cons "left" (
                                  - 0 1
                                )
                              )
                               (
                                cons "right" (
                                  - 0 1
                                )
                              )
                            )
                          )
                           (
                            alist->hash-table (
                              _list (
                                cons "val" 6
                              )
                               (
                                cons "left" (
                                  - 0 1
                                )
                              )
                               (
                                cons "right" (
                                  - 0 1
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        _display (
                          if (
                            string? (
                              max_sum_bst t3_nodes 0
                            )
                          )
                           (
                            max_sum_bst t3_nodes 0
                          )
                           (
                            to-str (
                              max_sum_bst t3_nodes 0
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
