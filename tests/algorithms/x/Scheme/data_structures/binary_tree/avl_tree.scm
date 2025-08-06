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
          NIL (
            - 0 1
          )
        )
      )
       (
        begin (
          let (
            (
              nodes (
                _list
              )
            )
          )
           (
            begin (
              define (
                new_node value
              )
               (
                call/cc (
                  lambda (
                    ret1
                  )
                   (
                    let (
                      (
                        node (
                          alist->hash-table (
                            _list (
                              cons "data" value
                            )
                             (
                              cons "left" NIL
                            )
                             (
                              cons "right" NIL
                            )
                             (
                              cons "height" 1
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        set! nodes (
                          append nodes (
                            _list node
                          )
                        )
                      )
                       (
                        ret1 (
                          - (
                            _len nodes
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
                get_height i
              )
               (
                call/cc (
                  lambda (
                    ret2
                  )
                   (
                    begin (
                      if (
                        equal? i NIL
                      )
                       (
                        begin (
                          ret2 0
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret2 (
                        cond (
                          (
                            string? (
                              list-ref nodes i
                            )
                          )
                           (
                            _substring (
                              list-ref nodes i
                            )
                             "height" (
                              + "height" 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? (
                              list-ref nodes i
                            )
                          )
                           (
                            hash-table-ref (
                              list-ref nodes i
                            )
                             "height"
                          )
                        )
                         (
                          else (
                            list-ref (
                              list-ref nodes i
                            )
                             "height"
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
                my_max a b
              )
               (
                call/cc (
                  lambda (
                    ret3
                  )
                   (
                    begin (
                      if (
                        > a b
                      )
                       (
                        begin (
                          ret3 a
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret3 b
                    )
                  )
                )
              )
            )
             (
              define (
                update_height i
              )
               (
                call/cc (
                  lambda (
                    ret4
                  )
                   (
                    hash-table-set! (
                      list-ref nodes i
                    )
                     "height" (
                      _add (
                        my_max (
                          get_height (
                            cond (
                              (
                                string? (
                                  list-ref nodes i
                                )
                              )
                               (
                                _substring (
                                  list-ref nodes i
                                )
                                 "left" (
                                  + "left" 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? (
                                  list-ref nodes i
                                )
                              )
                               (
                                hash-table-ref (
                                  list-ref nodes i
                                )
                                 "left"
                              )
                            )
                             (
                              else (
                                list-ref (
                                  list-ref nodes i
                                )
                                 "left"
                              )
                            )
                          )
                        )
                         (
                          get_height (
                            cond (
                              (
                                string? (
                                  list-ref nodes i
                                )
                              )
                               (
                                _substring (
                                  list-ref nodes i
                                )
                                 "right" (
                                  + "right" 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? (
                                  list-ref nodes i
                                )
                              )
                               (
                                hash-table-ref (
                                  list-ref nodes i
                                )
                                 "right"
                              )
                            )
                             (
                              else (
                                list-ref (
                                  list-ref nodes i
                                )
                                 "right"
                              )
                            )
                          )
                        )
                      )
                       1
                    )
                  )
                )
              )
            )
             (
              define (
                right_rotation i
              )
               (
                call/cc (
                  lambda (
                    ret5
                  )
                   (
                    let (
                      (
                        left (
                          cond (
                            (
                              string? (
                                list-ref nodes i
                              )
                            )
                             (
                              _substring (
                                list-ref nodes i
                              )
                               "left" (
                                + "left" 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? (
                                list-ref nodes i
                              )
                            )
                             (
                              hash-table-ref (
                                list-ref nodes i
                              )
                               "left"
                            )
                          )
                           (
                            else (
                              list-ref (
                                list-ref nodes i
                              )
                               "left"
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        hash-table-set! (
                          list-ref nodes i
                        )
                         "left" (
                          cond (
                            (
                              string? (
                                list-ref nodes left
                              )
                            )
                             (
                              _substring (
                                list-ref nodes left
                              )
                               "right" (
                                + "right" 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? (
                                list-ref nodes left
                              )
                            )
                             (
                              hash-table-ref (
                                list-ref nodes left
                              )
                               "right"
                            )
                          )
                           (
                            else (
                              list-ref (
                                list-ref nodes left
                              )
                               "right"
                            )
                          )
                        )
                      )
                       (
                        hash-table-set! (
                          list-ref nodes left
                        )
                         "right" i
                      )
                       (
                        update_height i
                      )
                       (
                        update_height left
                      )
                       (
                        ret5 left
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                left_rotation i
              )
               (
                call/cc (
                  lambda (
                    ret6
                  )
                   (
                    let (
                      (
                        right (
                          cond (
                            (
                              string? (
                                list-ref nodes i
                              )
                            )
                             (
                              _substring (
                                list-ref nodes i
                              )
                               "right" (
                                + "right" 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? (
                                list-ref nodes i
                              )
                            )
                             (
                              hash-table-ref (
                                list-ref nodes i
                              )
                               "right"
                            )
                          )
                           (
                            else (
                              list-ref (
                                list-ref nodes i
                              )
                               "right"
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        hash-table-set! (
                          list-ref nodes i
                        )
                         "right" (
                          cond (
                            (
                              string? (
                                list-ref nodes right
                              )
                            )
                             (
                              _substring (
                                list-ref nodes right
                              )
                               "left" (
                                + "left" 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? (
                                list-ref nodes right
                              )
                            )
                             (
                              hash-table-ref (
                                list-ref nodes right
                              )
                               "left"
                            )
                          )
                           (
                            else (
                              list-ref (
                                list-ref nodes right
                              )
                               "left"
                            )
                          )
                        )
                      )
                       (
                        hash-table-set! (
                          list-ref nodes right
                        )
                         "left" i
                      )
                       (
                        update_height i
                      )
                       (
                        update_height right
                      )
                       (
                        ret6 right
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                lr_rotation i
              )
               (
                call/cc (
                  lambda (
                    ret7
                  )
                   (
                    begin (
                      hash-table-set! (
                        list-ref nodes i
                      )
                       "left" (
                        left_rotation (
                          cond (
                            (
                              string? (
                                list-ref nodes i
                              )
                            )
                             (
                              _substring (
                                list-ref nodes i
                              )
                               "left" (
                                + "left" 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? (
                                list-ref nodes i
                              )
                            )
                             (
                              hash-table-ref (
                                list-ref nodes i
                              )
                               "left"
                            )
                          )
                           (
                            else (
                              list-ref (
                                list-ref nodes i
                              )
                               "left"
                            )
                          )
                        )
                      )
                    )
                     (
                      ret7 (
                        right_rotation i
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                rl_rotation i
              )
               (
                call/cc (
                  lambda (
                    ret8
                  )
                   (
                    begin (
                      hash-table-set! (
                        list-ref nodes i
                      )
                       "right" (
                        right_rotation (
                          cond (
                            (
                              string? (
                                list-ref nodes i
                              )
                            )
                             (
                              _substring (
                                list-ref nodes i
                              )
                               "right" (
                                + "right" 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? (
                                list-ref nodes i
                              )
                            )
                             (
                              hash-table-ref (
                                list-ref nodes i
                              )
                               "right"
                            )
                          )
                           (
                            else (
                              list-ref (
                                list-ref nodes i
                              )
                               "right"
                            )
                          )
                        )
                      )
                    )
                     (
                      ret8 (
                        left_rotation i
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                insert_node i value
              )
               (
                call/cc (
                  lambda (
                    ret9
                  )
                   (
                    begin (
                      if (
                        equal? i NIL
                      )
                       (
                        begin (
                          ret9 (
                            new_node value
                          )
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        < value (
                          cond (
                            (
                              string? (
                                list-ref nodes i
                              )
                            )
                             (
                              _substring (
                                list-ref nodes i
                              )
                               "data" (
                                + "data" 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? (
                                list-ref nodes i
                              )
                            )
                             (
                              hash-table-ref (
                                list-ref nodes i
                              )
                               "data"
                            )
                          )
                           (
                            else (
                              list-ref (
                                list-ref nodes i
                              )
                               "data"
                            )
                          )
                        )
                      )
                       (
                        begin (
                          hash-table-set! (
                            list-ref nodes i
                          )
                           "left" (
                            insert_node (
                              cond (
                                (
                                  string? (
                                    list-ref nodes i
                                  )
                                )
                                 (
                                  _substring (
                                    list-ref nodes i
                                  )
                                   "left" (
                                    + "left" 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? (
                                    list-ref nodes i
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    list-ref nodes i
                                  )
                                   "left"
                                )
                              )
                               (
                                else (
                                  list-ref (
                                    list-ref nodes i
                                  )
                                   "left"
                                )
                              )
                            )
                             value
                          )
                        )
                         (
                          if (
                            equal? (
                              - (
                                get_height (
                                  cond (
                                    (
                                      string? (
                                        list-ref nodes i
                                      )
                                    )
                                     (
                                      _substring (
                                        list-ref nodes i
                                      )
                                       "left" (
                                        + "left" 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        list-ref nodes i
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        list-ref nodes i
                                      )
                                       "left"
                                    )
                                  )
                                   (
                                    else (
                                      list-ref (
                                        list-ref nodes i
                                      )
                                       "left"
                                    )
                                  )
                                )
                              )
                               (
                                get_height (
                                  cond (
                                    (
                                      string? (
                                        list-ref nodes i
                                      )
                                    )
                                     (
                                      _substring (
                                        list-ref nodes i
                                      )
                                       "right" (
                                        + "right" 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        list-ref nodes i
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        list-ref nodes i
                                      )
                                       "right"
                                    )
                                  )
                                   (
                                    else (
                                      list-ref (
                                        list-ref nodes i
                                      )
                                       "right"
                                    )
                                  )
                                )
                              )
                            )
                             2
                          )
                           (
                            begin (
                              if (
                                < value (
                                  cond (
                                    (
                                      string? (
                                        list-ref nodes (
                                          cond (
                                            (
                                              string? (
                                                list-ref nodes i
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref nodes i
                                              )
                                               "left" (
                                                + "left" 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref nodes i
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref nodes i
                                              )
                                               "left"
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref nodes i
                                              )
                                               "left"
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      _substring (
                                        list-ref nodes (
                                          cond (
                                            (
                                              string? (
                                                list-ref nodes i
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref nodes i
                                              )
                                               "left" (
                                                + "left" 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref nodes i
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref nodes i
                                              )
                                               "left"
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref nodes i
                                              )
                                               "left"
                                            )
                                          )
                                        )
                                      )
                                       "data" (
                                        + "data" 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        list-ref nodes (
                                          cond (
                                            (
                                              string? (
                                                list-ref nodes i
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref nodes i
                                              )
                                               "left" (
                                                + "left" 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref nodes i
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref nodes i
                                              )
                                               "left"
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref nodes i
                                              )
                                               "left"
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        list-ref nodes (
                                          cond (
                                            (
                                              string? (
                                                list-ref nodes i
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref nodes i
                                              )
                                               "left" (
                                                + "left" 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref nodes i
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref nodes i
                                              )
                                               "left"
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref nodes i
                                              )
                                               "left"
                                            )
                                          )
                                        )
                                      )
                                       "data"
                                    )
                                  )
                                   (
                                    else (
                                      list-ref (
                                        list-ref nodes (
                                          cond (
                                            (
                                              string? (
                                                list-ref nodes i
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref nodes i
                                              )
                                               "left" (
                                                + "left" 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref nodes i
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref nodes i
                                              )
                                               "left"
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref nodes i
                                              )
                                               "left"
                                            )
                                          )
                                        )
                                      )
                                       "data"
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  set! i (
                                    right_rotation i
                                  )
                                )
                              )
                               (
                                begin (
                                  set! i (
                                    lr_rotation i
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
                      )
                       (
                        begin (
                          hash-table-set! (
                            list-ref nodes i
                          )
                           "right" (
                            insert_node (
                              cond (
                                (
                                  string? (
                                    list-ref nodes i
                                  )
                                )
                                 (
                                  _substring (
                                    list-ref nodes i
                                  )
                                   "right" (
                                    + "right" 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? (
                                    list-ref nodes i
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    list-ref nodes i
                                  )
                                   "right"
                                )
                              )
                               (
                                else (
                                  list-ref (
                                    list-ref nodes i
                                  )
                                   "right"
                                )
                              )
                            )
                             value
                          )
                        )
                         (
                          if (
                            equal? (
                              - (
                                get_height (
                                  cond (
                                    (
                                      string? (
                                        list-ref nodes i
                                      )
                                    )
                                     (
                                      _substring (
                                        list-ref nodes i
                                      )
                                       "right" (
                                        + "right" 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        list-ref nodes i
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        list-ref nodes i
                                      )
                                       "right"
                                    )
                                  )
                                   (
                                    else (
                                      list-ref (
                                        list-ref nodes i
                                      )
                                       "right"
                                    )
                                  )
                                )
                              )
                               (
                                get_height (
                                  cond (
                                    (
                                      string? (
                                        list-ref nodes i
                                      )
                                    )
                                     (
                                      _substring (
                                        list-ref nodes i
                                      )
                                       "left" (
                                        + "left" 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        list-ref nodes i
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        list-ref nodes i
                                      )
                                       "left"
                                    )
                                  )
                                   (
                                    else (
                                      list-ref (
                                        list-ref nodes i
                                      )
                                       "left"
                                    )
                                  )
                                )
                              )
                            )
                             2
                          )
                           (
                            begin (
                              if (
                                < value (
                                  cond (
                                    (
                                      string? (
                                        list-ref nodes (
                                          cond (
                                            (
                                              string? (
                                                list-ref nodes i
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref nodes i
                                              )
                                               "right" (
                                                + "right" 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref nodes i
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref nodes i
                                              )
                                               "right"
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref nodes i
                                              )
                                               "right"
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      _substring (
                                        list-ref nodes (
                                          cond (
                                            (
                                              string? (
                                                list-ref nodes i
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref nodes i
                                              )
                                               "right" (
                                                + "right" 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref nodes i
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref nodes i
                                              )
                                               "right"
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref nodes i
                                              )
                                               "right"
                                            )
                                          )
                                        )
                                      )
                                       "data" (
                                        + "data" 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        list-ref nodes (
                                          cond (
                                            (
                                              string? (
                                                list-ref nodes i
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref nodes i
                                              )
                                               "right" (
                                                + "right" 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref nodes i
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref nodes i
                                              )
                                               "right"
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref nodes i
                                              )
                                               "right"
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        list-ref nodes (
                                          cond (
                                            (
                                              string? (
                                                list-ref nodes i
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref nodes i
                                              )
                                               "right" (
                                                + "right" 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref nodes i
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref nodes i
                                              )
                                               "right"
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref nodes i
                                              )
                                               "right"
                                            )
                                          )
                                        )
                                      )
                                       "data"
                                    )
                                  )
                                   (
                                    else (
                                      list-ref (
                                        list-ref nodes (
                                          cond (
                                            (
                                              string? (
                                                list-ref nodes i
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref nodes i
                                              )
                                               "right" (
                                                + "right" 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref nodes i
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref nodes i
                                              )
                                               "right"
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref nodes i
                                              )
                                               "right"
                                            )
                                          )
                                        )
                                      )
                                       "data"
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  set! i (
                                    rl_rotation i
                                  )
                                )
                              )
                               (
                                begin (
                                  set! i (
                                    left_rotation i
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
                      )
                    )
                     (
                      update_height i
                    )
                     (
                      ret9 i
                    )
                  )
                )
              )
            )
             (
              define (
                get_left_most i
              )
               (
                call/cc (
                  lambda (
                    ret10
                  )
                   (
                    let (
                      (
                        cur i
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
                                      not (
                                        equal? (
                                          cond (
                                            (
                                              string? (
                                                list-ref nodes cur
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref nodes cur
                                              )
                                               "left" (
                                                + "left" 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref nodes cur
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref nodes cur
                                              )
                                               "left"
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref nodes cur
                                              )
                                               "left"
                                            )
                                          )
                                        )
                                         NIL
                                      )
                                    )
                                     (
                                      begin (
                                        set! cur (
                                          cond (
                                            (
                                              string? (
                                                list-ref nodes cur
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref nodes cur
                                              )
                                               "left" (
                                                + "left" 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref nodes cur
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref nodes cur
                                              )
                                               "left"
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref nodes cur
                                              )
                                               "left"
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
                        ret10 (
                          cond (
                            (
                              string? (
                                list-ref nodes cur
                              )
                            )
                             (
                              _substring (
                                list-ref nodes cur
                              )
                               "data" (
                                + "data" 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? (
                                list-ref nodes cur
                              )
                            )
                             (
                              hash-table-ref (
                                list-ref nodes cur
                              )
                               "data"
                            )
                          )
                           (
                            else (
                              list-ref (
                                list-ref nodes cur
                              )
                               "data"
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
                del_node i value
              )
               (
                call/cc (
                  lambda (
                    ret13
                  )
                   (
                    begin (
                      if (
                        equal? i NIL
                      )
                       (
                        begin (
                          ret13 NIL
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        < value (
                          cond (
                            (
                              string? (
                                list-ref nodes i
                              )
                            )
                             (
                              _substring (
                                list-ref nodes i
                              )
                               "data" (
                                + "data" 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? (
                                list-ref nodes i
                              )
                            )
                             (
                              hash-table-ref (
                                list-ref nodes i
                              )
                               "data"
                            )
                          )
                           (
                            else (
                              list-ref (
                                list-ref nodes i
                              )
                               "data"
                            )
                          )
                        )
                      )
                       (
                        begin (
                          hash-table-set! (
                            list-ref nodes i
                          )
                           "left" (
                            del_node (
                              cond (
                                (
                                  string? (
                                    list-ref nodes i
                                  )
                                )
                                 (
                                  _substring (
                                    list-ref nodes i
                                  )
                                   "left" (
                                    + "left" 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? (
                                    list-ref nodes i
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    list-ref nodes i
                                  )
                                   "left"
                                )
                              )
                               (
                                else (
                                  list-ref (
                                    list-ref nodes i
                                  )
                                   "left"
                                )
                              )
                            )
                             value
                          )
                        )
                      )
                       (
                        if (
                          > value (
                            cond (
                              (
                                string? (
                                  list-ref nodes i
                                )
                              )
                               (
                                _substring (
                                  list-ref nodes i
                                )
                                 "data" (
                                  + "data" 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? (
                                  list-ref nodes i
                                )
                              )
                               (
                                hash-table-ref (
                                  list-ref nodes i
                                )
                                 "data"
                              )
                            )
                             (
                              else (
                                list-ref (
                                  list-ref nodes i
                                )
                                 "data"
                              )
                            )
                          )
                        )
                         (
                          begin (
                            hash-table-set! (
                              list-ref nodes i
                            )
                             "right" (
                              del_node (
                                cond (
                                  (
                                    string? (
                                      list-ref nodes i
                                    )
                                  )
                                   (
                                    _substring (
                                      list-ref nodes i
                                    )
                                     "right" (
                                      + "right" 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? (
                                      list-ref nodes i
                                    )
                                  )
                                   (
                                    hash-table-ref (
                                      list-ref nodes i
                                    )
                                     "right"
                                  )
                                )
                                 (
                                  else (
                                    list-ref (
                                      list-ref nodes i
                                    )
                                     "right"
                                  )
                                )
                              )
                               value
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              and (
                                not (
                                  equal? (
                                    cond (
                                      (
                                        string? (
                                          list-ref nodes i
                                        )
                                      )
                                       (
                                        _substring (
                                          list-ref nodes i
                                        )
                                         "left" (
                                          + "left" 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? (
                                          list-ref nodes i
                                        )
                                      )
                                       (
                                        hash-table-ref (
                                          list-ref nodes i
                                        )
                                         "left"
                                      )
                                    )
                                     (
                                      else (
                                        list-ref (
                                          list-ref nodes i
                                        )
                                         "left"
                                      )
                                    )
                                  )
                                   NIL
                                )
                              )
                               (
                                not (
                                  equal? (
                                    cond (
                                      (
                                        string? (
                                          list-ref nodes i
                                        )
                                      )
                                       (
                                        _substring (
                                          list-ref nodes i
                                        )
                                         "right" (
                                          + "right" 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? (
                                          list-ref nodes i
                                        )
                                      )
                                       (
                                        hash-table-ref (
                                          list-ref nodes i
                                        )
                                         "right"
                                      )
                                    )
                                     (
                                      else (
                                        list-ref (
                                          list-ref nodes i
                                        )
                                         "right"
                                      )
                                    )
                                  )
                                   NIL
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    temp (
                                      get_left_most (
                                        cond (
                                          (
                                            string? (
                                              list-ref nodes i
                                            )
                                          )
                                           (
                                            _substring (
                                              list-ref nodes i
                                            )
                                             "right" (
                                              + "right" 1
                                            )
                                          )
                                        )
                                         (
                                          (
                                            hash-table? (
                                              list-ref nodes i
                                            )
                                          )
                                           (
                                            hash-table-ref (
                                              list-ref nodes i
                                            )
                                             "right"
                                          )
                                        )
                                         (
                                          else (
                                            list-ref (
                                              list-ref nodes i
                                            )
                                             "right"
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    hash-table-set! (
                                      list-ref nodes i
                                    )
                                     "data" temp
                                  )
                                   (
                                    hash-table-set! (
                                      list-ref nodes i
                                    )
                                     "right" (
                                      del_node (
                                        cond (
                                          (
                                            string? (
                                              list-ref nodes i
                                            )
                                          )
                                           (
                                            _substring (
                                              list-ref nodes i
                                            )
                                             "right" (
                                              + "right" 1
                                            )
                                          )
                                        )
                                         (
                                          (
                                            hash-table? (
                                              list-ref nodes i
                                            )
                                          )
                                           (
                                            hash-table-ref (
                                              list-ref nodes i
                                            )
                                             "right"
                                          )
                                        )
                                         (
                                          else (
                                            list-ref (
                                              list-ref nodes i
                                            )
                                             "right"
                                          )
                                        )
                                      )
                                       temp
                                    )
                                  )
                                )
                              )
                            )
                             (
                              if (
                                not (
                                  equal? (
                                    cond (
                                      (
                                        string? (
                                          list-ref nodes i
                                        )
                                      )
                                       (
                                        _substring (
                                          list-ref nodes i
                                        )
                                         "left" (
                                          + "left" 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? (
                                          list-ref nodes i
                                        )
                                      )
                                       (
                                        hash-table-ref (
                                          list-ref nodes i
                                        )
                                         "left"
                                      )
                                    )
                                     (
                                      else (
                                        list-ref (
                                          list-ref nodes i
                                        )
                                         "left"
                                      )
                                    )
                                  )
                                   NIL
                                )
                              )
                               (
                                begin (
                                  set! i (
                                    cond (
                                      (
                                        string? (
                                          list-ref nodes i
                                        )
                                      )
                                       (
                                        _substring (
                                          list-ref nodes i
                                        )
                                         "left" (
                                          + "left" 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? (
                                          list-ref nodes i
                                        )
                                      )
                                       (
                                        hash-table-ref (
                                          list-ref nodes i
                                        )
                                         "left"
                                      )
                                    )
                                     (
                                      else (
                                        list-ref (
                                          list-ref nodes i
                                        )
                                         "left"
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  set! i (
                                    cond (
                                      (
                                        string? (
                                          list-ref nodes i
                                        )
                                      )
                                       (
                                        _substring (
                                          list-ref nodes i
                                        )
                                         "right" (
                                          + "right" 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? (
                                          list-ref nodes i
                                        )
                                      )
                                       (
                                        hash-table-ref (
                                          list-ref nodes i
                                        )
                                         "right"
                                      )
                                    )
                                     (
                                      else (
                                        list-ref (
                                          list-ref nodes i
                                        )
                                         "right"
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
                      if (
                        equal? i NIL
                      )
                       (
                        begin (
                          ret13 NIL
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
                          lh (
                            get_height (
                              cond (
                                (
                                  string? (
                                    list-ref nodes i
                                  )
                                )
                                 (
                                  _substring (
                                    list-ref nodes i
                                  )
                                   "left" (
                                    + "left" 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? (
                                    list-ref nodes i
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    list-ref nodes i
                                  )
                                   "left"
                                )
                              )
                               (
                                else (
                                  list-ref (
                                    list-ref nodes i
                                  )
                                   "left"
                                )
                              )
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              rh (
                                get_height (
                                  cond (
                                    (
                                      string? (
                                        list-ref nodes i
                                      )
                                    )
                                     (
                                      _substring (
                                        list-ref nodes i
                                      )
                                       "right" (
                                        + "right" 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        list-ref nodes i
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        list-ref nodes i
                                      )
                                       "right"
                                    )
                                  )
                                   (
                                    else (
                                      list-ref (
                                        list-ref nodes i
                                      )
                                       "right"
                                    )
                                  )
                                )
                              )
                            )
                          )
                           (
                            begin (
                              if (
                                equal? (
                                  - rh lh
                                )
                                 2
                              )
                               (
                                begin (
                                  if (
                                    _gt (
                                      get_height (
                                        cond (
                                          (
                                            string? (
                                              list-ref nodes (
                                                cond (
                                                  (
                                                    string? (
                                                      list-ref nodes i
                                                    )
                                                  )
                                                   (
                                                    _substring (
                                                      list-ref nodes i
                                                    )
                                                     "right" (
                                                      + "right" 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? (
                                                      list-ref nodes i
                                                    )
                                                  )
                                                   (
                                                    hash-table-ref (
                                                      list-ref nodes i
                                                    )
                                                     "right"
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref (
                                                      list-ref nodes i
                                                    )
                                                     "right"
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            _substring (
                                              list-ref nodes (
                                                cond (
                                                  (
                                                    string? (
                                                      list-ref nodes i
                                                    )
                                                  )
                                                   (
                                                    _substring (
                                                      list-ref nodes i
                                                    )
                                                     "right" (
                                                      + "right" 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? (
                                                      list-ref nodes i
                                                    )
                                                  )
                                                   (
                                                    hash-table-ref (
                                                      list-ref nodes i
                                                    )
                                                     "right"
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref (
                                                      list-ref nodes i
                                                    )
                                                     "right"
                                                  )
                                                )
                                              )
                                            )
                                             "right" (
                                              + "right" 1
                                            )
                                          )
                                        )
                                         (
                                          (
                                            hash-table? (
                                              list-ref nodes (
                                                cond (
                                                  (
                                                    string? (
                                                      list-ref nodes i
                                                    )
                                                  )
                                                   (
                                                    _substring (
                                                      list-ref nodes i
                                                    )
                                                     "right" (
                                                      + "right" 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? (
                                                      list-ref nodes i
                                                    )
                                                  )
                                                   (
                                                    hash-table-ref (
                                                      list-ref nodes i
                                                    )
                                                     "right"
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref (
                                                      list-ref nodes i
                                                    )
                                                     "right"
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            hash-table-ref (
                                              list-ref nodes (
                                                cond (
                                                  (
                                                    string? (
                                                      list-ref nodes i
                                                    )
                                                  )
                                                   (
                                                    _substring (
                                                      list-ref nodes i
                                                    )
                                                     "right" (
                                                      + "right" 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? (
                                                      list-ref nodes i
                                                    )
                                                  )
                                                   (
                                                    hash-table-ref (
                                                      list-ref nodes i
                                                    )
                                                     "right"
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref (
                                                      list-ref nodes i
                                                    )
                                                     "right"
                                                  )
                                                )
                                              )
                                            )
                                             "right"
                                          )
                                        )
                                         (
                                          else (
                                            list-ref (
                                              list-ref nodes (
                                                cond (
                                                  (
                                                    string? (
                                                      list-ref nodes i
                                                    )
                                                  )
                                                   (
                                                    _substring (
                                                      list-ref nodes i
                                                    )
                                                     "right" (
                                                      + "right" 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? (
                                                      list-ref nodes i
                                                    )
                                                  )
                                                   (
                                                    hash-table-ref (
                                                      list-ref nodes i
                                                    )
                                                     "right"
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref (
                                                      list-ref nodes i
                                                    )
                                                     "right"
                                                  )
                                                )
                                              )
                                            )
                                             "right"
                                          )
                                        )
                                      )
                                    )
                                     (
                                      get_height (
                                        cond (
                                          (
                                            string? (
                                              list-ref nodes (
                                                cond (
                                                  (
                                                    string? (
                                                      list-ref nodes i
                                                    )
                                                  )
                                                   (
                                                    _substring (
                                                      list-ref nodes i
                                                    )
                                                     "right" (
                                                      + "right" 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? (
                                                      list-ref nodes i
                                                    )
                                                  )
                                                   (
                                                    hash-table-ref (
                                                      list-ref nodes i
                                                    )
                                                     "right"
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref (
                                                      list-ref nodes i
                                                    )
                                                     "right"
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            _substring (
                                              list-ref nodes (
                                                cond (
                                                  (
                                                    string? (
                                                      list-ref nodes i
                                                    )
                                                  )
                                                   (
                                                    _substring (
                                                      list-ref nodes i
                                                    )
                                                     "right" (
                                                      + "right" 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? (
                                                      list-ref nodes i
                                                    )
                                                  )
                                                   (
                                                    hash-table-ref (
                                                      list-ref nodes i
                                                    )
                                                     "right"
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref (
                                                      list-ref nodes i
                                                    )
                                                     "right"
                                                  )
                                                )
                                              )
                                            )
                                             "left" (
                                              + "left" 1
                                            )
                                          )
                                        )
                                         (
                                          (
                                            hash-table? (
                                              list-ref nodes (
                                                cond (
                                                  (
                                                    string? (
                                                      list-ref nodes i
                                                    )
                                                  )
                                                   (
                                                    _substring (
                                                      list-ref nodes i
                                                    )
                                                     "right" (
                                                      + "right" 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? (
                                                      list-ref nodes i
                                                    )
                                                  )
                                                   (
                                                    hash-table-ref (
                                                      list-ref nodes i
                                                    )
                                                     "right"
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref (
                                                      list-ref nodes i
                                                    )
                                                     "right"
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            hash-table-ref (
                                              list-ref nodes (
                                                cond (
                                                  (
                                                    string? (
                                                      list-ref nodes i
                                                    )
                                                  )
                                                   (
                                                    _substring (
                                                      list-ref nodes i
                                                    )
                                                     "right" (
                                                      + "right" 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? (
                                                      list-ref nodes i
                                                    )
                                                  )
                                                   (
                                                    hash-table-ref (
                                                      list-ref nodes i
                                                    )
                                                     "right"
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref (
                                                      list-ref nodes i
                                                    )
                                                     "right"
                                                  )
                                                )
                                              )
                                            )
                                             "left"
                                          )
                                        )
                                         (
                                          else (
                                            list-ref (
                                              list-ref nodes (
                                                cond (
                                                  (
                                                    string? (
                                                      list-ref nodes i
                                                    )
                                                  )
                                                   (
                                                    _substring (
                                                      list-ref nodes i
                                                    )
                                                     "right" (
                                                      + "right" 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? (
                                                      list-ref nodes i
                                                    )
                                                  )
                                                   (
                                                    hash-table-ref (
                                                      list-ref nodes i
                                                    )
                                                     "right"
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref (
                                                      list-ref nodes i
                                                    )
                                                     "right"
                                                  )
                                                )
                                              )
                                            )
                                             "left"
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      set! i (
                                        left_rotation i
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      set! i (
                                        rl_rotation i
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                if (
                                  equal? (
                                    - lh rh
                                  )
                                   2
                                )
                                 (
                                  begin (
                                    if (
                                      _gt (
                                        get_height (
                                          cond (
                                            (
                                              string? (
                                                list-ref nodes (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref nodes i
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref nodes i
                                                      )
                                                       "left" (
                                                        + "left" 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref nodes i
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref nodes i
                                                      )
                                                       "left"
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref (
                                                        list-ref nodes i
                                                      )
                                                       "left"
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref nodes (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref nodes i
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref nodes i
                                                      )
                                                       "left" (
                                                        + "left" 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref nodes i
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref nodes i
                                                      )
                                                       "left"
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref (
                                                        list-ref nodes i
                                                      )
                                                       "left"
                                                    )
                                                  )
                                                )
                                              )
                                               "left" (
                                                + "left" 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref nodes (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref nodes i
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref nodes i
                                                      )
                                                       "left" (
                                                        + "left" 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref nodes i
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref nodes i
                                                      )
                                                       "left"
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref (
                                                        list-ref nodes i
                                                      )
                                                       "left"
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref nodes (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref nodes i
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref nodes i
                                                      )
                                                       "left" (
                                                        + "left" 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref nodes i
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref nodes i
                                                      )
                                                       "left"
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref (
                                                        list-ref nodes i
                                                      )
                                                       "left"
                                                    )
                                                  )
                                                )
                                              )
                                               "left"
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref nodes (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref nodes i
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref nodes i
                                                      )
                                                       "left" (
                                                        + "left" 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref nodes i
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref nodes i
                                                      )
                                                       "left"
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref (
                                                        list-ref nodes i
                                                      )
                                                       "left"
                                                    )
                                                  )
                                                )
                                              )
                                               "left"
                                            )
                                          )
                                        )
                                      )
                                       (
                                        get_height (
                                          cond (
                                            (
                                              string? (
                                                list-ref nodes (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref nodes i
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref nodes i
                                                      )
                                                       "left" (
                                                        + "left" 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref nodes i
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref nodes i
                                                      )
                                                       "left"
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref (
                                                        list-ref nodes i
                                                      )
                                                       "left"
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref nodes (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref nodes i
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref nodes i
                                                      )
                                                       "left" (
                                                        + "left" 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref nodes i
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref nodes i
                                                      )
                                                       "left"
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref (
                                                        list-ref nodes i
                                                      )
                                                       "left"
                                                    )
                                                  )
                                                )
                                              )
                                               "right" (
                                                + "right" 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref nodes (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref nodes i
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref nodes i
                                                      )
                                                       "left" (
                                                        + "left" 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref nodes i
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref nodes i
                                                      )
                                                       "left"
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref (
                                                        list-ref nodes i
                                                      )
                                                       "left"
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref nodes (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref nodes i
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref nodes i
                                                      )
                                                       "left" (
                                                        + "left" 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref nodes i
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref nodes i
                                                      )
                                                       "left"
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref (
                                                        list-ref nodes i
                                                      )
                                                       "left"
                                                    )
                                                  )
                                                )
                                              )
                                               "right"
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref nodes (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref nodes i
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref nodes i
                                                      )
                                                       "left" (
                                                        + "left" 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref nodes i
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref nodes i
                                                      )
                                                       "left"
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref (
                                                        list-ref nodes i
                                                      )
                                                       "left"
                                                    )
                                                  )
                                                )
                                              )
                                               "right"
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! i (
                                          right_rotation i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! i (
                                          lr_rotation i
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
                            )
                             (
                              update_height i
                            )
                             (
                              ret13 i
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
                inorder i
              )
               (
                call/cc (
                  lambda (
                    ret14
                  )
                   (
                    begin (
                      if (
                        equal? i NIL
                      )
                       (
                        begin (
                          ret14 ""
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
                            inorder (
                              cond (
                                (
                                  string? (
                                    list-ref nodes i
                                  )
                                )
                                 (
                                  _substring (
                                    list-ref nodes i
                                  )
                                   "left" (
                                    + "left" 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? (
                                    list-ref nodes i
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    list-ref nodes i
                                  )
                                   "left"
                                )
                              )
                               (
                                else (
                                  list-ref (
                                    list-ref nodes i
                                  )
                                   "left"
                                )
                              )
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              right (
                                inorder (
                                  cond (
                                    (
                                      string? (
                                        list-ref nodes i
                                      )
                                    )
                                     (
                                      _substring (
                                        list-ref nodes i
                                      )
                                       "right" (
                                        + "right" 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        list-ref nodes i
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        list-ref nodes i
                                      )
                                       "right"
                                    )
                                  )
                                   (
                                    else (
                                      list-ref (
                                        list-ref nodes i
                                      )
                                       "right"
                                    )
                                  )
                                )
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  res (
                                    to-str-space (
                                      cond (
                                        (
                                          string? (
                                            list-ref nodes i
                                          )
                                        )
                                         (
                                          _substring (
                                            list-ref nodes i
                                          )
                                           "data" (
                                            + "data" 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? (
                                            list-ref nodes i
                                          )
                                        )
                                         (
                                          hash-table-ref (
                                            list-ref nodes i
                                          )
                                           "data"
                                        )
                                      )
                                       (
                                        else (
                                          list-ref (
                                            list-ref nodes i
                                          )
                                           "data"
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  if (
                                    not (
                                      string=? left ""
                                    )
                                  )
                                   (
                                    begin (
                                      set! res (
                                        string-append (
                                          string-append left " "
                                        )
                                         res
                                      )
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
                                      string=? right ""
                                    )
                                  )
                                   (
                                    begin (
                                      set! res (
                                        string-append (
                                          string-append res " "
                                        )
                                         right
                                      )
                                    )
                                  )
                                   (
                                    quote (
                                      
                                    )
                                  )
                                )
                                 (
                                  ret14 res
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
                      set! nodes (
                        _list
                      )
                    )
                     (
                      let (
                        (
                          root NIL
                        )
                      )
                       (
                        begin (
                          set! root (
                            insert_node root 4
                          )
                        )
                         (
                          set! root (
                            insert_node root 2
                          )
                        )
                         (
                          set! root (
                            insert_node root 3
                          )
                        )
                         (
                          _display (
                            if (
                              string? (
                                inorder root
                              )
                            )
                             (
                              inorder root
                            )
                             (
                              to-str (
                                inorder root
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
                                  get_height root
                                )
                              )
                            )
                             (
                              to-str-space (
                                get_height root
                              )
                            )
                             (
                              to-str (
                                to-str-space (
                                  get_height root
                                )
                              )
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          set! root (
                            del_node root 3
                          )
                        )
                         (
                          _display (
                            if (
                              string? (
                                inorder root
                              )
                            )
                             (
                              inorder root
                            )
                             (
                              to-str (
                                inorder root
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
