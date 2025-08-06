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
      start14 (
        current-jiffy
      )
    )
     (
      jps17 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          LABEL 0
        )
      )
       (
        begin (
          let (
            (
              COLOR 1
            )
          )
           (
            begin (
              let (
                (
                  PARENT 2
                )
              )
               (
                begin (
                  let (
                    (
                      LEFT 3
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          RIGHT 4
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              NEG_ONE (
                                - 1
                              )
                            )
                          )
                           (
                            begin (
                              define (
                                make_tree
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
                                          cons "nodes" (
                                            _list
                                          )
                                        )
                                         (
                                          cons "root" (
                                            - 1
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
                                rotate_left t x
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret2
                                  )
                                   (
                                    let (
                                      (
                                        nodes (
                                          hash-table-ref t "nodes"
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            y (
                                              cond (
                                                (
                                                  string? (
                                                    list-ref nodes x
                                                  )
                                                )
                                                 (
                                                  _substring (
                                                    list-ref nodes x
                                                  )
                                                   RIGHT (
                                                    + RIGHT 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? (
                                                    list-ref nodes x
                                                  )
                                                )
                                                 (
                                                  hash-table-ref (
                                                    list-ref nodes x
                                                  )
                                                   RIGHT
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref (
                                                    list-ref nodes x
                                                  )
                                                   RIGHT
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                yLeft (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref nodes y
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref nodes y
                                                      )
                                                       LEFT (
                                                        + LEFT 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref nodes y
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref nodes y
                                                      )
                                                       LEFT
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref (
                                                        list-ref nodes y
                                                      )
                                                       LEFT
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                list-set! (
                                                  list-ref nodes x
                                                )
                                                 RIGHT yLeft
                                              )
                                               (
                                                if (
                                                  not (
                                                    equal? yLeft NEG_ONE
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    list-set! (
                                                      list-ref nodes yLeft
                                                    )
                                                     PARENT x
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
                                                    xParent (
                                                      cond (
                                                        (
                                                          string? (
                                                            list-ref nodes x
                                                          )
                                                        )
                                                         (
                                                          _substring (
                                                            list-ref nodes x
                                                          )
                                                           PARENT (
                                                            + PARENT 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? (
                                                            list-ref nodes x
                                                          )
                                                        )
                                                         (
                                                          hash-table-ref (
                                                            list-ref nodes x
                                                          )
                                                           PARENT
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref (
                                                            list-ref nodes x
                                                          )
                                                           PARENT
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    list-set! (
                                                      list-ref nodes y
                                                    )
                                                     PARENT xParent
                                                  )
                                                   (
                                                    if (
                                                      equal? xParent NEG_ONE
                                                    )
                                                     (
                                                      begin (
                                                        hash-table-set! t "root" y
                                                      )
                                                    )
                                                     (
                                                      if (
                                                        equal? x (
                                                          cond (
                                                            (
                                                              string? (
                                                                list-ref nodes xParent
                                                              )
                                                            )
                                                             (
                                                              _substring (
                                                                list-ref nodes xParent
                                                              )
                                                               LEFT (
                                                                + LEFT 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? (
                                                                list-ref nodes xParent
                                                              )
                                                            )
                                                             (
                                                              hash-table-ref (
                                                                list-ref nodes xParent
                                                              )
                                                               LEFT
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref (
                                                                list-ref nodes xParent
                                                              )
                                                               LEFT
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          list-set! (
                                                            list-ref nodes xParent
                                                          )
                                                           LEFT y
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          list-set! (
                                                            list-ref nodes xParent
                                                          )
                                                           RIGHT y
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    list-set! (
                                                      list-ref nodes y
                                                    )
                                                     LEFT x
                                                  )
                                                   (
                                                    list-set! (
                                                      list-ref nodes x
                                                    )
                                                     PARENT y
                                                  )
                                                   (
                                                    hash-table-set! t "nodes" nodes
                                                  )
                                                   (
                                                    ret2 t
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
                                rotate_right t x
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret3
                                  )
                                   (
                                    let (
                                      (
                                        nodes (
                                          hash-table-ref t "nodes"
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            y (
                                              cond (
                                                (
                                                  string? (
                                                    list-ref nodes x
                                                  )
                                                )
                                                 (
                                                  _substring (
                                                    list-ref nodes x
                                                  )
                                                   LEFT (
                                                    + LEFT 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? (
                                                    list-ref nodes x
                                                  )
                                                )
                                                 (
                                                  hash-table-ref (
                                                    list-ref nodes x
                                                  )
                                                   LEFT
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref (
                                                    list-ref nodes x
                                                  )
                                                   LEFT
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                yRight (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref nodes y
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref nodes y
                                                      )
                                                       RIGHT (
                                                        + RIGHT 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref nodes y
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref nodes y
                                                      )
                                                       RIGHT
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref (
                                                        list-ref nodes y
                                                      )
                                                       RIGHT
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                list-set! (
                                                  list-ref nodes x
                                                )
                                                 LEFT yRight
                                              )
                                               (
                                                if (
                                                  not (
                                                    equal? yRight NEG_ONE
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    list-set! (
                                                      list-ref nodes yRight
                                                    )
                                                     PARENT x
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
                                                    xParent (
                                                      cond (
                                                        (
                                                          string? (
                                                            list-ref nodes x
                                                          )
                                                        )
                                                         (
                                                          _substring (
                                                            list-ref nodes x
                                                          )
                                                           PARENT (
                                                            + PARENT 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? (
                                                            list-ref nodes x
                                                          )
                                                        )
                                                         (
                                                          hash-table-ref (
                                                            list-ref nodes x
                                                          )
                                                           PARENT
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref (
                                                            list-ref nodes x
                                                          )
                                                           PARENT
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    list-set! (
                                                      list-ref nodes y
                                                    )
                                                     PARENT xParent
                                                  )
                                                   (
                                                    if (
                                                      equal? xParent NEG_ONE
                                                    )
                                                     (
                                                      begin (
                                                        hash-table-set! t "root" y
                                                      )
                                                    )
                                                     (
                                                      if (
                                                        equal? x (
                                                          cond (
                                                            (
                                                              string? (
                                                                list-ref nodes xParent
                                                              )
                                                            )
                                                             (
                                                              _substring (
                                                                list-ref nodes xParent
                                                              )
                                                               RIGHT (
                                                                + RIGHT 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? (
                                                                list-ref nodes xParent
                                                              )
                                                            )
                                                             (
                                                              hash-table-ref (
                                                                list-ref nodes xParent
                                                              )
                                                               RIGHT
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref (
                                                                list-ref nodes xParent
                                                              )
                                                               RIGHT
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          list-set! (
                                                            list-ref nodes xParent
                                                          )
                                                           RIGHT y
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          list-set! (
                                                            list-ref nodes xParent
                                                          )
                                                           LEFT y
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    list-set! (
                                                      list-ref nodes y
                                                    )
                                                     RIGHT x
                                                  )
                                                   (
                                                    list-set! (
                                                      list-ref nodes x
                                                    )
                                                     PARENT y
                                                  )
                                                   (
                                                    hash-table-set! t "nodes" nodes
                                                  )
                                                   (
                                                    ret3 t
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
                                insert_fix t z
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret4
                                  )
                                   (
                                    let (
                                      (
                                        nodes (
                                          hash-table-ref t "nodes"
                                        )
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
                                                      and (
                                                        not (
                                                          equal? z (
                                                            hash-table-ref t "root"
                                                          )
                                                        )
                                                      )
                                                       (
                                                        equal? (
                                                          cond (
                                                            (
                                                              string? (
                                                                list-ref nodes (
                                                                  cond (
                                                                    (
                                                                      string? (
                                                                        list-ref nodes z
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref nodes z
                                                                      )
                                                                       PARENT (
                                                                        + PARENT 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        list-ref nodes z
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref nodes z
                                                                      )
                                                                       PARENT
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref (
                                                                        list-ref nodes z
                                                                      )
                                                                       PARENT
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
                                                                        list-ref nodes z
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref nodes z
                                                                      )
                                                                       PARENT (
                                                                        + PARENT 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        list-ref nodes z
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref nodes z
                                                                      )
                                                                       PARENT
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref (
                                                                        list-ref nodes z
                                                                      )
                                                                       PARENT
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               COLOR (
                                                                + COLOR 1
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
                                                                        list-ref nodes z
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref nodes z
                                                                      )
                                                                       PARENT (
                                                                        + PARENT 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        list-ref nodes z
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref nodes z
                                                                      )
                                                                       PARENT
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref (
                                                                        list-ref nodes z
                                                                      )
                                                                       PARENT
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
                                                                        list-ref nodes z
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref nodes z
                                                                      )
                                                                       PARENT (
                                                                        + PARENT 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        list-ref nodes z
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref nodes z
                                                                      )
                                                                       PARENT
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref (
                                                                        list-ref nodes z
                                                                      )
                                                                       PARENT
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               COLOR
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref (
                                                                list-ref nodes (
                                                                  cond (
                                                                    (
                                                                      string? (
                                                                        list-ref nodes z
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref nodes z
                                                                      )
                                                                       PARENT (
                                                                        + PARENT 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        list-ref nodes z
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref nodes z
                                                                      )
                                                                       PARENT
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref (
                                                                        list-ref nodes z
                                                                      )
                                                                       PARENT
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               COLOR
                                                            )
                                                          )
                                                        )
                                                         1
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          equal? (
                                                            cond (
                                                              (
                                                                string? (
                                                                  list-ref nodes z
                                                                )
                                                              )
                                                               (
                                                                _substring (
                                                                  list-ref nodes z
                                                                )
                                                                 PARENT (
                                                                  + PARENT 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? (
                                                                  list-ref nodes z
                                                                )
                                                              )
                                                               (
                                                                hash-table-ref (
                                                                  list-ref nodes z
                                                                )
                                                                 PARENT
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref (
                                                                  list-ref nodes z
                                                                )
                                                                 PARENT
                                                              )
                                                            )
                                                          )
                                                           (
                                                            cond (
                                                              (
                                                                string? (
                                                                  list-ref nodes (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref nodes (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
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
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         PARENT (
                                                                          + PARENT 1
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
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
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
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         PARENT
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          list-ref nodes (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         PARENT
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
                                                                          list-ref nodes (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
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
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         PARENT (
                                                                          + PARENT 1
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
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
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
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         PARENT
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          list-ref nodes (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         PARENT
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 LEFT (
                                                                  + LEFT 1
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
                                                                          list-ref nodes (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
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
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         PARENT (
                                                                          + PARENT 1
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
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
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
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         PARENT
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          list-ref nodes (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         PARENT
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
                                                                          list-ref nodes (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
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
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         PARENT (
                                                                          + PARENT 1
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
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
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
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         PARENT
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          list-ref nodes (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         PARENT
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 LEFT
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref (
                                                                  list-ref nodes (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref nodes (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
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
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         PARENT (
                                                                          + PARENT 1
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
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
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
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         PARENT
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          list-ref nodes (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT (
                                                                                  + PARENT 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes z
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes z
                                                                                )
                                                                                 PARENT
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         PARENT
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 LEFT
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                y (
                                                                  cond (
                                                                    (
                                                                      string? (
                                                                        list-ref nodes (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT (
                                                                                + PARENT 1
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
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
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT (
                                                                                + PARENT 1
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       RIGHT (
                                                                        + RIGHT 1
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
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT (
                                                                                + PARENT 1
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
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
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT (
                                                                                + PARENT 1
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       RIGHT
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref (
                                                                        list-ref nodes (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT (
                                                                                + PARENT 1
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       RIGHT
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  and (
                                                                    not (
                                                                      equal? y NEG_ONE
                                                                    )
                                                                  )
                                                                   (
                                                                    equal? (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            list-ref nodes y
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref nodes y
                                                                          )
                                                                           COLOR (
                                                                            + COLOR 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref nodes y
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref nodes y
                                                                          )
                                                                           COLOR
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            list-ref nodes y
                                                                          )
                                                                           COLOR
                                                                        )
                                                                      )
                                                                    )
                                                                     1
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    list-set! (
                                                                      list-ref nodes (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref nodes z
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref nodes z
                                                                            )
                                                                             PARENT (
                                                                              + PARENT 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref nodes z
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref nodes z
                                                                            )
                                                                             PARENT
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref nodes z
                                                                            )
                                                                             PARENT
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     COLOR 0
                                                                  )
                                                                   (
                                                                    list-set! (
                                                                      list-ref nodes y
                                                                    )
                                                                     COLOR 0
                                                                  )
                                                                   (
                                                                    let (
                                                                      (
                                                                        gp (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT (
                                                                                + PARENT 1
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        list-set! (
                                                                          list-ref nodes gp
                                                                        )
                                                                         COLOR 1
                                                                      )
                                                                       (
                                                                        set! z gp
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      equal? z (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref nodes (
                                                                                cond (
                                                                                  (
                                                                                    string? (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT (
                                                                                      + PARENT 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT
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
                                                                                      list-ref nodes z
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT (
                                                                                      + PARENT 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             RIGHT (
                                                                              + RIGHT 1
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
                                                                                      list-ref nodes z
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT (
                                                                                      + PARENT 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT
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
                                                                                      list-ref nodes z
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT (
                                                                                      + PARENT 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             RIGHT
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref nodes (
                                                                                cond (
                                                                                  (
                                                                                    string? (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT (
                                                                                      + PARENT 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             RIGHT
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! z (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref nodes z
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref nodes z
                                                                              )
                                                                               PARENT (
                                                                                + PARENT 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref nodes z
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref nodes z
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref nodes z
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-set! t "nodes" nodes
                                                                      )
                                                                       (
                                                                        set! t (
                                                                          rotate_left t z
                                                                        )
                                                                      )
                                                                       (
                                                                        set! nodes (
                                                                          hash-table-ref t "nodes"
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      quote (
                                                                        
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    list-set! (
                                                                      list-ref nodes (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref nodes z
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref nodes z
                                                                            )
                                                                             PARENT (
                                                                              + PARENT 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref nodes z
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref nodes z
                                                                            )
                                                                             PARENT
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref nodes z
                                                                            )
                                                                             PARENT
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     COLOR 0
                                                                  )
                                                                   (
                                                                    let (
                                                                      (
                                                                        gp (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT (
                                                                                + PARENT 1
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        list-set! (
                                                                          list-ref nodes gp
                                                                        )
                                                                         COLOR 1
                                                                      )
                                                                       (
                                                                        hash-table-set! t "nodes" nodes
                                                                      )
                                                                       (
                                                                        set! t (
                                                                          rotate_right t gp
                                                                        )
                                                                      )
                                                                       (
                                                                        set! nodes (
                                                                          hash-table-ref t "nodes"
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
                                                          begin (
                                                            let (
                                                              (
                                                                y (
                                                                  cond (
                                                                    (
                                                                      string? (
                                                                        list-ref nodes (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT (
                                                                                + PARENT 1
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
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
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT (
                                                                                + PARENT 1
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       LEFT (
                                                                        + LEFT 1
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
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT (
                                                                                + PARENT 1
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
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
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT (
                                                                                + PARENT 1
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       LEFT
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref (
                                                                        list-ref nodes (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT (
                                                                                + PARENT 1
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       LEFT
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  and (
                                                                    not (
                                                                      equal? y NEG_ONE
                                                                    )
                                                                  )
                                                                   (
                                                                    equal? (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            list-ref nodes y
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref nodes y
                                                                          )
                                                                           COLOR (
                                                                            + COLOR 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref nodes y
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref nodes y
                                                                          )
                                                                           COLOR
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            list-ref nodes y
                                                                          )
                                                                           COLOR
                                                                        )
                                                                      )
                                                                    )
                                                                     1
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    list-set! (
                                                                      list-ref nodes (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref nodes z
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref nodes z
                                                                            )
                                                                             PARENT (
                                                                              + PARENT 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref nodes z
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref nodes z
                                                                            )
                                                                             PARENT
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref nodes z
                                                                            )
                                                                             PARENT
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     COLOR 0
                                                                  )
                                                                   (
                                                                    list-set! (
                                                                      list-ref nodes y
                                                                    )
                                                                     COLOR 0
                                                                  )
                                                                   (
                                                                    let (
                                                                      (
                                                                        gp (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT (
                                                                                + PARENT 1
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        list-set! (
                                                                          list-ref nodes gp
                                                                        )
                                                                         COLOR 1
                                                                      )
                                                                       (
                                                                        set! z gp
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      equal? z (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref nodes (
                                                                                cond (
                                                                                  (
                                                                                    string? (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT (
                                                                                      + PARENT 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT
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
                                                                                      list-ref nodes z
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT (
                                                                                      + PARENT 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             LEFT (
                                                                              + LEFT 1
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
                                                                                      list-ref nodes z
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT (
                                                                                      + PARENT 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT
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
                                                                                      list-ref nodes z
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT (
                                                                                      + PARENT 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             LEFT
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref nodes (
                                                                                cond (
                                                                                  (
                                                                                    string? (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT (
                                                                                      + PARENT 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref (
                                                                                      list-ref nodes z
                                                                                    )
                                                                                     PARENT
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             LEFT
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! z (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref nodes z
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref nodes z
                                                                              )
                                                                               PARENT (
                                                                                + PARENT 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref nodes z
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref nodes z
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref nodes z
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-set! t "nodes" nodes
                                                                      )
                                                                       (
                                                                        set! t (
                                                                          rotate_right t z
                                                                        )
                                                                      )
                                                                       (
                                                                        set! nodes (
                                                                          hash-table-ref t "nodes"
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      quote (
                                                                        
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    list-set! (
                                                                      list-ref nodes (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref nodes z
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref nodes z
                                                                            )
                                                                             PARENT (
                                                                              + PARENT 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref nodes z
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref nodes z
                                                                            )
                                                                             PARENT
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref nodes z
                                                                            )
                                                                             PARENT
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     COLOR 0
                                                                  )
                                                                   (
                                                                    let (
                                                                      (
                                                                        gp (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT (
                                                                                + PARENT 1
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
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
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref nodes (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT (
                                                                                        + PARENT 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref nodes z
                                                                                      )
                                                                                       PARENT
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               PARENT
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        list-set! (
                                                                          list-ref nodes gp
                                                                        )
                                                                         COLOR 1
                                                                      )
                                                                       (
                                                                        hash-table-set! t "nodes" nodes
                                                                      )
                                                                       (
                                                                        set! t (
                                                                          rotate_left t gp
                                                                        )
                                                                      )
                                                                       (
                                                                        set! nodes (
                                                                          hash-table-ref t "nodes"
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
                                        set! nodes (
                                          hash-table-ref t "nodes"
                                        )
                                      )
                                       (
                                        list-set! (
                                          list-ref nodes (
                                            hash-table-ref t "root"
                                          )
                                        )
                                         COLOR 0
                                      )
                                       (
                                        hash-table-set! t "nodes" nodes
                                      )
                                       (
                                        ret4 t
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              define (
                                tree_insert t v
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret7
                                  )
                                   (
                                    let (
                                      (
                                        nodes (
                                          hash-table-ref t "nodes"
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            node (
                                              _list v 1 (
                                                - 1
                                              )
                                               (
                                                - 1
                                              )
                                               (
                                                - 1
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
                                            let (
                                              (
                                                idx (
                                                  - (
                                                    _len nodes
                                                  )
                                                   1
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    y NEG_ONE
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        x (
                                                          hash-table-ref t "root"
                                                        )
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
                                                                      not (
                                                                        equal? x NEG_ONE
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! y x
                                                                      )
                                                                       (
                                                                        if (
                                                                          < v (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref nodes x
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref nodes x
                                                                                )
                                                                                 LABEL (
                                                                                  + LABEL 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref nodes x
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref nodes x
                                                                                )
                                                                                 LABEL
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref nodes x
                                                                                )
                                                                                 LABEL
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! x (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref nodes x
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref nodes x
                                                                                  )
                                                                                   LEFT (
                                                                                    + LEFT 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref nodes x
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref nodes x
                                                                                  )
                                                                                   LEFT
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref nodes x
                                                                                  )
                                                                                   LEFT
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! x (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref nodes x
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref nodes x
                                                                                  )
                                                                                   RIGHT (
                                                                                    + RIGHT 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref nodes x
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref nodes x
                                                                                  )
                                                                                   RIGHT
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref nodes x
                                                                                  )
                                                                                   RIGHT
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
                                                        list-set! (
                                                          list-ref nodes idx
                                                        )
                                                         PARENT y
                                                      )
                                                       (
                                                        if (
                                                          equal? y NEG_ONE
                                                        )
                                                         (
                                                          begin (
                                                            hash-table-set! t "root" idx
                                                          )
                                                        )
                                                         (
                                                          if (
                                                            < v (
                                                              cond (
                                                                (
                                                                  string? (
                                                                    list-ref nodes y
                                                                  )
                                                                )
                                                                 (
                                                                  _substring (
                                                                    list-ref nodes y
                                                                  )
                                                                   LABEL (
                                                                    + LABEL 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                (
                                                                  hash-table? (
                                                                    list-ref nodes y
                                                                  )
                                                                )
                                                                 (
                                                                  hash-table-ref (
                                                                    list-ref nodes y
                                                                  )
                                                                   LABEL
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref (
                                                                    list-ref nodes y
                                                                  )
                                                                   LABEL
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              list-set! (
                                                                list-ref nodes y
                                                              )
                                                               LEFT idx
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              list-set! (
                                                                list-ref nodes y
                                                              )
                                                               RIGHT idx
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        hash-table-set! t "nodes" nodes
                                                      )
                                                       (
                                                        set! t (
                                                          insert_fix t idx
                                                        )
                                                      )
                                                       (
                                                        ret7 t
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
                                inorder t x acc
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret10
                                  )
                                   (
                                    begin (
                                      if (
                                        equal? x NEG_ONE
                                      )
                                       (
                                        begin (
                                          ret10 acc
                                        )
                                      )
                                       (
                                        quote (
                                          
                                        )
                                      )
                                    )
                                     (
                                      set! acc (
                                        inorder t (
                                          cond (
                                            (
                                              string? (
                                                list-ref (
                                                  hash-table-ref t "nodes"
                                                )
                                                 x
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref (
                                                  hash-table-ref t "nodes"
                                                )
                                                 x
                                              )
                                               LEFT (
                                                + LEFT 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref (
                                                  hash-table-ref t "nodes"
                                                )
                                                 x
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref (
                                                  hash-table-ref t "nodes"
                                                )
                                                 x
                                              )
                                               LEFT
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref (
                                                  hash-table-ref t "nodes"
                                                )
                                                 x
                                              )
                                               LEFT
                                            )
                                          )
                                        )
                                         acc
                                      )
                                    )
                                     (
                                      set! acc (
                                        append acc (
                                          _list (
                                            cond (
                                              (
                                                string? (
                                                  list-ref (
                                                    hash-table-ref t "nodes"
                                                  )
                                                   x
                                                )
                                              )
                                               (
                                                _substring (
                                                  list-ref (
                                                    hash-table-ref t "nodes"
                                                  )
                                                   x
                                                )
                                                 LABEL (
                                                  + LABEL 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? (
                                                  list-ref (
                                                    hash-table-ref t "nodes"
                                                  )
                                                   x
                                                )
                                              )
                                               (
                                                hash-table-ref (
                                                  list-ref (
                                                    hash-table-ref t "nodes"
                                                  )
                                                   x
                                                )
                                                 LABEL
                                              )
                                            )
                                             (
                                              else (
                                                list-ref (
                                                  list-ref (
                                                    hash-table-ref t "nodes"
                                                  )
                                                   x
                                                )
                                                 LABEL
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      set! acc (
                                        inorder t (
                                          cond (
                                            (
                                              string? (
                                                list-ref (
                                                  hash-table-ref t "nodes"
                                                )
                                                 x
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref (
                                                  hash-table-ref t "nodes"
                                                )
                                                 x
                                              )
                                               RIGHT (
                                                + RIGHT 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref (
                                                  hash-table-ref t "nodes"
                                                )
                                                 x
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref (
                                                  hash-table-ref t "nodes"
                                                )
                                                 x
                                              )
                                               RIGHT
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref (
                                                  hash-table-ref t "nodes"
                                                )
                                                 x
                                              )
                                               RIGHT
                                            )
                                          )
                                        )
                                         acc
                                      )
                                    )
                                     (
                                      ret10 acc
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
                                    ret11
                                  )
                                   (
                                    let (
                                      (
                                        t (
                                          make_tree
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            values (
                                              _list 10 20 30 15 25 5 1
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
                                                    break13
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop12 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              < i (
                                                                _len values
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! t (
                                                                  tree_insert t (
                                                                    list-ref values i
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! i (
                                                                  + i 1
                                                                )
                                                              )
                                                               (
                                                                loop12
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
                                                      loop12
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                let (
                                                  (
                                                    res (
                                                      _list
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! res (
                                                      inorder t (
                                                        hash-table-ref t "root"
                                                      )
                                                       res
                                                    )
                                                  )
                                                   (
                                                    _display (
                                                      if (
                                                        string? (
                                                          to-str-space res
                                                        )
                                                      )
                                                       (
                                                        to-str-space res
                                                      )
                                                       (
                                                        to-str (
                                                          to-str-space res
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
                             (
                              main
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
          end15 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur16 (
              quotient (
                * (
                  - end15 start14
                )
                 1000000
              )
               jps17
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur16
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
