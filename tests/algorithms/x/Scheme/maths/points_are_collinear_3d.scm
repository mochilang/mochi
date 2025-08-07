;; Generated on 2025-08-07 16:11 +0700
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
      start12 (
        current-jiffy
      )
    )
     (
      jps15 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        create_vector p1 p2
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                vx (
                  - (
                    hash-table-ref p2 "x"
                  )
                   (
                    hash-table-ref p1 "x"
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    vy (
                      - (
                        hash-table-ref p2 "y"
                      )
                       (
                        hash-table-ref p1 "y"
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        vz (
                          - (
                            hash-table-ref p2 "z"
                          )
                           (
                            hash-table-ref p1 "z"
                          )
                        )
                      )
                    )
                     (
                      begin (
                        ret1 (
                          alist->hash-table (
                            _list (
                              cons "x" vx
                            )
                             (
                              cons "y" vy
                            )
                             (
                              cons "z" vz
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
        get_3d_vectors_cross ab ac
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                cx (
                  - (
                    * (
                      hash-table-ref ab "y"
                    )
                     (
                      hash-table-ref ac "z"
                    )
                  )
                   (
                    * (
                      hash-table-ref ab "z"
                    )
                     (
                      hash-table-ref ac "y"
                    )
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    cy (
                      - (
                        * (
                          hash-table-ref ab "z"
                        )
                         (
                          hash-table-ref ac "x"
                        )
                      )
                       (
                        * (
                          hash-table-ref ab "x"
                        )
                         (
                          hash-table-ref ac "z"
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        cz (
                          - (
                            * (
                              hash-table-ref ab "x"
                            )
                             (
                              hash-table-ref ac "y"
                            )
                          )
                           (
                            * (
                              hash-table-ref ab "y"
                            )
                             (
                              hash-table-ref ac "x"
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        ret2 (
                          alist->hash-table (
                            _list (
                              cons "x" cx
                            )
                             (
                              cons "y" cy
                            )
                             (
                              cons "z" cz
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
        pow10 exp
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            let (
              (
                result 1.0
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
                                  < i exp
                                )
                                 (
                                  begin (
                                    set! result (
                                      * result 10.0
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
                                 '(
                                  
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
                    ret3 result
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
        round_float x digits
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                factor (
                  pow10 digits
                )
              )
            )
             (
              begin (
                let (
                  (
                    v (
                      * x factor
                    )
                  )
                )
                 (
                  begin (
                    if (
                      _ge v 0.0
                    )
                     (
                      begin (
                        set! v (
                          _add v 0.5
                        )
                      )
                    )
                     (
                      begin (
                        set! v (
                          - v 0.5
                        )
                      )
                    )
                  )
                   (
                    let (
                      (
                        t (
                          let (
                            (
                              v7 v
                            )
                          )
                           (
                            cond (
                              (
                                string? v7
                              )
                               (
                                inexact->exact (
                                  floor (
                                    string->number v7
                                  )
                                )
                              )
                            )
                             (
                              (
                                boolean? v7
                              )
                               (
                                if v7 1 0
                              )
                            )
                             (
                              else (
                                inexact->exact (
                                  floor v7
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        ret6 (
                          _div (
                            + 0.0 t
                          )
                           factor
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
        is_zero_vector v accuracy
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            ret8 (
              and (
                and (
                  equal? (
                    round_float (
                      hash-table-ref v "x"
                    )
                     accuracy
                  )
                   0.0
                )
                 (
                  equal? (
                    round_float (
                      hash-table-ref v "y"
                    )
                     accuracy
                  )
                   0.0
                )
              )
               (
                equal? (
                  round_float (
                    hash-table-ref v "z"
                  )
                   accuracy
                )
                 0.0
              )
            )
          )
        )
      )
    )
     (
      define (
        are_collinear a b c accuracy
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                ab (
                  create_vector a b
                )
              )
            )
             (
              begin (
                let (
                  (
                    ac (
                      create_vector a c
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        cross (
                          get_3d_vectors_cross ab ac
                        )
                      )
                    )
                     (
                      begin (
                        ret9 (
                          is_zero_vector cross accuracy
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
        test_are_collinear
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                p1 (
                  alist->hash-table (
                    _list (
                      cons "x" 0.0
                    )
                     (
                      cons "y" 0.0
                    )
                     (
                      cons "z" 0.0
                    )
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    p2 (
                      alist->hash-table (
                        _list (
                          cons "x" 1.0
                        )
                         (
                          cons "y" 1.0
                        )
                         (
                          cons "z" 1.0
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        p3 (
                          alist->hash-table (
                            _list (
                              cons "x" 2.0
                            )
                             (
                              cons "y" 2.0
                            )
                             (
                              cons "z" 2.0
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          not (
                            are_collinear p1 p2 p3 10
                          )
                        )
                         (
                          begin (
                            panic "collinear test failed"
                          )
                        )
                         '(
                          
                        )
                      )
                       (
                        let (
                          (
                            q3 (
                              alist->hash-table (
                                _list (
                                  cons "x" 1.0
                                )
                                 (
                                  cons "y" 2.0
                                )
                                 (
                                  cons "z" 3.0
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              are_collinear p1 p2 q3 10
                            )
                             (
                              begin (
                                panic "non-collinear test failed"
                              )
                            )
                             '(
                              
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
        main
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            begin (
              test_are_collinear
            )
             (
              let (
                (
                  a (
                    alist->hash-table (
                      _list (
                        cons "x" 4.802293498137402
                      )
                       (
                        cons "y" 3.536233125455244
                      )
                       (
                        cons "z" 0.0
                      )
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      b (
                        alist->hash-table (
                          _list (
                            cons "x" (
                              - 2.186788107953106
                            )
                          )
                           (
                            cons "y" (
                              - 9.24561398001649
                            )
                          )
                           (
                            cons "z" 7.141509524846482
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          c (
                            alist->hash-table (
                              _list (
                                cons "x" 1.530169574640268
                              )
                               (
                                cons "y" (
                                  - 2.447927606600034
                                )
                              )
                               (
                                cons "z" 3.343487096469054
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
                                to-str-space (
                                  are_collinear a b c 10
                                )
                              )
                            )
                             (
                              to-str-space (
                                are_collinear a b c 10
                              )
                            )
                             (
                              to-str (
                                to-str-space (
                                  are_collinear a b c 10
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
                              d (
                                alist->hash-table (
                                  _list (
                                    cons "x" 2.399001826862445
                                  )
                                   (
                                    cons "y" (
                                      - 2.452009976680793
                                    )
                                  )
                                   (
                                    cons "z" 4.464656666157666
                                  )
                                )
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  e (
                                    alist->hash-table (
                                      _list (
                                        cons "x" (
                                          - 3.682816335934376
                                        )
                                      )
                                       (
                                        cons "y" 5.753788986533145
                                      )
                                       (
                                        cons "z" 9.490993909044244
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      f (
                                        alist->hash-table (
                                          _list (
                                            cons "x" 1.962903518985307
                                          )
                                           (
                                            cons "y" 3.741415730125627
                                          )
                                           (
                                            cons "z" 7.0
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
                                            to-str-space (
                                              are_collinear d e f 10
                                            )
                                          )
                                        )
                                         (
                                          to-str-space (
                                            are_collinear d e f 10
                                          )
                                        )
                                         (
                                          to-str (
                                            to-str-space (
                                              are_collinear d e f 10
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
          end13 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur14 (
              quotient (
                * (
                  - end13 start12
                )
                 1000000
              )
               jps15
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur14
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
