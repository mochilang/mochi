;; Generated on 2025-08-07 08:56 +0700
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
(
  let (
    (
      start8 (
        current-jiffy
      )
    )
     (
      jps11 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        stringify fs
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              string-append (
                string-append (
                  string-append (
                    string-append (
                      string-append (
                        string-append (
                          string-append (
                            hash-table-ref fs "name"
                          )
                           ": ["
                        )
                         (
                          to-str-space (
                            hash-table-ref fs "left_boundary"
                          )
                        )
                      )
                       ", "
                    )
                     (
                      to-str-space (
                        hash-table-ref fs "peak"
                      )
                    )
                  )
                   ", "
                )
                 (
                  to-str-space (
                    hash-table-ref fs "right_boundary"
                  )
                )
              )
               "]"
            )
          )
        )
      )
    )
     (
      define (
        max2 a b
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
        min2 a b
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            begin (
              if (
                < a b
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
        complement fs
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            ret4 (
              alist->hash-table (
                _list (
                  cons "name" (
                    string-append "¬" (
                      hash-table-ref fs "name"
                    )
                  )
                )
                 (
                  cons "left_boundary" (
                    - 1.0 (
                      hash-table-ref fs "right_boundary"
                    )
                  )
                )
                 (
                  cons "peak" (
                    - 1.0 (
                      hash-table-ref fs "left_boundary"
                    )
                  )
                )
                 (
                  cons "right_boundary" (
                    - 1.0 (
                      hash-table-ref fs "peak"
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
        intersection a b
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            ret5 (
              alist->hash-table (
                _list (
                  cons "name" (
                    string-append (
                      string-append (
                        hash-table-ref a "name"
                      )
                       " ∩ "
                    )
                     (
                      hash-table-ref b "name"
                    )
                  )
                )
                 (
                  cons "left_boundary" (
                    max2 (
                      hash-table-ref a "left_boundary"
                    )
                     (
                      hash-table-ref b "left_boundary"
                    )
                  )
                )
                 (
                  cons "peak" (
                    min2 (
                      hash-table-ref a "right_boundary"
                    )
                     (
                      hash-table-ref b "right_boundary"
                    )
                  )
                )
                 (
                  cons "right_boundary" (
                    _div (
                      + (
                        hash-table-ref a "peak"
                      )
                       (
                        hash-table-ref b "peak"
                      )
                    )
                     2.0
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
        union a b
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            ret6 (
              alist->hash-table (
                _list (
                  cons "name" (
                    string-append (
                      string-append (
                        hash-table-ref a "name"
                      )
                       " U "
                    )
                     (
                      hash-table-ref b "name"
                    )
                  )
                )
                 (
                  cons "left_boundary" (
                    min2 (
                      hash-table-ref a "left_boundary"
                    )
                     (
                      hash-table-ref b "left_boundary"
                    )
                  )
                )
                 (
                  cons "peak" (
                    max2 (
                      hash-table-ref a "right_boundary"
                    )
                     (
                      hash-table-ref b "right_boundary"
                    )
                  )
                )
                 (
                  cons "right_boundary" (
                    _div (
                      + (
                        hash-table-ref a "peak"
                      )
                       (
                        hash-table-ref b "peak"
                      )
                    )
                     2.0
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
        membership fs x
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              if (
                or (
                  <= x (
                    hash-table-ref fs "left_boundary"
                  )
                )
                 (
                  >= x (
                    hash-table-ref fs "right_boundary"
                  )
                )
              )
               (
                begin (
                  ret7 0.0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                and (
                  < (
                    hash-table-ref fs "left_boundary"
                  )
                   x
                )
                 (
                  <= x (
                    hash-table-ref fs "peak"
                  )
                )
              )
               (
                begin (
                  ret7 (
                    _div (
                      - x (
                        hash-table-ref fs "left_boundary"
                      )
                    )
                     (
                      - (
                        hash-table-ref fs "peak"
                      )
                       (
                        hash-table-ref fs "left_boundary"
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
              if (
                and (
                  < (
                    hash-table-ref fs "peak"
                  )
                   x
                )
                 (
                  < x (
                    hash-table-ref fs "right_boundary"
                  )
                )
              )
               (
                begin (
                  ret7 (
                    _div (
                      - (
                        hash-table-ref fs "right_boundary"
                      )
                       x
                    )
                     (
                      - (
                        hash-table-ref fs "right_boundary"
                      )
                       (
                        hash-table-ref fs "peak"
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
              ret7 0.0
            )
          )
        )
      )
    )
     (
      let (
        (
          sheru (
            alist->hash-table (
              _list (
                cons "name" "Sheru"
              )
               (
                cons "left_boundary" 0.4
              )
               (
                cons "peak" 1.0
              )
               (
                cons "right_boundary" 0.6
              )
            )
          )
        )
      )
       (
        begin (
          let (
            (
              siya (
                alist->hash-table (
                  _list (
                    cons "name" "Siya"
                  )
                   (
                    cons "left_boundary" 0.5
                  )
                   (
                    cons "peak" 1.0
                  )
                   (
                    cons "right_boundary" 0.7
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
                    stringify sheru
                  )
                )
                 (
                  stringify sheru
                )
                 (
                  to-str (
                    stringify sheru
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
                    stringify siya
                  )
                )
                 (
                  stringify siya
                )
                 (
                  to-str (
                    stringify siya
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
                  sheru_comp (
                    complement sheru
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        stringify sheru_comp
                      )
                    )
                     (
                      stringify sheru_comp
                    )
                     (
                      to-str (
                        stringify sheru_comp
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
                      inter (
                        intersection siya sheru
                      )
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            stringify inter
                          )
                        )
                         (
                          stringify inter
                        )
                         (
                          to-str (
                            stringify inter
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
                            string-append "Sheru membership 0.5: " (
                              to-str-space (
                                membership sheru 0.5
                              )
                            )
                          )
                        )
                         (
                          string-append "Sheru membership 0.5: " (
                            to-str-space (
                              membership sheru 0.5
                            )
                          )
                        )
                         (
                          to-str (
                            string-append "Sheru membership 0.5: " (
                              to-str-space (
                                membership sheru 0.5
                              )
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
                            string-append "Sheru membership 0.6: " (
                              to-str-space (
                                membership sheru 0.6
                              )
                            )
                          )
                        )
                         (
                          string-append "Sheru membership 0.6: " (
                            to-str-space (
                              membership sheru 0.6
                            )
                          )
                        )
                         (
                          to-str (
                            string-append "Sheru membership 0.6: " (
                              to-str-space (
                                membership sheru 0.6
                              )
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
                          uni (
                            union siya sheru
                          )
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? (
                                stringify uni
                              )
                            )
                             (
                              stringify uni
                            )
                             (
                              to-str (
                                stringify uni
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
     (
      let (
        (
          end9 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur10 (
              quotient (
                * (
                  - end9 start8
                )
                 1000000
              )
               jps11
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur10
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
