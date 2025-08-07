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
      start33 (
        current-jiffy
      )
    )
     (
      jps36 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        pad_edge image pad_size
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                height (
                  _len image
                )
              )
            )
             (
              begin (
                let (
                  (
                    width (
                      _len (
                        list-ref image 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        new_height (
                          + height (
                            * pad_size 2
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            new_width (
                              + width (
                                * pad_size 2
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                padded (
                                  _list
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
                                                  < i new_height
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        row (
                                                          _list
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            src_i i
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              < src_i pad_size
                                                            )
                                                             (
                                                              begin (
                                                                set! src_i 0
                                                              )
                                                            )
                                                             (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                           (
                                                            if (
                                                              >= src_i (
                                                                + height pad_size
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! src_i (
                                                                  - height 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! src_i (
                                                                  - src_i pad_size
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            let (
                                                              (
                                                                j 0
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
                                                                              < j new_width
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    src_j j
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    if (
                                                                                      < src_j pad_size
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! src_j 0
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      quote (
                                                                                        
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    if (
                                                                                      >= src_j (
                                                                                        + width pad_size
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! src_j (
                                                                                          - width 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! src_j (
                                                                                          - src_j pad_size
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! row (
                                                                                      append row (
                                                                                        _list (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                list-ref image src_i
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref image src_i
                                                                                              )
                                                                                               src_j (
                                                                                                + src_j 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref image src_i
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref image src_i
                                                                                              )
                                                                                               src_j
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref image src_i
                                                                                              )
                                                                                               src_j
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! j (
                                                                                      + j 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                loop4
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
                                                                      loop4
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! padded (
                                                                  append padded (
                                                                    _list row
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! i (
                                                                  + i 1
                                                                )
                                                              )
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
                                    ret1 padded
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
        im2col image block_h block_w
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                rows (
                  _len image
                )
              )
            )
             (
              begin (
                let (
                  (
                    cols (
                      _len (
                        list-ref image 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        dst_height (
                          + (
                            - rows block_h
                          )
                           1
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            dst_width (
                              + (
                                - cols block_w
                              )
                               1
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                image_array (
                                  _list
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
                                        break8
                                      )
                                       (
                                        letrec (
                                          (
                                            loop7 (
                                              lambda (
                                                
                                              )
                                               (
                                                if (
                                                  < i dst_height
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        j 0
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
                                                                      < j dst_width
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            window (
                                                                              _list
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                bi 0
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
                                                                                              < bi block_h
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                let (
                                                                                                  (
                                                                                                    bj 0
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    call/cc (
                                                                                                      lambda (
                                                                                                        break14
                                                                                                      )
                                                                                                       (
                                                                                                        letrec (
                                                                                                          (
                                                                                                            loop13 (
                                                                                                              lambda (
                                                                                                                
                                                                                                              )
                                                                                                               (
                                                                                                                if (
                                                                                                                  < bj block_w
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    set! window (
                                                                                                                      append window (
                                                                                                                        _list (
                                                                                                                          cond (
                                                                                                                            (
                                                                                                                              string? (
                                                                                                                                list-ref image (
                                                                                                                                  + i bi
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              _substring (
                                                                                                                                list-ref image (
                                                                                                                                  + i bi
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                + j bj
                                                                                                                              )
                                                                                                                               (
                                                                                                                                + (
                                                                                                                                  + j bj
                                                                                                                                )
                                                                                                                                 1
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            (
                                                                                                                              hash-table? (
                                                                                                                                list-ref image (
                                                                                                                                  + i bi
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              hash-table-ref (
                                                                                                                                list-ref image (
                                                                                                                                  + i bi
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                + j bj
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            else (
                                                                                                                              list-ref (
                                                                                                                                list-ref image (
                                                                                                                                  + i bi
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                + j bj
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    set! bj (
                                                                                                                      + bj 1
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    loop13
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
                                                                                                          loop13
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    set! bi (
                                                                                                      + bi 1
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
                                                                                set! image_array (
                                                                                  append image_array (
                                                                                    _list window
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! j (
                                                                                  + j 1
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
                                                        set! i (
                                                          + i 1
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    loop7
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
                                          loop7
                                        )
                                      )
                                    )
                                  )
                                   (
                                    ret6 image_array
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
        flatten matrix
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            let (
              (
                out (
                  _list
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
                        break17
                      )
                       (
                        letrec (
                          (
                            loop16 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len matrix
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        j 0
                                      )
                                    )
                                     (
                                      begin (
                                        call/cc (
                                          lambda (
                                            break19
                                          )
                                           (
                                            letrec (
                                              (
                                                loop18 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < j (
                                                        _len (
                                                          list-ref matrix i
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! out (
                                                          append out (
                                                            _list (
                                                              cond (
                                                                (
                                                                  string? (
                                                                    list-ref matrix i
                                                                  )
                                                                )
                                                                 (
                                                                  _substring (
                                                                    list-ref matrix i
                                                                  )
                                                                   j (
                                                                    + j 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                (
                                                                  hash-table? (
                                                                    list-ref matrix i
                                                                  )
                                                                )
                                                                 (
                                                                  hash-table-ref (
                                                                    list-ref matrix i
                                                                  )
                                                                   j
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref (
                                                                    list-ref matrix i
                                                                  )
                                                                   j
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! j (
                                                          + j 1
                                                        )
                                                      )
                                                       (
                                                        loop18
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
                                              loop18
                                            )
                                          )
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop16
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
                          loop16
                        )
                      )
                    )
                  )
                   (
                    ret15 out
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
        dot a b
      )
       (
        call/cc (
          lambda (
            ret20
          )
           (
            let (
              (
                sum 0
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
                        break22
                      )
                       (
                        letrec (
                          (
                            loop21 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len a
                                  )
                                )
                                 (
                                  begin (
                                    set! sum (
                                      + sum (
                                        * (
                                          list-ref a i
                                        )
                                         (
                                          list-ref b i
                                        )
                                      )
                                    )
                                  )
                                   (
                                    set! i (
                                      + i 1
                                    )
                                  )
                                   (
                                    loop21
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
                          loop21
                        )
                      )
                    )
                  )
                   (
                    ret20 sum
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
        img_convolve image kernel
      )
       (
        call/cc (
          lambda (
            ret23
          )
           (
            let (
              (
                height (
                  _len image
                )
              )
            )
             (
              begin (
                let (
                  (
                    width (
                      _len (
                        list-ref image 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        k_size (
                          _len kernel
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            pad_size (
                              _div k_size 2
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                padded (
                                  pad_edge image pad_size
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    image_array (
                                      im2col padded k_size k_size
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        kernel_flat (
                                          flatten kernel
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            dst (
                                              _list
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                idx 0
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
                                                        break25
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop24 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < i height
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        row (
                                                                          _list
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            j 0
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            call/cc (
                                                                              lambda (
                                                                                break27
                                                                              )
                                                                               (
                                                                                letrec (
                                                                                  (
                                                                                    loop26 (
                                                                                      lambda (
                                                                                        
                                                                                      )
                                                                                       (
                                                                                        if (
                                                                                          < j width
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                val (
                                                                                                  dot (
                                                                                                    cond (
                                                                                                      (
                                                                                                        string? image_array
                                                                                                      )
                                                                                                       (
                                                                                                        _substring image_array idx (
                                                                                                          + idx 1
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      (
                                                                                                        hash-table? image_array
                                                                                                      )
                                                                                                       (
                                                                                                        hash-table-ref image_array idx
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      else (
                                                                                                        list-ref image_array idx
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   kernel_flat
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! row (
                                                                                                  append row (
                                                                                                    _list val
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                set! idx (
                                                                                                  + idx 1
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                set! j (
                                                                                                  + j 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            loop26
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
                                                                                  loop26
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! dst (
                                                                              append dst (
                                                                                _list row
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! i (
                                                                              + i 1
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    loop24
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
                                                          loop24
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    ret23 dst
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
            )
          )
        )
      )
    )
     (
      define (
        print_matrix m
      )
       (
        call/cc (
          lambda (
            ret28
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
                    break30
                  )
                   (
                    letrec (
                      (
                        loop29 (
                          lambda (
                            
                          )
                           (
                            if (
                              < i (
                                _len m
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    line ""
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        j 0
                                      )
                                    )
                                     (
                                      begin (
                                        call/cc (
                                          lambda (
                                            break32
                                          )
                                           (
                                            letrec (
                                              (
                                                loop31 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < j (
                                                        _len (
                                                          list-ref m i
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          > j 0
                                                        )
                                                         (
                                                          begin (
                                                            set! line (
                                                              string-append line " "
                                                            )
                                                          )
                                                        )
                                                         (
                                                          quote (
                                                            
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! line (
                                                          string-append line (
                                                            to-str-space (
                                                              cond (
                                                                (
                                                                  string? (
                                                                    list-ref m i
                                                                  )
                                                                )
                                                                 (
                                                                  _substring (
                                                                    list-ref m i
                                                                  )
                                                                   j (
                                                                    + j 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                (
                                                                  hash-table? (
                                                                    list-ref m i
                                                                  )
                                                                )
                                                                 (
                                                                  hash-table-ref (
                                                                    list-ref m i
                                                                  )
                                                                   j
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref (
                                                                    list-ref m i
                                                                  )
                                                                   j
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! j (
                                                          + j 1
                                                        )
                                                      )
                                                       (
                                                        loop31
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
                                              loop31
                                            )
                                          )
                                        )
                                      )
                                       (
                                        _display (
                                          if (
                                            string? line
                                          )
                                           line (
                                            to-str line
                                          )
                                        )
                                      )
                                       (
                                        newline
                                      )
                                       (
                                        set! i (
                                          + i 1
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                loop29
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
                      loop29
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
          image (
            _list (
              _list 1 2 3 0 0
            )
             (
              _list 4 5 6 0 0
            )
             (
              _list 7 8 9 0 0
            )
             (
              _list 0 0 0 0 0
            )
             (
              _list 0 0 0 0 0
            )
          )
        )
      )
       (
        begin (
          let (
            (
              laplace_kernel (
                _list (
                  _list 0 1 0
                )
                 (
                  _list 1 (
                    - 4
                  )
                   1
                )
                 (
                  _list 0 1 0
                )
              )
            )
          )
           (
            begin (
              let (
                (
                  result (
                    img_convolve image laplace_kernel
                  )
                )
              )
               (
                begin (
                  print_matrix result
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
          end34 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur35 (
              quotient (
                * (
                  - end34 start33
                )
                 1000000
              )
               jps36
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur35
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
