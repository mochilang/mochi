;; Generated on 2025-08-07 16:45 +0700
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
        ((number? x)
         (if (integer? x)
             (number->string (inexact->exact x))
             (number->string x)))
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
      start18 (
        current-jiffy
      )
    )
     (
      jps21 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          grid (
            _list (
              _list 8 2 22 97 38 15 0 40 0 75 4 5 7 78 52 12 50 77 91 8
            )
             (
              _list 49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 4 56 62 0
            )
             (
              _list 81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 3 49 13 36 65
            )
             (
              _list 52 70 95 23 4 60 11 42 69 24 68 56 1 32 56 71 37 2 36 91
            )
             (
              _list 22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
            )
             (
              _list 24 47 32 60 99 3 45 2 44 75 33 53 78 36 84 20 35 17 12 50
            )
             (
              _list 32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
            )
             (
              _list 67 26 20 68 2 62 12 20 95 63 94 39 63 8 40 91 66 49 94 21
            )
             (
              _list 24 55 58 5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
            )
             (
              _list 21 36 23 9 75 0 76 44 20 45 35 14 0 61 33 97 34 31 33 95
            )
             (
              _list 78 17 53 28 22 75 31 67 15 94 3 80 4 62 16 14 9 53 56 92
            )
             (
              _list 16 39 5 42 96 35 31 47 55 58 88 24 0 17 54 24 36 29 85 57
            )
             (
              _list 86 56 0 48 35 71 89 7 5 44 44 37 44 60 21 58 51 54 17 58
            )
             (
              _list 19 80 81 68 5 94 47 69 28 73 92 13 86 52 17 77 4 89 55 40
            )
             (
              _list 4 52 8 83 97 35 99 16 7 97 57 32 16 26 26 79 33 27 98 66
            )
             (
              _list 88 36 68 87 57 62 20 72 3 46 33 67 46 55 12 32 63 93 53 69
            )
             (
              _list 4 42 16 73 38 25 39 11 24 94 72 18 8 46 29 32 40 62 76 36
            )
             (
              _list 20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 4 36 16
            )
             (
              _list 20 73 35 29 78 31 90 1 74 31 49 71 48 86 81 16 23 57 5 54
            )
             (
              _list 1 70 54 71 83 51 54 69 16 92 33 48 61 43 52 1 89 19 67 48
            )
          )
        )
      )
       (
        begin (
          define (
            max_product_four grid
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                let (
                  (
                    maximum 0
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
                                      < i 20
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
                                                          < j 17
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                temp (
                                                                  * (
                                                                    * (
                                                                      * (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref-safe grid i
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref-safe grid i
                                                                            )
                                                                             j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref-safe grid i
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref-safe grid i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref-safe (
                                                                              list-ref-safe grid i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref-safe grid i
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref-safe grid i
                                                                            )
                                                                             (
                                                                              + j 1
                                                                            )
                                                                             (
                                                                              + (
                                                                                + j 1
                                                                              )
                                                                               1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref-safe grid i
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref-safe grid i
                                                                            )
                                                                             (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref-safe (
                                                                              list-ref-safe grid i
                                                                            )
                                                                             (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            list-ref-safe grid i
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref-safe grid i
                                                                          )
                                                                           (
                                                                            + j 2
                                                                          )
                                                                           (
                                                                            + (
                                                                              + j 2
                                                                            )
                                                                             1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref-safe grid i
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref-safe grid i
                                                                          )
                                                                           (
                                                                            + j 2
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref-safe (
                                                                            list-ref-safe grid i
                                                                          )
                                                                           (
                                                                            + j 2
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref-safe grid i
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref-safe grid i
                                                                        )
                                                                         (
                                                                          + j 3
                                                                        )
                                                                         (
                                                                          + (
                                                                            + j 3
                                                                          )
                                                                           1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref-safe grid i
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref-safe grid i
                                                                        )
                                                                         (
                                                                          + j 3
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref-safe (
                                                                          list-ref-safe grid i
                                                                        )
                                                                         (
                                                                          + j 3
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
                                                                  > temp maximum
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! maximum temp
                                                                  )
                                                                )
                                                                 '(
                                                                  
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
                                            set! i (
                                              + i 1
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop2
                                      )
                                    )
                                     '(
                                      
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
                        set! i 0
                      )
                       (
                        call/cc (
                          lambda (
                            break7
                          )
                           (
                            letrec (
                              (
                                loop6 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i 17
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
                                                          < j 20
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                temp (
                                                                  * (
                                                                    * (
                                                                      * (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref-safe grid i
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref-safe grid i
                                                                            )
                                                                             j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref-safe grid i
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref-safe grid i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref-safe (
                                                                              list-ref-safe grid i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref-safe grid (
                                                                                + i 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref-safe grid (
                                                                                + i 1
                                                                              )
                                                                            )
                                                                             j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref-safe grid (
                                                                                + i 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref-safe grid (
                                                                                + i 1
                                                                              )
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref-safe (
                                                                              list-ref-safe grid (
                                                                                + i 1
                                                                              )
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            list-ref-safe grid (
                                                                              + i 2
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref-safe grid (
                                                                              + i 2
                                                                            )
                                                                          )
                                                                           j (
                                                                            + j 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref-safe grid (
                                                                              + i 2
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref-safe grid (
                                                                              + i 2
                                                                            )
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref-safe (
                                                                            list-ref-safe grid (
                                                                              + i 2
                                                                            )
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref-safe grid (
                                                                            + i 3
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref-safe grid (
                                                                            + i 3
                                                                          )
                                                                        )
                                                                         j (
                                                                          + j 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref-safe grid (
                                                                            + i 3
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref-safe grid (
                                                                            + i 3
                                                                          )
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref-safe (
                                                                          list-ref-safe grid (
                                                                            + i 3
                                                                          )
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  > temp maximum
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! maximum temp
                                                                  )
                                                                )
                                                                 '(
                                                                  
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
                                                            loop8
                                                          )
                                                        )
                                                         '(
                                                          
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
                                            set! i (
                                              + i 1
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop6
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop6
                            )
                          )
                        )
                      )
                       (
                        set! i 0
                      )
                       (
                        call/cc (
                          lambda (
                            break11
                          )
                           (
                            letrec (
                              (
                                loop10 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i 17
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
                                                          < j 17
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                temp (
                                                                  * (
                                                                    * (
                                                                      * (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref-safe grid i
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref-safe grid i
                                                                            )
                                                                             j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref-safe grid i
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref-safe grid i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref-safe (
                                                                              list-ref-safe grid i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref-safe grid (
                                                                                + i 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref-safe grid (
                                                                                + i 1
                                                                              )
                                                                            )
                                                                             (
                                                                              + j 1
                                                                            )
                                                                             (
                                                                              + (
                                                                                + j 1
                                                                              )
                                                                               1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref-safe grid (
                                                                                + i 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref-safe grid (
                                                                                + i 1
                                                                              )
                                                                            )
                                                                             (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref-safe (
                                                                              list-ref-safe grid (
                                                                                + i 1
                                                                              )
                                                                            )
                                                                             (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            list-ref-safe grid (
                                                                              + i 2
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref-safe grid (
                                                                              + i 2
                                                                            )
                                                                          )
                                                                           (
                                                                            + j 2
                                                                          )
                                                                           (
                                                                            + (
                                                                              + j 2
                                                                            )
                                                                             1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref-safe grid (
                                                                              + i 2
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref-safe grid (
                                                                              + i 2
                                                                            )
                                                                          )
                                                                           (
                                                                            + j 2
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref-safe (
                                                                            list-ref-safe grid (
                                                                              + i 2
                                                                            )
                                                                          )
                                                                           (
                                                                            + j 2
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref-safe grid (
                                                                            + i 3
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref-safe grid (
                                                                            + i 3
                                                                          )
                                                                        )
                                                                         (
                                                                          + j 3
                                                                        )
                                                                         (
                                                                          + (
                                                                            + j 3
                                                                          )
                                                                           1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref-safe grid (
                                                                            + i 3
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref-safe grid (
                                                                            + i 3
                                                                          )
                                                                        )
                                                                         (
                                                                          + j 3
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref-safe (
                                                                          list-ref-safe grid (
                                                                            + i 3
                                                                          )
                                                                        )
                                                                         (
                                                                          + j 3
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
                                                                  > temp maximum
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! maximum temp
                                                                  )
                                                                )
                                                                 '(
                                                                  
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
                                                            loop12
                                                          )
                                                        )
                                                         '(
                                                          
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
                                            set! i (
                                              + i 1
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop10
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop10
                            )
                          )
                        )
                      )
                       (
                        set! i 0
                      )
                       (
                        call/cc (
                          lambda (
                            break15
                          )
                           (
                            letrec (
                              (
                                loop14 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i 17
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            j 3
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
                                                          < j 20
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                temp (
                                                                  * (
                                                                    * (
                                                                      * (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref-safe grid i
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref-safe grid i
                                                                            )
                                                                             j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref-safe grid i
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref-safe grid i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref-safe (
                                                                              list-ref-safe grid i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref-safe grid (
                                                                                + i 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref-safe grid (
                                                                                + i 1
                                                                              )
                                                                            )
                                                                             (
                                                                              - j 1
                                                                            )
                                                                             (
                                                                              + (
                                                                                - j 1
                                                                              )
                                                                               1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref-safe grid (
                                                                                + i 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref-safe grid (
                                                                                + i 1
                                                                              )
                                                                            )
                                                                             (
                                                                              - j 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref-safe (
                                                                              list-ref-safe grid (
                                                                                + i 1
                                                                              )
                                                                            )
                                                                             (
                                                                              - j 1
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            list-ref-safe grid (
                                                                              + i 2
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref-safe grid (
                                                                              + i 2
                                                                            )
                                                                          )
                                                                           (
                                                                            - j 2
                                                                          )
                                                                           (
                                                                            + (
                                                                              - j 2
                                                                            )
                                                                             1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref-safe grid (
                                                                              + i 2
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref-safe grid (
                                                                              + i 2
                                                                            )
                                                                          )
                                                                           (
                                                                            - j 2
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref-safe (
                                                                            list-ref-safe grid (
                                                                              + i 2
                                                                            )
                                                                          )
                                                                           (
                                                                            - j 2
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref-safe grid (
                                                                            + i 3
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref-safe grid (
                                                                            + i 3
                                                                          )
                                                                        )
                                                                         (
                                                                          - j 3
                                                                        )
                                                                         (
                                                                          + (
                                                                            - j 3
                                                                          )
                                                                           1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref-safe grid (
                                                                            + i 3
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref-safe grid (
                                                                            + i 3
                                                                          )
                                                                        )
                                                                         (
                                                                          - j 3
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref-safe (
                                                                          list-ref-safe grid (
                                                                            + i 3
                                                                          )
                                                                        )
                                                                         (
                                                                          - j 3
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
                                                                  > temp maximum
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! maximum temp
                                                                  )
                                                                )
                                                                 '(
                                                                  
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
                                                            loop16
                                                          )
                                                        )
                                                         '(
                                                          
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
                                            set! i (
                                              + i 1
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop14
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop14
                            )
                          )
                        )
                      )
                       (
                        ret1 maximum
                      )
                    )
                  )
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
                  max_product_four grid
                )
              )
            )
             (
              to-str-space (
                max_product_four grid
              )
            )
             (
              to-str (
                to-str-space (
                  max_product_four grid
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
     (
      let (
        (
          end19 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur20 (
              quotient (
                * (
                  - end19 start18
                )
                 1000000
              )
               jps21
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur20
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
