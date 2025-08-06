;; Generated on 2025-08-06 18:11 +0700
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
      let (
        (
          alphabet "abcdefghijklmnopqrstuvwxyz"
        )
      )
       (
        begin (
          define (
            contains xs x
          )
           (
            call/cc (
              lambda (
                ret1
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
                                  < i (
                                    _len xs
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      string=? (
                                        list-ref xs i
                                      )
                                       x
                                    )
                                     (
                                      begin (
                                        ret1 #t
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
                    ret1 #f
                  )
                )
              )
            )
          )
        )
         (
          define (
            remove_item xs x
          )
           (
            call/cc (
              lambda (
                ret4
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
                    let (
                      (
                        removed #f
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
                                          < i (
                                            _len xs
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              and (
                                                not removed
                                              )
                                               (
                                                string=? (
                                                  list-ref xs i
                                                )
                                                 x
                                              )
                                            )
                                             (
                                              begin (
                                                set! removed #t
                                              )
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  append res (
                                                    _list (
                                                      list-ref xs i
                                                    )
                                                  )
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
                            ret4 res
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
            word_ladder current path target words
          )
           (
            call/cc (
              lambda (
                ret7
              )
               (
                begin (
                  if (
                    string=? current target
                  )
                   (
                    begin (
                      ret7 path
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
                      i 0
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
                                    < i (
                                      _len current
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
                                                        < j (
                                                          _len alphabet
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              c (
                                                                _substring alphabet j (
                                                                  + j 1
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  transformed (
                                                                    string-append (
                                                                      string-append (
                                                                        _substring current 0 i
                                                                      )
                                                                       c
                                                                    )
                                                                     (
                                                                      _substring current (
                                                                        + i 1
                                                                      )
                                                                       (
                                                                        _len current
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  if (
                                                                    contains words transformed
                                                                  )
                                                                   (
                                                                    begin (
                                                                      let (
                                                                        (
                                                                          new_words (
                                                                            remove_item words transformed
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          let (
                                                                            (
                                                                              new_path (
                                                                                append path (
                                                                                  _list transformed
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              let (
                                                                                (
                                                                                  result (
                                                                                    word_ladder transformed new_path target new_words
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  if (
                                                                                    > (
                                                                                      _len result
                                                                                    )
                                                                                     0
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      ret7 result
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
                                                                  set! j (
                                                                    + j 1
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          loop10
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
                                                loop10
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
                      ret7 (
                        _list
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
                ret12
              )
               (
                let (
                  (
                    w1 (
                      _list "hot" "dot" "dog" "lot" "log" "cog"
                    )
                  )
                )
                 (
                  begin (
                    _display (
                      if (
                        string? (
                          to-str-space (
                            word_ladder "hit" (
                              _list "hit"
                            )
                             "cog" w1
                          )
                        )
                      )
                       (
                        to-str-space (
                          word_ladder "hit" (
                            _list "hit"
                          )
                           "cog" w1
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            word_ladder "hit" (
                              _list "hit"
                            )
                             "cog" w1
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
                        w2 (
                          _list "hot" "dot" "dog" "lot" "log"
                        )
                      )
                    )
                     (
                      begin (
                        _display (
                          if (
                            string? (
                              to-str-space (
                                word_ladder "hit" (
                                  _list "hit"
                                )
                                 "cog" w2
                              )
                            )
                          )
                           (
                            to-str-space (
                              word_ladder "hit" (
                                _list "hit"
                              )
                               "cog" w2
                            )
                          )
                           (
                            to-str (
                              to-str-space (
                                word_ladder "hit" (
                                  _list "hit"
                                )
                                 "cog" w2
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
                            w3 (
                              _list "load" "goad" "gold" "lead" "lord"
                            )
                          )
                        )
                         (
                          begin (
                            _display (
                              if (
                                string? (
                                  to-str-space (
                                    word_ladder "lead" (
                                      _list "lead"
                                    )
                                     "gold" w3
                                  )
                                )
                              )
                               (
                                to-str-space (
                                  word_ladder "lead" (
                                    _list "lead"
                                  )
                                   "gold" w3
                                )
                              )
                               (
                                to-str (
                                  to-str-space (
                                    word_ladder "lead" (
                                      _list "lead"
                                    )
                                     "gold" w3
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
                                w4 (
                                  _list "came" "cage" "code" "cade" "gave"
                                )
                              )
                            )
                             (
                              begin (
                                _display (
                                  if (
                                    string? (
                                      to-str-space (
                                        word_ladder "game" (
                                          _list "game"
                                        )
                                         "code" w4
                                      )
                                    )
                                  )
                                   (
                                    to-str-space (
                                      word_ladder "game" (
                                        _list "game"
                                      )
                                       "code" w4
                                    )
                                  )
                                   (
                                    to-str (
                                      to-str-space (
                                        word_ladder "game" (
                                          _list "game"
                                        )
                                         "code" w4
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
         (
          main
        )
      )
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
