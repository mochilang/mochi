;; Generated on 2025-08-07 08:40 +0700
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
      define (
        copy_list xs
      )
       (
        call/cc (
          lambda (
            ret1
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
                                    set! res (
                                      append res (
                                        _list (
                                          list-ref xs i
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
                    ret1 res
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
        longest_subsequence arr
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                n (
                  _len arr
                )
              )
            )
             (
              begin (
                let (
                  (
                    lis (
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
                                      < i n
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            single (
                                              _list
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! single (
                                              append single (
                                                _list (
                                                  list-ref arr i
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! lis (
                                              append lis (
                                                _list single
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
                        set! i 1
                      )
                       (
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
                                      < i n
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            prev 0
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
                                                          < prev i
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              and (
                                                                <= (
                                                                  list-ref arr prev
                                                                )
                                                                 (
                                                                  list-ref arr i
                                                                )
                                                              )
                                                               (
                                                                > (
                                                                  + (
                                                                    _len (
                                                                      list-ref lis prev
                                                                    )
                                                                  )
                                                                   1
                                                                )
                                                                 (
                                                                  _len (
                                                                    list-ref lis i
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    temp (
                                                                      copy_list (
                                                                        list-ref lis prev
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        temp2 (
                                                                          append temp (
                                                                            _list (
                                                                              list-ref arr i
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        list-set! lis i temp2
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
                                                            set! prev (
                                                              + prev 1
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
                        let (
                          (
                            result (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            set! i 0
                          )
                           (
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
                                          < i n
                                        )
                                         (
                                          begin (
                                            if (
                                              > (
                                                _len (
                                                  list-ref lis i
                                                )
                                              )
                                               (
                                                _len result
                                              )
                                            )
                                             (
                                              begin (
                                                set! result (
                                                  list-ref lis i
                                                )
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
                            ret4 result
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
            ret13
          )
           (
            begin (
              _display (
                if (
                  string? (
                    to-str-space (
                      longest_subsequence (
                        _list 10 22 9 33 21 50 41 60 80
                      )
                    )
                  )
                )
                 (
                  to-str-space (
                    longest_subsequence (
                      _list 10 22 9 33 21 50 41 60 80
                    )
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      longest_subsequence (
                        _list 10 22 9 33 21 50 41 60 80
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
                    to-str-space (
                      longest_subsequence (
                        _list 4 8 7 5 1 12 2 3 9
                      )
                    )
                  )
                )
                 (
                  to-str-space (
                    longest_subsequence (
                      _list 4 8 7 5 1 12 2 3 9
                    )
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      longest_subsequence (
                        _list 4 8 7 5 1 12 2 3 9
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
                    to-str-space (
                      longest_subsequence (
                        _list 9 8 7 6 5 7
                      )
                    )
                  )
                )
                 (
                  to-str-space (
                    longest_subsequence (
                      _list 9 8 7 6 5 7
                    )
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      longest_subsequence (
                        _list 9 8 7 6 5 7
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
                    to-str-space (
                      longest_subsequence (
                        _list 28 26 12 23 35 39
                      )
                    )
                  )
                )
                 (
                  to-str-space (
                    longest_subsequence (
                      _list 28 26 12 23 35 39
                    )
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      longest_subsequence (
                        _list 28 26 12 23 35 39
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
                    to-str-space (
                      longest_subsequence (
                        _list 1 1 1
                      )
                    )
                  )
                )
                 (
                  to-str-space (
                    longest_subsequence (
                      _list 1 1 1
                    )
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      longest_subsequence (
                        _list 1 1 1
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
                    to-str-space (
                      longest_subsequence (
                        _list
                      )
                    )
                  )
                )
                 (
                  to-str-space (
                    longest_subsequence (
                      _list
                    )
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      longest_subsequence (
                        _list
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
     (
      main
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
