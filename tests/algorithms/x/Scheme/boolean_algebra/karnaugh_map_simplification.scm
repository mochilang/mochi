;; Generated on 2025-08-06 21:38 +0700
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
      start15 (
        current-jiffy
      )
    )
     (
      jps18 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        row_string row
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                s "["
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
                                    _len row
                                  )
                                )
                                 (
                                  begin (
                                    set! s (
                                      string-append s (
                                        to-str-space (
                                          list-ref row i
                                        )
                                      )
                                    )
                                  )
                                   (
                                    if (
                                      < i (
                                        - (
                                          _len row
                                        )
                                         1
                                      )
                                    )
                                     (
                                      begin (
                                        set! s (
                                          string-append s ", "
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
                    set! s (
                      string-append s "]"
                    )
                  )
                   (
                    ret1 s
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
        print_kmap kmap
      )
       (
        call/cc (
          lambda (
            ret4
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
                                _len kmap
                              )
                            )
                             (
                              begin (
                                _display (
                                  if (
                                    string? (
                                      row_string (
                                        list-ref kmap i
                                      )
                                    )
                                  )
                                   (
                                    row_string (
                                      list-ref kmap i
                                    )
                                  )
                                   (
                                    to-str (
                                      row_string (
                                        list-ref kmap i
                                      )
                                    )
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
            )
          )
        )
      )
    )
     (
      define (
        join_terms terms
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              if (
                equal? (
                  _len terms
                )
                 0
              )
               (
                begin (
                  ret7 ""
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
                  res (
                    list-ref terms 0
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      i 1
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
                                      _len terms
                                    )
                                  )
                                   (
                                    begin (
                                      set! res (
                                        string-append (
                                          string-append res " + "
                                        )
                                         (
                                          list-ref terms i
                                        )
                                      )
                                    )
                                     (
                                      set! i (
                                        + i 1
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
                      ret7 res
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
        simplify_kmap board
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                terms (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    a 0
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
                                  < a (
                                    _len board
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        row (
                                          list-ref board a
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            b 0
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
                                                          < b (
                                                            _len row
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                item (
                                                                  list-ref row b
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  not (
                                                                    equal? item 0
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        term (
                                                                          string-append (
                                                                            if (
                                                                              not (
                                                                                equal? a 0
                                                                              )
                                                                            )
                                                                             "A" "A'"
                                                                          )
                                                                           (
                                                                            if (
                                                                              not (
                                                                                equal? b 0
                                                                              )
                                                                            )
                                                                             "B" "B'"
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! terms (
                                                                          append terms (
                                                                            _list term
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
                                                                set! b (
                                                                  + b 1
                                                                )
                                                              )
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
                                            set! a (
                                              + a 1
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
                    let (
                      (
                        expr (
                          join_terms terms
                        )
                      )
                    )
                     (
                      begin (
                        ret10 expr
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
          kmap (
            _list (
              _list 0 1
            )
             (
              _list 1 1
            )
          )
        )
      )
       (
        begin (
          print_kmap kmap
        )
         (
          _display (
            if (
              string? "Simplified Expression:"
            )
             "Simplified Expression:" (
              to-str "Simplified Expression:"
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
                simplify_kmap kmap
              )
            )
             (
              simplify_kmap kmap
            )
             (
              to-str (
                simplify_kmap kmap
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
          end16 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur17 (
              quotient (
                * (
                  - end16 start15
                )
                 1000000
              )
               jps18
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur17
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
