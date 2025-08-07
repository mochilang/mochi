;; Generated on 2025-08-07 11:54 +0700
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
(define (_div a b) (/ a b))
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
      start11 (
        current-jiffy
      )
    )
     (
      jps14 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        unique nums
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
                                    _len nums
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        v (
                                          list-ref nums i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            found #f
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
                                                              < j (
                                                                _len res
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  equal? (
                                                                    list-ref res j
                                                                  )
                                                                   v
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! found #t
                                                                  )
                                                                   (
                                                                    break5 (
                                                                      quote (
                                                                        
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
                                                if (
                                                  not found
                                                )
                                                 (
                                                  begin (
                                                    set! res (
                                                      append res (
                                                        _list v
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
        array_equalization vector step_size
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            begin (
              if (
                <= step_size 0
              )
               (
                begin (
                  error "Step size must be positive and non-zero."
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
                  elems (
                    unique vector
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      min_updates (
                        _len vector
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
                                        < i (
                                          _len elems
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              target (
                                                cond (
                                                  (
                                                    string? elems
                                                  )
                                                   (
                                                    _substring elems i (
                                                      + i 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? elems
                                                  )
                                                   (
                                                    hash-table-ref elems i
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref elems i
                                                  )
                                                )
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
                                                      updates 0
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
                                                                    < idx (
                                                                      _len vector
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      if (
                                                                        not (
                                                                          equal? (
                                                                            list-ref vector idx
                                                                          )
                                                                           target
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          set! updates (
                                                                            + updates 1
                                                                          )
                                                                        )
                                                                         (
                                                                          set! idx (
                                                                            + idx step_size
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          set! idx (
                                                                            + idx 1
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
                                                      if (
                                                        < updates min_updates
                                                      )
                                                       (
                                                        begin (
                                                          set! min_updates updates
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
                          ret6 min_updates
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
      _display (
        if (
          string? (
            to-str-space (
              array_equalization (
                _list 1 1 6 2 4 6 5 1 7 2 2 1 7 2 2
              )
               4
            )
          )
        )
         (
          to-str-space (
            array_equalization (
              _list 1 1 6 2 4 6 5 1 7 2 2 1 7 2 2
            )
             4
          )
        )
         (
          to-str (
            to-str-space (
              array_equalization (
                _list 1 1 6 2 4 6 5 1 7 2 2 1 7 2 2
              )
               4
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
              array_equalization (
                _list 22 81 88 71 22 81 632 81 81 22 92
              )
               2
            )
          )
        )
         (
          to-str-space (
            array_equalization (
              _list 22 81 88 71 22 81 632 81 81 22 92
            )
             2
          )
        )
         (
          to-str (
            to-str-space (
              array_equalization (
                _list 22 81 88 71 22 81 632 81 81 22 92
              )
               2
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
              array_equalization (
                _list 0 0 0 0 0 0 0 0 0 0 0 0
              )
               5
            )
          )
        )
         (
          to-str-space (
            array_equalization (
              _list 0 0 0 0 0 0 0 0 0 0 0 0
            )
             5
          )
        )
         (
          to-str (
            to-str-space (
              array_equalization (
                _list 0 0 0 0 0 0 0 0 0 0 0 0
              )
               5
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
              array_equalization (
                _list 22 22 22 33 33 33
              )
               2
            )
          )
        )
         (
          to-str-space (
            array_equalization (
              _list 22 22 22 33 33 33
            )
             2
          )
        )
         (
          to-str (
            to-str-space (
              array_equalization (
                _list 22 22 22 33 33 33
              )
               2
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
              array_equalization (
                _list 1 2 3
              )
               2147483647
            )
          )
        )
         (
          to-str-space (
            array_equalization (
              _list 1 2 3
            )
             2147483647
          )
        )
         (
          to-str (
            to-str-space (
              array_equalization (
                _list 1 2 3
              )
               2147483647
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
          end12 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur13 (
              quotient (
                * (
                  - end12 start11
                )
                 1000000
              )
               jps14
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur13
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
