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
      start17 (
        current-jiffy
      )
    )
     (
      jps20 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          seed 1
        )
      )
       (
        begin (
          define (
            rand
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                begin (
                  set! seed (
                    _mod (
                      + (
                        * seed 1103515245
                      )
                       12345
                    )
                     2147483648
                  )
                )
                 (
                  ret1 seed
                )
              )
            )
          )
        )
         (
          define (
            random
          )
           (
            call/cc (
              lambda (
                ret2
              )
               (
                ret2 (
                  _div (
                    * 1.0 (
                      rand
                    )
                  )
                   2147483648.0
                )
              )
            )
          )
        )
         (
          define (
            get_nodes trans
          )
           (
            call/cc (
              lambda (
                ret3
              )
               (
                let (
                  (
                    seen (
                      alist->hash-table (
                        _list
                      )
                    )
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
                                xs
                              )
                               (
                                if (
                                  null? xs
                                )
                                 (
                                  quote (
                                    
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        t (
                                          car xs
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        hash-table-set! seen (
                                          hash-table-ref t "src"
                                        )
                                         #t
                                      )
                                       (
                                        hash-table-set! seen (
                                          hash-table-ref t "dst"
                                        )
                                         #t
                                      )
                                    )
                                  )
                                   (
                                    loop4 (
                                      cdr xs
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          loop4 trans
                        )
                      )
                    )
                  )
                   (
                    let (
                      (
                        nodes (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        call/cc (
                          lambda (
                            break7
                          )
                           (
                            letrec (
                              (
                                loop6 (
                                  lambda (
                                    xs
                                  )
                                   (
                                    if (
                                      null? xs
                                    )
                                     (
                                      quote (
                                        
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            k (
                                              car xs
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! nodes (
                                              append nodes (
                                                _list k
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop6 (
                                          cdr xs
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop6 (
                                hash-table-keys seen
                              )
                            )
                          )
                        )
                      )
                       (
                        ret3 nodes
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
            transition current trans
          )
           (
            call/cc (
              lambda (
                ret8
              )
               (
                let (
                  (
                    current_probability 0.0
                  )
                )
                 (
                  begin (
                    let (
                      (
                        random_value (
                          random
                        )
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
                                    xs
                                  )
                                   (
                                    if (
                                      null? xs
                                    )
                                     (
                                      quote (
                                        
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            t (
                                              car xs
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              string=? (
                                                hash-table-ref t "src"
                                              )
                                               current
                                            )
                                             (
                                              begin (
                                                set! current_probability (
                                                  _add current_probability (
                                                    hash-table-ref t "prob"
                                                  )
                                                )
                                              )
                                               (
                                                if (
                                                  _gt current_probability random_value
                                                )
                                                 (
                                                  begin (
                                                    ret8 (
                                                      hash-table-ref t "dst"
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
                                              quote (
                                                
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop9 (
                                          cdr xs
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop9 trans
                            )
                          )
                        )
                      )
                       (
                        ret8 ""
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
            get_transitions start trans steps
          )
           (
            call/cc (
              lambda (
                ret11
              )
               (
                let (
                  (
                    visited (
                      alist->hash-table (
                        _list
                      )
                    )
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
                                xs
                              )
                               (
                                if (
                                  null? xs
                                )
                                 (
                                  quote (
                                    
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        node (
                                          car xs
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            one 1
                                          )
                                        )
                                         (
                                          begin (
                                            hash-table-set! visited node one
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop12 (
                                      cdr xs
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          loop12 (
                            get_nodes trans
                          )
                        )
                      )
                    )
                  )
                   (
                    let (
                      (
                        node start
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
                                          < i steps
                                        )
                                         (
                                          begin (
                                            set! node (
                                              transition node trans
                                            )
                                          )
                                           (
                                            let (
                                              (
                                                count (
                                                  hash-table-ref/default visited node (
                                                    quote (
                                                      
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! count (
                                                  + count 1
                                                )
                                              )
                                               (
                                                hash-table-set! visited node count
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
                                         (
                                          quote (
                                            
                                          )
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
                            ret11 visited
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
                ret16
              )
               (
                let (
                  (
                    transitions (
                      _list (
                        alist->hash-table (
                          _list (
                            cons "src" "a"
                          )
                           (
                            cons "dst" "a"
                          )
                           (
                            cons "prob" 0.9
                          )
                        )
                      )
                       (
                        alist->hash-table (
                          _list (
                            cons "src" "a"
                          )
                           (
                            cons "dst" "b"
                          )
                           (
                            cons "prob" 0.075
                          )
                        )
                      )
                       (
                        alist->hash-table (
                          _list (
                            cons "src" "a"
                          )
                           (
                            cons "dst" "c"
                          )
                           (
                            cons "prob" 0.025
                          )
                        )
                      )
                       (
                        alist->hash-table (
                          _list (
                            cons "src" "b"
                          )
                           (
                            cons "dst" "a"
                          )
                           (
                            cons "prob" 0.15
                          )
                        )
                      )
                       (
                        alist->hash-table (
                          _list (
                            cons "src" "b"
                          )
                           (
                            cons "dst" "b"
                          )
                           (
                            cons "prob" 0.8
                          )
                        )
                      )
                       (
                        alist->hash-table (
                          _list (
                            cons "src" "b"
                          )
                           (
                            cons "dst" "c"
                          )
                           (
                            cons "prob" 0.05
                          )
                        )
                      )
                       (
                        alist->hash-table (
                          _list (
                            cons "src" "c"
                          )
                           (
                            cons "dst" "a"
                          )
                           (
                            cons "prob" 0.25
                          )
                        )
                      )
                       (
                        alist->hash-table (
                          _list (
                            cons "src" "c"
                          )
                           (
                            cons "dst" "b"
                          )
                           (
                            cons "prob" 0.25
                          )
                        )
                      )
                       (
                        alist->hash-table (
                          _list (
                            cons "src" "c"
                          )
                           (
                            cons "dst" "c"
                          )
                           (
                            cons "prob" 0.5
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
                        result (
                          get_transitions "a" transitions 5000
                        )
                      )
                    )
                     (
                      begin (
                        _display (
                          if (
                            string? (
                              string-append (
                                string-append (
                                  string-append (
                                    string-append (
                                      to-str-space (
                                        cond (
                                          (
                                            string? result
                                          )
                                           (
                                            _substring result "a" (
                                              + "a" 1
                                            )
                                          )
                                        )
                                         (
                                          (
                                            hash-table? result
                                          )
                                           (
                                            hash-table-ref result "a"
                                          )
                                        )
                                         (
                                          else (
                                            list-ref result "a"
                                          )
                                        )
                                      )
                                    )
                                     " "
                                  )
                                   (
                                    to-str-space (
                                      cond (
                                        (
                                          string? result
                                        )
                                         (
                                          _substring result "b" (
                                            + "b" 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? result
                                        )
                                         (
                                          hash-table-ref result "b"
                                        )
                                      )
                                       (
                                        else (
                                          list-ref result "b"
                                        )
                                      )
                                    )
                                  )
                                )
                                 " "
                              )
                               (
                                to-str-space (
                                  cond (
                                    (
                                      string? result
                                    )
                                     (
                                      _substring result "c" (
                                        + "c" 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? result
                                    )
                                     (
                                      hash-table-ref result "c"
                                    )
                                  )
                                   (
                                    else (
                                      list-ref result "c"
                                    )
                                  )
                                )
                              )
                            )
                          )
                           (
                            string-append (
                              string-append (
                                string-append (
                                  string-append (
                                    to-str-space (
                                      cond (
                                        (
                                          string? result
                                        )
                                         (
                                          _substring result "a" (
                                            + "a" 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? result
                                        )
                                         (
                                          hash-table-ref result "a"
                                        )
                                      )
                                       (
                                        else (
                                          list-ref result "a"
                                        )
                                      )
                                    )
                                  )
                                   " "
                                )
                                 (
                                  to-str-space (
                                    cond (
                                      (
                                        string? result
                                      )
                                       (
                                        _substring result "b" (
                                          + "b" 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? result
                                      )
                                       (
                                        hash-table-ref result "b"
                                      )
                                    )
                                     (
                                      else (
                                        list-ref result "b"
                                      )
                                    )
                                  )
                                )
                              )
                               " "
                            )
                             (
                              to-str-space (
                                cond (
                                  (
                                    string? result
                                  )
                                   (
                                    _substring result "c" (
                                      + "c" 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? result
                                  )
                                   (
                                    hash-table-ref result "c"
                                  )
                                )
                                 (
                                  else (
                                    list-ref result "c"
                                  )
                                )
                              )
                            )
                          )
                           (
                            to-str (
                              string-append (
                                string-append (
                                  string-append (
                                    string-append (
                                      to-str-space (
                                        cond (
                                          (
                                            string? result
                                          )
                                           (
                                            _substring result "a" (
                                              + "a" 1
                                            )
                                          )
                                        )
                                         (
                                          (
                                            hash-table? result
                                          )
                                           (
                                            hash-table-ref result "a"
                                          )
                                        )
                                         (
                                          else (
                                            list-ref result "a"
                                          )
                                        )
                                      )
                                    )
                                     " "
                                  )
                                   (
                                    to-str-space (
                                      cond (
                                        (
                                          string? result
                                        )
                                         (
                                          _substring result "b" (
                                            + "b" 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? result
                                        )
                                         (
                                          hash-table-ref result "b"
                                        )
                                      )
                                       (
                                        else (
                                          list-ref result "b"
                                        )
                                      )
                                    )
                                  )
                                )
                                 " "
                              )
                               (
                                to-str-space (
                                  cond (
                                    (
                                      string? result
                                    )
                                     (
                                      _substring result "c" (
                                        + "c" 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? result
                                    )
                                     (
                                      hash-table-ref result "c"
                                    )
                                  )
                                   (
                                    else (
                                      list-ref result "c"
                                    )
                                  )
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
          end18 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur19 (
              quotient (
                * (
                  - end18 start17
                )
                 1000000
              )
               jps20
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur19
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
