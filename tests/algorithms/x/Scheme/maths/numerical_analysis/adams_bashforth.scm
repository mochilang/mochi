;; Generated on 2025-08-07 10:06 +0700
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
      start26 (
        current-jiffy
      )
    )
     (
      jps29 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        abs_float x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            if (
              < x 0.0
            )
             (
              begin (
                ret1 (
                  - x
                )
              )
            )
             (
              begin (
                ret1 x
              )
            )
          )
        )
      )
    )
     (
      define (
        validate_inputs x_initials step_size x_final
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                >= (
                  list-ref x_initials (
                    - (
                      _len x_initials
                    )
                     1
                  )
                )
                 x_final
              )
               (
                begin (
                  panic "The final value of x must be greater than the initial values of x."
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                <= step_size 0.0
              )
               (
                begin (
                  panic "Step size must be positive."
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
                      break4
                    )
                     (
                      letrec (
                        (
                          loop3 (
                            lambda (
                              
                            )
                             (
                              if (
                                < i (
                                  - (
                                    _len x_initials
                                  )
                                   1
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      diff (
                                        - (
                                          list-ref x_initials (
                                            + i 1
                                          )
                                        )
                                         (
                                          list-ref x_initials i
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      if (
                                        _gt (
                                          abs_float (
                                            - diff step_size
                                          )
                                        )
                                         1e-10
                                      )
                                       (
                                        begin (
                                          panic "x-values must be equally spaced according to step size."
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
                                 (
                                  loop3
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
                        loop3
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
        list_to_string xs
      )
       (
        call/cc (
          lambda (
            ret5
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
                                  < i (
                                    _len xs
                                  )
                                )
                                 (
                                  begin (
                                    set! s (
                                      string-append s (
                                        to-str-space (
                                          list-ref xs i
                                        )
                                      )
                                    )
                                  )
                                   (
                                    if (
                                      < (
                                        + i 1
                                      )
                                       (
                                        _len xs
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
                                    loop6
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
                          loop6
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
                    ret5 s
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
        adams_bashforth_step2 f x_initials y_initials step_size x_final
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            begin (
              validate_inputs x_initials step_size x_final
            )
             (
              if (
                or (
                  not (
                    equal? (
                      _len x_initials
                    )
                     2
                  )
                )
                 (
                  not (
                    equal? (
                      _len y_initials
                    )
                     2
                  )
                )
              )
               (
                begin (
                  panic "Insufficient initial points information."
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
                  x0 (
                    list-ref x_initials 0
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      x1 (
                        list-ref x_initials 1
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          y (
                            _list
                          )
                        )
                      )
                       (
                        begin (
                          set! y (
                            append y (
                              _list (
                                list-ref y_initials 0
                              )
                            )
                          )
                        )
                         (
                          set! y (
                            append y (
                              _list (
                                list-ref y_initials 1
                              )
                            )
                          )
                        )
                         (
                          let (
                            (
                              n (
                                let (
                                  (
                                    v9 (
                                      _div (
                                        - x_final x1
                                      )
                                       step_size
                                    )
                                  )
                                )
                                 (
                                  cond (
                                    (
                                      string? v9
                                    )
                                     (
                                      inexact->exact (
                                        floor (
                                          string->number v9
                                        )
                                      )
                                    )
                                  )
                                   (
                                    (
                                      boolean? v9
                                    )
                                     (
                                      if v9 1 0
                                    )
                                  )
                                   (
                                    else (
                                      inexact->exact (
                                        floor v9
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
                                  i 0
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
                                                < i n
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      term (
                                                        - (
                                                          * 3.0 (
                                                            f x1 (
                                                              list-ref y (
                                                                + i 1
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          f x0 (
                                                            list-ref y i
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          y_next (
                                                            _add (
                                                              list-ref y (
                                                                + i 1
                                                              )
                                                            )
                                                             (
                                                              * (
                                                                _div step_size 2.0
                                                              )
                                                               term
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          set! y (
                                                            append y (
                                                              _list y_next
                                                            )
                                                          )
                                                        )
                                                         (
                                                          set! x0 x1
                                                        )
                                                         (
                                                          set! x1 (
                                                            + x1 step_size
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
                                  ret8 y
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
        adams_bashforth_step3 f x_initials y_initials step_size x_final
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            begin (
              validate_inputs x_initials step_size x_final
            )
             (
              if (
                or (
                  not (
                    equal? (
                      _len x_initials
                    )
                     3
                  )
                )
                 (
                  not (
                    equal? (
                      _len y_initials
                    )
                     3
                  )
                )
              )
               (
                begin (
                  panic "Insufficient initial points information."
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
                  x0 (
                    list-ref x_initials 0
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      x1 (
                        list-ref x_initials 1
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          x2 (
                            list-ref x_initials 2
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              y (
                                _list
                              )
                            )
                          )
                           (
                            begin (
                              set! y (
                                append y (
                                  _list (
                                    list-ref y_initials 0
                                  )
                                )
                              )
                            )
                             (
                              set! y (
                                append y (
                                  _list (
                                    list-ref y_initials 1
                                  )
                                )
                              )
                            )
                             (
                              set! y (
                                append y (
                                  _list (
                                    list-ref y_initials 2
                                  )
                                )
                              )
                            )
                             (
                              let (
                                (
                                  n (
                                    let (
                                      (
                                        v13 (
                                          _div (
                                            - x_final x2
                                          )
                                           step_size
                                        )
                                      )
                                    )
                                     (
                                      cond (
                                        (
                                          string? v13
                                        )
                                         (
                                          inexact->exact (
                                            floor (
                                              string->number v13
                                            )
                                          )
                                        )
                                      )
                                       (
                                        (
                                          boolean? v13
                                        )
                                         (
                                          if v13 1 0
                                        )
                                      )
                                       (
                                        else (
                                          inexact->exact (
                                            floor v13
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
                                                    <= i n
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          term (
                                                            _add (
                                                              - (
                                                                * 23.0 (
                                                                  f x2 (
                                                                    list-ref y (
                                                                      + i 2
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                * 16.0 (
                                                                  f x1 (
                                                                    list-ref y (
                                                                      + i 1
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              * 5.0 (
                                                                f x0 (
                                                                  list-ref y i
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
                                                              y_next (
                                                                _add (
                                                                  list-ref y (
                                                                    + i 2
                                                                  )
                                                                )
                                                                 (
                                                                  * (
                                                                    _div step_size 12.0
                                                                  )
                                                                   term
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              set! y (
                                                                append y (
                                                                  _list y_next
                                                                )
                                                              )
                                                            )
                                                             (
                                                              set! x0 x1
                                                            )
                                                             (
                                                              set! x1 x2
                                                            )
                                                             (
                                                              set! x2 (
                                                                + x2 step_size
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
                                      ret12 y
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
        adams_bashforth_step4 f x_initials y_initials step_size x_final
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            begin (
              validate_inputs x_initials step_size x_final
            )
             (
              if (
                or (
                  not (
                    equal? (
                      _len x_initials
                    )
                     4
                  )
                )
                 (
                  not (
                    equal? (
                      _len y_initials
                    )
                     4
                  )
                )
              )
               (
                begin (
                  panic "Insufficient initial points information."
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
                  x0 (
                    list-ref x_initials 0
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      x1 (
                        list-ref x_initials 1
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          x2 (
                            list-ref x_initials 2
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              x3 (
                                list-ref x_initials 3
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  y (
                                    _list
                                  )
                                )
                              )
                               (
                                begin (
                                  set! y (
                                    append y (
                                      _list (
                                        list-ref y_initials 0
                                      )
                                    )
                                  )
                                )
                                 (
                                  set! y (
                                    append y (
                                      _list (
                                        list-ref y_initials 1
                                      )
                                    )
                                  )
                                )
                                 (
                                  set! y (
                                    append y (
                                      _list (
                                        list-ref y_initials 2
                                      )
                                    )
                                  )
                                )
                                 (
                                  set! y (
                                    append y (
                                      _list (
                                        list-ref y_initials 3
                                      )
                                    )
                                  )
                                )
                                 (
                                  let (
                                    (
                                      n (
                                        let (
                                          (
                                            v17 (
                                              _div (
                                                - x_final x3
                                              )
                                               step_size
                                            )
                                          )
                                        )
                                         (
                                          cond (
                                            (
                                              string? v17
                                            )
                                             (
                                              inexact->exact (
                                                floor (
                                                  string->number v17
                                                )
                                              )
                                            )
                                          )
                                           (
                                            (
                                              boolean? v17
                                            )
                                             (
                                              if v17 1 0
                                            )
                                          )
                                           (
                                            else (
                                              inexact->exact (
                                                floor v17
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
                                          i 0
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
                                                        < i n
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              term (
                                                                - (
                                                                  _add (
                                                                    - (
                                                                      * 55.0 (
                                                                        f x3 (
                                                                          list-ref y (
                                                                            + i 3
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      * 59.0 (
                                                                        f x2 (
                                                                          list-ref y (
                                                                            + i 2
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    * 37.0 (
                                                                      f x1 (
                                                                        list-ref y (
                                                                          + i 1
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  * 9.0 (
                                                                    f x0 (
                                                                      list-ref y i
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
                                                                  y_next (
                                                                    _add (
                                                                      list-ref y (
                                                                        + i 3
                                                                      )
                                                                    )
                                                                     (
                                                                      * (
                                                                        _div step_size 24.0
                                                                      )
                                                                       term
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  set! y (
                                                                    append y (
                                                                      _list y_next
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  set! x0 x1
                                                                )
                                                                 (
                                                                  set! x1 x2
                                                                )
                                                                 (
                                                                  set! x2 x3
                                                                )
                                                                 (
                                                                  set! x3 (
                                                                    + x3 step_size
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
                                          ret16 y
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
        adams_bashforth_step5 f x_initials y_initials step_size x_final
      )
       (
        call/cc (
          lambda (
            ret20
          )
           (
            begin (
              validate_inputs x_initials step_size x_final
            )
             (
              if (
                or (
                  not (
                    equal? (
                      _len x_initials
                    )
                     5
                  )
                )
                 (
                  not (
                    equal? (
                      _len y_initials
                    )
                     5
                  )
                )
              )
               (
                begin (
                  panic "Insufficient initial points information."
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
                  x0 (
                    list-ref x_initials 0
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      x1 (
                        list-ref x_initials 1
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          x2 (
                            list-ref x_initials 2
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              x3 (
                                list-ref x_initials 3
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  x4 (
                                    list-ref x_initials 4
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      y (
                                        _list
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      set! y (
                                        append y (
                                          _list (
                                            list-ref y_initials 0
                                          )
                                        )
                                      )
                                    )
                                     (
                                      set! y (
                                        append y (
                                          _list (
                                            list-ref y_initials 1
                                          )
                                        )
                                      )
                                    )
                                     (
                                      set! y (
                                        append y (
                                          _list (
                                            list-ref y_initials 2
                                          )
                                        )
                                      )
                                    )
                                     (
                                      set! y (
                                        append y (
                                          _list (
                                            list-ref y_initials 3
                                          )
                                        )
                                      )
                                    )
                                     (
                                      set! y (
                                        append y (
                                          _list (
                                            list-ref y_initials 4
                                          )
                                        )
                                      )
                                    )
                                     (
                                      let (
                                        (
                                          n (
                                            let (
                                              (
                                                v21 (
                                                  _div (
                                                    - x_final x4
                                                  )
                                                   step_size
                                                )
                                              )
                                            )
                                             (
                                              cond (
                                                (
                                                  string? v21
                                                )
                                                 (
                                                  inexact->exact (
                                                    floor (
                                                      string->number v21
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  boolean? v21
                                                )
                                                 (
                                                  if v21 1 0
                                                )
                                              )
                                               (
                                                else (
                                                  inexact->exact (
                                                    floor v21
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
                                              i 0
                                            )
                                          )
                                           (
                                            begin (
                                              call/cc (
                                                lambda (
                                                  break23
                                                )
                                                 (
                                                  letrec (
                                                    (
                                                      loop22 (
                                                        lambda (
                                                          
                                                        )
                                                         (
                                                          if (
                                                            <= i n
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  term (
                                                                    _add (
                                                                      - (
                                                                        - (
                                                                          - (
                                                                            * 1901.0 (
                                                                              f x4 (
                                                                                list-ref y (
                                                                                  + i 4
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            * 2774.0 (
                                                                              f x3 (
                                                                                list-ref y (
                                                                                  + i 3
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          * 2616.0 (
                                                                            f x2 (
                                                                              list-ref y (
                                                                                + i 2
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        * 1274.0 (
                                                                          f x1 (
                                                                            list-ref y (
                                                                              + i 1
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      * 251.0 (
                                                                        f x0 (
                                                                          list-ref y i
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
                                                                      y_next (
                                                                        _add (
                                                                          list-ref y (
                                                                            + i 4
                                                                          )
                                                                        )
                                                                         (
                                                                          * (
                                                                            _div step_size 720.0
                                                                          )
                                                                           term
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      set! y (
                                                                        append y (
                                                                          _list y_next
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      set! x0 x1
                                                                    )
                                                                     (
                                                                      set! x1 x2
                                                                    )
                                                                     (
                                                                      set! x2 x3
                                                                    )
                                                                     (
                                                                      set! x3 x4
                                                                    )
                                                                     (
                                                                      set! x4 (
                                                                        + x4 step_size
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
                                                              loop22
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
                                                    loop22
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              ret20 y
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
        f_x x y
      )
       (
        call/cc (
          lambda (
            ret24
          )
           (
            ret24 x
          )
        )
      )
    )
     (
      define (
        f_xy x y
      )
       (
        call/cc (
          lambda (
            ret25
          )
           (
            ret25 (
              + x y
            )
          )
        )
      )
    )
     (
      let (
        (
          y2 (
            adams_bashforth_step2 f_x (
              _list 0.0 0.2
            )
             (
              _list 0.0 0.0
            )
             0.2 1.0
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                list_to_string y2
              )
            )
             (
              list_to_string y2
            )
             (
              to-str (
                list_to_string y2
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
              y3 (
                adams_bashforth_step3 f_xy (
                  _list 0.0 0.2 0.4
                )
                 (
                  _list 0.0 0.0 0.04
                )
                 0.2 1.0
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    to-str-space (
                      cond (
                        (
                          string? y3
                        )
                         (
                          _substring y3 3 (
                            + 3 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? y3
                        )
                         (
                          hash-table-ref y3 3
                        )
                      )
                       (
                        else (
                          list-ref y3 3
                        )
                      )
                    )
                  )
                )
                 (
                  to-str-space (
                    cond (
                      (
                        string? y3
                      )
                       (
                        _substring y3 3 (
                          + 3 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? y3
                      )
                       (
                        hash-table-ref y3 3
                      )
                    )
                     (
                      else (
                        list-ref y3 3
                      )
                    )
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      cond (
                        (
                          string? y3
                        )
                         (
                          _substring y3 3 (
                            + 3 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? y3
                        )
                         (
                          hash-table-ref y3 3
                        )
                      )
                       (
                        else (
                          list-ref y3 3
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
             (
              let (
                (
                  y4 (
                    adams_bashforth_step4 f_xy (
                      _list 0.0 0.2 0.4 0.6
                    )
                     (
                      _list 0.0 0.0 0.04 0.128
                    )
                     0.2 1.0
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        to-str-space (
                          cond (
                            (
                              string? y4
                            )
                             (
                              _substring y4 4 (
                                + 4 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? y4
                            )
                             (
                              hash-table-ref y4 4
                            )
                          )
                           (
                            else (
                              list-ref y4 4
                            )
                          )
                        )
                      )
                    )
                     (
                      to-str-space (
                        cond (
                          (
                            string? y4
                          )
                           (
                            _substring y4 4 (
                              + 4 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? y4
                          )
                           (
                            hash-table-ref y4 4
                          )
                        )
                         (
                          else (
                            list-ref y4 4
                          )
                        )
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          cond (
                            (
                              string? y4
                            )
                             (
                              _substring y4 4 (
                                + 4 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? y4
                            )
                             (
                              hash-table-ref y4 4
                            )
                          )
                           (
                            else (
                              list-ref y4 4
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
                 (
                  _display (
                    if (
                      string? (
                        to-str-space (
                          cond (
                            (
                              string? y4
                            )
                             (
                              _substring y4 5 (
                                + 5 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? y4
                            )
                             (
                              hash-table-ref y4 5
                            )
                          )
                           (
                            else (
                              list-ref y4 5
                            )
                          )
                        )
                      )
                    )
                     (
                      to-str-space (
                        cond (
                          (
                            string? y4
                          )
                           (
                            _substring y4 5 (
                              + 5 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? y4
                          )
                           (
                            hash-table-ref y4 5
                          )
                        )
                         (
                          else (
                            list-ref y4 5
                          )
                        )
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          cond (
                            (
                              string? y4
                            )
                             (
                              _substring y4 5 (
                                + 5 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? y4
                            )
                             (
                              hash-table-ref y4 5
                            )
                          )
                           (
                            else (
                              list-ref y4 5
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
                 (
                  let (
                    (
                      y5 (
                        adams_bashforth_step5 f_xy (
                          _list 0.0 0.2 0.4 0.6 0.8
                        )
                         (
                          _list 0.0 0.0214 0.0214 0.22211 0.42536
                        )
                         0.2 1.0
                      )
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            to-str-space (
                              cond (
                                (
                                  string? y5
                                )
                                 (
                                  _substring y5 (
                                    - (
                                      _len y5
                                    )
                                     1
                                  )
                                   (
                                    + (
                                      - (
                                        _len y5
                                      )
                                       1
                                    )
                                     1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? y5
                                )
                                 (
                                  hash-table-ref y5 (
                                    - (
                                      _len y5
                                    )
                                     1
                                  )
                                )
                              )
                               (
                                else (
                                  list-ref y5 (
                                    - (
                                      _len y5
                                    )
                                     1
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          to-str-space (
                            cond (
                              (
                                string? y5
                              )
                               (
                                _substring y5 (
                                  - (
                                    _len y5
                                  )
                                   1
                                )
                                 (
                                  + (
                                    - (
                                      _len y5
                                    )
                                     1
                                  )
                                   1
                                )
                              )
                            )
                             (
                              (
                                hash-table? y5
                              )
                               (
                                hash-table-ref y5 (
                                  - (
                                    _len y5
                                  )
                                   1
                                )
                              )
                            )
                             (
                              else (
                                list-ref y5 (
                                  - (
                                    _len y5
                                  )
                                   1
                                )
                              )
                            )
                          )
                        )
                         (
                          to-str (
                            to-str-space (
                              cond (
                                (
                                  string? y5
                                )
                                 (
                                  _substring y5 (
                                    - (
                                      _len y5
                                    )
                                     1
                                  )
                                   (
                                    + (
                                      - (
                                        _len y5
                                      )
                                       1
                                    )
                                     1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? y5
                                )
                                 (
                                  hash-table-ref y5 (
                                    - (
                                      _len y5
                                    )
                                     1
                                  )
                                )
                              )
                               (
                                else (
                                  list-ref y5 (
                                    - (
                                      _len y5
                                    )
                                     1
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
    )
     (
      let (
        (
          end27 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur28 (
              quotient (
                * (
                  - end27 start26
                )
                 1000000
              )
               jps29
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur28
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
