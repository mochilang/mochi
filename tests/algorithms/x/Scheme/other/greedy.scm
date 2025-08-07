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
      start22 (
        current-jiffy
      )
    )
     (
      jps25 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        get_value t
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              hash-table-ref t "value"
            )
          )
        )
      )
    )
     (
      define (
        get_weight t
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            ret2 (
              hash-table-ref t "weight"
            )
          )
        )
      )
    )
     (
      define (
        get_name t
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            ret3 (
              hash-table-ref t "name"
            )
          )
        )
      )
    )
     (
      define (
        value_weight t
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            ret4 (
              _div (
                hash-table-ref t "value"
              )
               (
                hash-table-ref t "weight"
              )
            )
          )
        )
      )
    )
     (
      define (
        build_menu names values weights
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                menu (
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
                                  and (
                                    and (
                                      < i (
                                        _len values
                                      )
                                    )
                                     (
                                      < i (
                                        _len names
                                      )
                                    )
                                  )
                                   (
                                    < i (
                                      _len weights
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! menu (
                                      append menu (
                                        _list (
                                          alist->hash-table (
                                            _list (
                                              cons "name" (
                                                list-ref-safe names i
                                              )
                                            )
                                             (
                                              cons "value" (
                                                list-ref-safe values i
                                              )
                                            )
                                             (
                                              cons "weight" (
                                                list-ref-safe weights i
                                              )
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
                    ret5 menu
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
        sort_desc items key_func
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                arr (
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
                                  < i (
                                    _len items
                                  )
                                )
                                 (
                                  begin (
                                    set! arr (
                                      append arr (
                                        _list (
                                          list-ref-safe items i
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
                                    loop9
                                  )
                                )
                                 '(
                                  
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
                    let (
                      (
                        j 1
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
                                      < j (
                                        _len arr
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            key_item (
                                              list-ref-safe arr j
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                key_val (
                                                  key_func key_item
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    k (
                                                      - j 1
                                                    )
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
                                                                  and (
                                                                    >= k 0
                                                                  )
                                                                   (
                                                                    < (
                                                                      key_func (
                                                                        list-ref-safe arr k
                                                                      )
                                                                    )
                                                                     key_val
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    list-set! arr (
                                                                      + k 1
                                                                    )
                                                                     (
                                                                      list-ref-safe arr k
                                                                    )
                                                                  )
                                                                   (
                                                                    set! k (
                                                                      - k 1
                                                                    )
                                                                  )
                                                                   (
                                                                    loop13
                                                                  )
                                                                )
                                                                 '(
                                                                  
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
                                                    list-set! arr (
                                                      + k 1
                                                    )
                                                     key_item
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
                                        )
                                      )
                                       (
                                        loop11
                                      )
                                    )
                                     '(
                                      
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
                        ret8 arr
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
        greedy items max_cost key_func
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            let (
              (
                items_copy (
                  sort_desc items key_func
                )
              )
            )
             (
              begin (
                let (
                  (
                    result (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        total_value 0.0
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            total_cost 0.0
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
                                                _len items_copy
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    it (
                                                      cond (
                                                        (
                                                          string? items_copy
                                                        )
                                                         (
                                                          _substring items_copy i (
                                                            + i 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? items_copy
                                                        )
                                                         (
                                                          hash-table-ref items_copy i
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref-safe items_copy i
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        w (
                                                          get_weight it
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          _le (
                                                            _add total_cost w
                                                          )
                                                           max_cost
                                                        )
                                                         (
                                                          begin (
                                                            set! result (
                                                              append result (
                                                                _list it
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! total_cost (
                                                              _add total_cost w
                                                            )
                                                          )
                                                           (
                                                            set! total_value (
                                                              _add total_value (
                                                                get_value it
                                                              )
                                                            )
                                                          )
                                                        )
                                                         '(
                                                          
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
                                ret15 (
                                  alist->hash-table (
                                    _list (
                                      cons "items" result
                                    )
                                     (
                                      cons "total_value" total_value
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
        thing_to_string t
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            ret18 (
              string-append (
                string-append (
                  string-append (
                    string-append (
                      string-append (
                        string-append "Thing(" (
                          hash-table-ref t "name"
                        )
                      )
                       ", "
                    )
                     (
                      to-str-space (
                        hash-table-ref t "value"
                      )
                    )
                  )
                   ", "
                )
                 (
                  to-str-space (
                    hash-table-ref t "weight"
                  )
                )
              )
               ")"
            )
          )
        )
      )
    )
     (
      define (
        list_to_string ts
      )
       (
        call/cc (
          lambda (
            ret19
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
                        break21
                      )
                       (
                        letrec (
                          (
                            loop20 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len ts
                                  )
                                )
                                 (
                                  begin (
                                    set! s (
                                      string-append s (
                                        thing_to_string (
                                          list-ref-safe ts i
                                        )
                                      )
                                    )
                                  )
                                   (
                                    if (
                                      < i (
                                        - (
                                          _len ts
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
                                     '(
                                      
                                    )
                                  )
                                   (
                                    set! i (
                                      + i 1
                                    )
                                  )
                                   (
                                    loop20
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop20
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
                    ret19 s
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
          food (
            _list "Burger" "Pizza" "Coca Cola" "Rice" "Sambhar" "Chicken" "Fries" "Milk"
          )
        )
      )
       (
        begin (
          let (
            (
              value (
                _list 80.0 100.0 60.0 70.0 50.0 110.0 90.0 60.0
              )
            )
          )
           (
            begin (
              let (
                (
                  weight (
                    _list 40.0 60.0 40.0 70.0 100.0 85.0 55.0 70.0
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      foods (
                        build_menu food value weight
                      )
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            list_to_string foods
                          )
                        )
                         (
                          list_to_string foods
                        )
                         (
                          to-str (
                            list_to_string foods
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
                          res (
                            greedy foods 500.0 get_value
                          )
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? (
                                list_to_string (
                                  hash-table-ref res "items"
                                )
                              )
                            )
                             (
                              list_to_string (
                                hash-table-ref res "items"
                              )
                            )
                             (
                              to-str (
                                list_to_string (
                                  hash-table-ref res "items"
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
                                  hash-table-ref res "total_value"
                                )
                              )
                            )
                             (
                              to-str-space (
                                hash-table-ref res "total_value"
                              )
                            )
                             (
                              to-str (
                                to-str-space (
                                  hash-table-ref res "total_value"
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
     (
      let (
        (
          end23 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur24 (
              quotient (
                * (
                  - end23 start22
                )
                 1000000
              )
               jps25
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur24
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
