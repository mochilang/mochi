;; Generated on 2025-08-06 23:57 +0700
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
      define (
        parent_index child_idx
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                > child_idx 0
              )
               (
                begin (
                  ret1 (
                    _div (
                      - child_idx 1
                    )
                     2
                  )
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret1 (
                - 1
              )
            )
          )
        )
      )
    )
     (
      define (
        left_child_idx parent_idx
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            ret2 (
              + (
                * 2 parent_idx
              )
               1
            )
          )
        )
      )
    )
     (
      define (
        right_child_idx parent_idx
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            ret3 (
              + (
                * 2 parent_idx
              )
               2
            )
          )
        )
      )
    )
     (
      define (
        max_heapify h heap_size index
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                largest index
              )
            )
             (
              begin (
                let (
                  (
                    left (
                      left_child_idx index
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        right (
                          right_child_idx index
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          and (
                            _lt left heap_size
                          )
                           (
                            > (
                              list-ref h left
                            )
                             (
                              list-ref h largest
                            )
                          )
                        )
                         (
                          begin (
                            set! largest left
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
                            _lt right heap_size
                          )
                           (
                            > (
                              list-ref h right
                            )
                             (
                              list-ref h largest
                            )
                          )
                        )
                         (
                          begin (
                            set! largest right
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        if (
                          not (
                            equal? largest index
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                temp (
                                  list-ref h index
                                )
                              )
                            )
                             (
                              begin (
                                list-set! h index (
                                  list-ref h largest
                                )
                              )
                               (
                                list-set! h largest temp
                              )
                               (
                                max_heapify h heap_size largest
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
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        build_max_heap h
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                heap_size (
                  _len h
                )
              )
            )
             (
              begin (
                let (
                  (
                    i (
                      - (
                        _div heap_size 2
                      )
                       1
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
                                
                              )
                               (
                                if (
                                  >= i 0
                                )
                                 (
                                  begin (
                                    max_heapify h heap_size i
                                  )
                                   (
                                    set! i (
                                      - i 1
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
                    ret5 heap_size
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
        extract_max h heap_size
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                max_value (
                  list-ref h 0
                )
              )
            )
             (
              begin (
                list-set! h 0 (
                  list-ref h (
                    - heap_size 1
                  )
                )
              )
               (
                max_heapify h (
                  - heap_size 1
                )
                 0
              )
               (
                ret8 max_value
              )
            )
          )
        )
      )
    )
     (
      define (
        insert h heap_size value
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            begin (
              if (
                < heap_size (
                  _len h
                )
              )
               (
                begin (
                  list-set! h heap_size value
                )
              )
               (
                begin (
                  set! h (
                    append h (
                      _list value
                    )
                  )
                )
              )
            )
             (
              set! heap_size (
                + heap_size 1
              )
            )
             (
              let (
                (
                  idx (
                    _div (
                      - heap_size 1
                    )
                     2
                  )
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
                                >= idx 0
                              )
                               (
                                begin (
                                  max_heapify h heap_size idx
                                )
                                 (
                                  set! idx (
                                    _div (
                                      - idx 1
                                    )
                                     2
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
                  ret9 heap_size
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        heap_sort h heap_size
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                size heap_size
              )
            )
             (
              begin (
                let (
                  (
                    j (
                      - size 1
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
                                  > j 0
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        temp (
                                          list-ref h 0
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        list-set! h 0 (
                                          list-ref h j
                                        )
                                      )
                                       (
                                        list-set! h j temp
                                      )
                                       (
                                        set! size (
                                          - size 1
                                        )
                                      )
                                       (
                                        max_heapify h size 0
                                      )
                                       (
                                        set! j (
                                          - j 1
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
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        heap_to_string h heap_size
      )
       (
        call/cc (
          lambda (
            ret15
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
                                  < i heap_size
                                )
                                 (
                                  begin (
                                    set! s (
                                      string-append s (
                                        to-str-space (
                                          list-ref h i
                                        )
                                      )
                                    )
                                  )
                                   (
                                    if (
                                      < i (
                                        - heap_size 1
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
                    set! s (
                      string-append s "]"
                    )
                  )
                   (
                    ret15 s
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
          heap (
            _list 103.0 9.0 1.0 7.0 11.0 15.0 25.0 201.0 209.0 107.0 5.0
          )
        )
      )
       (
        begin (
          let (
            (
              size (
                build_max_heap heap
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    heap_to_string heap size
                  )
                )
                 (
                  heap_to_string heap size
                )
                 (
                  to-str (
                    heap_to_string heap size
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
                  m (
                    extract_max heap size
                  )
                )
              )
               (
                begin (
                  set! size (
                    - size 1
                  )
                )
                 (
                  _display (
                    if (
                      string? (
                        to-str-space m
                      )
                    )
                     (
                      to-str-space m
                    )
                     (
                      to-str (
                        to-str-space m
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
                        heap_to_string heap size
                      )
                    )
                     (
                      heap_to_string heap size
                    )
                     (
                      to-str (
                        heap_to_string heap size
                      )
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  set! size (
                    insert heap size 100.0
                  )
                )
                 (
                  _display (
                    if (
                      string? (
                        heap_to_string heap size
                      )
                    )
                     (
                      heap_to_string heap size
                    )
                     (
                      to-str (
                        heap_to_string heap size
                      )
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  heap_sort heap size
                )
                 (
                  _display (
                    if (
                      string? (
                        heap_to_string heap size
                      )
                    )
                     (
                      heap_to_string heap size
                    )
                     (
                      to-str (
                        heap_to_string heap size
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
