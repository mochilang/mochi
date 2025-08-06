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
      start7 (
        current-jiffy
      )
    )
     (
      jps10 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        inorder nodes index acc
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                equal? index (
                  - 0 1
                )
              )
               (
                begin (
                  ret1 acc
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
                  node (
                    list-ref nodes index
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      res (
                        inorder nodes (
                          hash-table-ref node "left"
                        )
                         acc
                      )
                    )
                  )
                   (
                    begin (
                      set! res (
                        append res (
                          _list (
                            hash-table-ref node "data"
                          )
                        )
                      )
                    )
                     (
                      set! res (
                        inorder nodes (
                          hash-table-ref node "right"
                        )
                         res
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
    )
     (
      define (
        size nodes index
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                equal? index (
                  - 0 1
                )
              )
               (
                begin (
                  ret2 0
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
                  node (
                    list-ref nodes index
                  )
                )
              )
               (
                begin (
                  ret2 (
                    _add (
                      _add 1 (
                        size nodes (
                          hash-table-ref node "left"
                        )
                      )
                    )
                     (
                      size nodes (
                        hash-table-ref node "right"
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
        depth nodes index
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            begin (
              if (
                equal? index (
                  - 0 1
                )
              )
               (
                begin (
                  ret3 0
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
                  node (
                    list-ref nodes index
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      left_depth (
                        depth nodes (
                          hash-table-ref node "left"
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          right_depth (
                            depth nodes (
                              hash-table-ref node "right"
                            )
                          )
                        )
                      )
                       (
                        begin (
                          if (
                            _gt left_depth right_depth
                          )
                           (
                            begin (
                              ret3 (
                                _add left_depth 1
                              )
                            )
                          )
                           (
                            quote (
                              
                            )
                          )
                        )
                         (
                          ret3 (
                            _add right_depth 1
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
        is_full nodes index
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                equal? index (
                  - 0 1
                )
              )
               (
                begin (
                  ret4 #t
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
                  node (
                    list-ref nodes index
                  )
                )
              )
               (
                begin (
                  if (
                    and (
                      equal? (
                        hash-table-ref node "left"
                      )
                       (
                        - 0 1
                      )
                    )
                     (
                      equal? (
                        hash-table-ref node "right"
                      )
                       (
                        - 0 1
                      )
                    )
                  )
                   (
                    begin (
                      ret4 #t
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
                      not (
                        equal? (
                          hash-table-ref node "left"
                        )
                         (
                          - 0 1
                        )
                      )
                    )
                     (
                      not (
                        equal? (
                          hash-table-ref node "right"
                        )
                         (
                          - 0 1
                        )
                      )
                    )
                  )
                   (
                    begin (
                      ret4 (
                        and (
                          is_full nodes (
                            hash-table-ref node "left"
                          )
                        )
                         (
                          is_full nodes (
                            hash-table-ref node "right"
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
                  ret4 #f
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        small_tree
      )
       (
        call/cc (
          lambda (
            ret5
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
                set! arr (
                  append arr (
                    _list (
                      alist->hash-table (
                        _list (
                          cons "data" 2
                        )
                         (
                          cons "left" 1
                        )
                         (
                          cons "right" 2
                        )
                      )
                    )
                  )
                )
              )
               (
                set! arr (
                  append arr (
                    _list (
                      alist->hash-table (
                        _list (
                          cons "data" 1
                        )
                         (
                          cons "left" (
                            - 0 1
                          )
                        )
                         (
                          cons "right" (
                            - 0 1
                          )
                        )
                      )
                    )
                  )
                )
              )
               (
                set! arr (
                  append arr (
                    _list (
                      alist->hash-table (
                        _list (
                          cons "data" 3
                        )
                         (
                          cons "left" (
                            - 0 1
                          )
                        )
                         (
                          cons "right" (
                            - 0 1
                          )
                        )
                      )
                    )
                  )
                )
              )
               (
                ret5 arr
              )
            )
          )
        )
      )
    )
     (
      define (
        medium_tree
      )
       (
        call/cc (
          lambda (
            ret6
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
                set! arr (
                  append arr (
                    _list (
                      alist->hash-table (
                        _list (
                          cons "data" 4
                        )
                         (
                          cons "left" 1
                        )
                         (
                          cons "right" 4
                        )
                      )
                    )
                  )
                )
              )
               (
                set! arr (
                  append arr (
                    _list (
                      alist->hash-table (
                        _list (
                          cons "data" 2
                        )
                         (
                          cons "left" 2
                        )
                         (
                          cons "right" 3
                        )
                      )
                    )
                  )
                )
              )
               (
                set! arr (
                  append arr (
                    _list (
                      alist->hash-table (
                        _list (
                          cons "data" 1
                        )
                         (
                          cons "left" (
                            - 0 1
                          )
                        )
                         (
                          cons "right" (
                            - 0 1
                          )
                        )
                      )
                    )
                  )
                )
              )
               (
                set! arr (
                  append arr (
                    _list (
                      alist->hash-table (
                        _list (
                          cons "data" 3
                        )
                         (
                          cons "left" (
                            - 0 1
                          )
                        )
                         (
                          cons "right" (
                            - 0 1
                          )
                        )
                      )
                    )
                  )
                )
              )
               (
                set! arr (
                  append arr (
                    _list (
                      alist->hash-table (
                        _list (
                          cons "data" 5
                        )
                         (
                          cons "left" (
                            - 0 1
                          )
                        )
                         (
                          cons "right" 5
                        )
                      )
                    )
                  )
                )
              )
               (
                set! arr (
                  append arr (
                    _list (
                      alist->hash-table (
                        _list (
                          cons "data" 6
                        )
                         (
                          cons "left" (
                            - 0 1
                          )
                        )
                         (
                          cons "right" 6
                        )
                      )
                    )
                  )
                )
              )
               (
                set! arr (
                  append arr (
                    _list (
                      alist->hash-table (
                        _list (
                          cons "data" 7
                        )
                         (
                          cons "left" (
                            - 0 1
                          )
                        )
                         (
                          cons "right" (
                            - 0 1
                          )
                        )
                      )
                    )
                  )
                )
              )
               (
                ret6 arr
              )
            )
          )
        )
      )
    )
     (
      let (
        (
          small (
            small_tree
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                size small 0
              )
            )
             (
              size small 0
            )
             (
              to-str (
                size small 0
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
                inorder small 0 (
                  _list
                )
              )
            )
             (
              inorder small 0 (
                _list
              )
            )
             (
              to-str (
                inorder small 0 (
                  _list
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
                depth small 0
              )
            )
             (
              depth small 0
            )
             (
              to-str (
                depth small 0
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
                is_full small 0
              )
            )
             (
              is_full small 0
            )
             (
              to-str (
                is_full small 0
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
              medium (
                medium_tree
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    size medium 0
                  )
                )
                 (
                  size medium 0
                )
                 (
                  to-str (
                    size medium 0
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
                    inorder medium 0 (
                      _list
                    )
                  )
                )
                 (
                  inorder medium 0 (
                    _list
                  )
                )
                 (
                  to-str (
                    inorder medium 0 (
                      _list
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
                    depth medium 0
                  )
                )
                 (
                  depth medium 0
                )
                 (
                  to-str (
                    depth medium 0
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
                    is_full medium 0
                  )
                )
                 (
                  is_full medium 0
                )
                 (
                  to-str (
                    is_full medium 0
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
      let (
        (
          end8 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur9 (
              quotient (
                * (
                  - end8 start7
                )
                 1000000
              )
               jps10
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur9
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
