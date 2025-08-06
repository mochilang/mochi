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
      start5 (
        current-jiffy
      )
    )
     (
      jps8 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        inorder nodes idx
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                equal? idx (
                  - 1
                )
              )
               (
                begin (
                  ret1 (
                    _list
                  )
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
                    list-ref nodes idx
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      result (
                        inorder nodes (
                          hash-table-ref node "left"
                        )
                      )
                    )
                  )
                   (
                    begin (
                      set! result (
                        append result (
                          _list (
                            hash-table-ref node "key"
                          )
                        )
                      )
                    )
                     (
                      set! result (
                        append result (
                          inorder nodes (
                            hash-table-ref node "right"
                          )
                        )
                      )
                    )
                     (
                      ret1 result
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
        floor_ceiling nodes idx key
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                floor_val (
                  quote (
                    
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    ceiling_val (
                      quote (
                        
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        current idx
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
                                      not (
                                        equal? current (
                                          - 1
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            node (
                                              list-ref nodes current
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              equal? (
                                                hash-table-ref node "key"
                                              )
                                               key
                                            )
                                             (
                                              begin (
                                                set! floor_val (
                                                  hash-table-ref node "key"
                                                )
                                              )
                                               (
                                                set! ceiling_val (
                                                  hash-table-ref node "key"
                                                )
                                              )
                                               (
                                                break4 (
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
                                            if (
                                              < key (
                                                hash-table-ref node "key"
                                              )
                                            )
                                             (
                                              begin (
                                                set! ceiling_val (
                                                  hash-table-ref node "key"
                                                )
                                              )
                                               (
                                                set! current (
                                                  hash-table-ref node "left"
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! floor_val (
                                                  hash-table-ref node "key"
                                                )
                                              )
                                               (
                                                set! current (
                                                  hash-table-ref node "right"
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
                       (
                        ret2 (
                          _list floor_val ceiling_val
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
          tree (
            _list (
              alist->hash-table (
                _list (
                  cons "key" 10
                )
                 (
                  cons "left" 1
                )
                 (
                  cons "right" 2
                )
              )
            )
             (
              alist->hash-table (
                _list (
                  cons "key" 5
                )
                 (
                  cons "left" 3
                )
                 (
                  cons "right" 4
                )
              )
            )
             (
              alist->hash-table (
                _list (
                  cons "key" 20
                )
                 (
                  cons "left" 5
                )
                 (
                  cons "right" 6
                )
              )
            )
             (
              alist->hash-table (
                _list (
                  cons "key" 3
                )
                 (
                  cons "left" (
                    - 1
                  )
                )
                 (
                  cons "right" (
                    - 1
                  )
                )
              )
            )
             (
              alist->hash-table (
                _list (
                  cons "key" 7
                )
                 (
                  cons "left" (
                    - 1
                  )
                )
                 (
                  cons "right" (
                    - 1
                  )
                )
              )
            )
             (
              alist->hash-table (
                _list (
                  cons "key" 15
                )
                 (
                  cons "left" (
                    - 1
                  )
                )
                 (
                  cons "right" (
                    - 1
                  )
                )
              )
            )
             (
              alist->hash-table (
                _list (
                  cons "key" 25
                )
                 (
                  cons "left" (
                    - 1
                  )
                )
                 (
                  cons "right" (
                    - 1
                  )
                )
              )
            )
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                to-str-space (
                  inorder tree 0
                )
              )
            )
             (
              to-str-space (
                inorder tree 0
              )
            )
             (
              to-str (
                to-str-space (
                  inorder tree 0
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
                  floor_ceiling tree 0 8
                )
              )
            )
             (
              to-str-space (
                floor_ceiling tree 0 8
              )
            )
             (
              to-str (
                to-str-space (
                  floor_ceiling tree 0 8
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
                  floor_ceiling tree 0 14
                )
              )
            )
             (
              to-str-space (
                floor_ceiling tree 0 14
              )
            )
             (
              to-str (
                to-str-space (
                  floor_ceiling tree 0 14
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
                  floor_ceiling tree 0 (
                    - 1
                  )
                )
              )
            )
             (
              to-str-space (
                floor_ceiling tree 0 (
                  - 1
                )
              )
            )
             (
              to-str (
                to-str-space (
                  floor_ceiling tree 0 (
                    - 1
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
                  floor_ceiling tree 0 30
                )
              )
            )
             (
              to-str-space (
                floor_ceiling tree 0 30
              )
            )
             (
              to-str (
                to-str-space (
                  floor_ceiling tree 0 30
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
          end6 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur7 (
              quotient (
                * (
                  - end6 start5
                )
                 1000000
              )
               jps8
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur7
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
