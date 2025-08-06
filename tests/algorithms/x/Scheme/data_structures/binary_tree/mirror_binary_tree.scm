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
      start10 (
        current-jiffy
      )
    )
     (
      jps13 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        mirror_node left right idx
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
              let (
                (
                  temp (
                    list-ref left idx
                  )
                )
              )
               (
                begin (
                  list-set! left idx (
                    list-ref right idx
                  )
                )
                 (
                  list-set! right idx temp
                )
                 (
                  mirror_node left right (
                    list-ref left idx
                  )
                )
                 (
                  mirror_node left right (
                    list-ref right idx
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
        mirror tree
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              mirror_node (
                hash-table-ref tree "left"
              )
               (
                hash-table-ref tree "right"
              )
               (
                hash-table-ref tree "root"
              )
            )
             (
              ret2 tree
            )
          )
        )
      )
    )
     (
      define (
        inorder tree idx
      )
       (
        call/cc (
          lambda (
            ret3
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
                  ret3 (
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
                  left_vals (
                    inorder tree (
                      list-ref (
                        hash-table-ref tree "left"
                      )
                       idx
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      right_vals (
                        inorder tree (
                          list-ref (
                            hash-table-ref tree "right"
                          )
                           idx
                        )
                      )
                    )
                  )
                   (
                    begin (
                      ret3 (
                        append (
                          append left_vals (
                            _list (
                              list-ref (
                                hash-table-ref tree "values"
                              )
                               idx
                            )
                          )
                        )
                         right_vals
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
        make_tree_zero
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            ret4 (
              alist->hash-table (
                _list (
                  cons "values" (
                    _list 0
                  )
                )
                 (
                  cons "left" (
                    _list (
                      - 1
                    )
                  )
                )
                 (
                  cons "right" (
                    _list (
                      - 1
                    )
                  )
                )
                 (
                  cons "root" 0
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        make_tree_seven
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            ret5 (
              alist->hash-table (
                _list (
                  cons "values" (
                    _list 1 2 3 4 5 6 7
                  )
                )
                 (
                  cons "left" (
                    _list 1 3 5 (
                      - 1
                    )
                     (
                      - 1
                    )
                     (
                      - 1
                    )
                     (
                      - 1
                    )
                  )
                )
                 (
                  cons "right" (
                    _list 2 4 6 (
                      - 1
                    )
                     (
                      - 1
                    )
                     (
                      - 1
                    )
                     (
                      - 1
                    )
                  )
                )
                 (
                  cons "root" 0
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        make_tree_nine
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            ret6 (
              alist->hash-table (
                _list (
                  cons "values" (
                    _list 1 2 3 4 5 6 7 8 9
                  )
                )
                 (
                  cons "left" (
                    _list 1 3 (
                      - 1
                    )
                     6 (
                      - 1
                    )
                     (
                      - 1
                    )
                     (
                      - 1
                    )
                     (
                      - 1
                    )
                     (
                      - 1
                    )
                  )
                )
                 (
                  cons "right" (
                    _list 2 4 5 7 8 (
                      - 1
                    )
                     (
                      - 1
                    )
                     (
                      - 1
                    )
                     (
                      - 1
                    )
                  )
                )
                 (
                  cons "root" 0
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
            ret7
          )
           (
            let (
              (
                names (
                  _list "zero" "seven" "nine"
                )
              )
            )
             (
              begin (
                let (
                  (
                    trees (
                      _list (
                        make_tree_zero
                      )
                       (
                        make_tree_seven
                      )
                       (
                        make_tree_nine
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
                                        _len trees
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            tree (
                                              list-ref trees i
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
                                                      string-append "      The " (
                                                        list-ref names i
                                                      )
                                                    )
                                                     " tree: "
                                                  )
                                                   (
                                                    to-str-space (
                                                      inorder tree (
                                                        hash-table-ref tree "root"
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                string-append (
                                                  string-append (
                                                    string-append "      The " (
                                                      list-ref names i
                                                    )
                                                  )
                                                   " tree: "
                                                )
                                                 (
                                                  to-str-space (
                                                    inorder tree (
                                                      hash-table-ref tree "root"
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                to-str (
                                                  string-append (
                                                    string-append (
                                                      string-append "      The " (
                                                        list-ref names i
                                                      )
                                                    )
                                                     " tree: "
                                                  )
                                                   (
                                                    to-str-space (
                                                      inorder tree (
                                                        hash-table-ref tree "root"
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
                                                mirrored (
                                                  mirror tree
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
                                                          string-append "Mirror of " (
                                                            list-ref names i
                                                          )
                                                        )
                                                         " tree: "
                                                      )
                                                       (
                                                        to-str-space (
                                                          inorder mirrored (
                                                            hash-table-ref mirrored "root"
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    string-append (
                                                      string-append (
                                                        string-append "Mirror of " (
                                                          list-ref names i
                                                        )
                                                      )
                                                       " tree: "
                                                    )
                                                     (
                                                      to-str-space (
                                                        inorder mirrored (
                                                          hash-table-ref mirrored "root"
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    to-str (
                                                      string-append (
                                                        string-append (
                                                          string-append "Mirror of " (
                                                            list-ref names i
                                                          )
                                                        )
                                                         " tree: "
                                                      )
                                                       (
                                                        to-str-space (
                                                          inorder mirrored (
                                                            hash-table-ref mirrored "root"
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
                                                set! i (
                                                  + i 1
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
     (
      let (
        (
          end11 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur12 (
              quotient (
                * (
                  - end11 start10
                )
                 1000000
              )
               jps13
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur12
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
