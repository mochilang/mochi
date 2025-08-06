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
      start6 (
        current-jiffy
      )
    )
     (
      jps9 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        binary_tree_mirror_dict tree root
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                or (
                  equal? root 0
                )
                 (
                  not (
                    cond (
                      (
                        string? tree
                      )
                       (
                        if (
                          string-contains tree root
                        )
                         #t #f
                      )
                    )
                     (
                      (
                        hash-table? tree
                      )
                       (
                        if (
                          hash-table-exists? tree root
                        )
                         #t #f
                      )
                    )
                     (
                      else (
                        if (
                          member root tree
                        )
                         #t #f
                      )
                    )
                  )
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
                  children (
                    hash-table-ref/default tree root (
                      quote (
                        
                      )
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      left (
                        list-ref children 0
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          right (
                            list-ref children 1
                          )
                        )
                      )
                       (
                        begin (
                          hash-table-set! tree root (
                            _list right left
                          )
                        )
                         (
                          binary_tree_mirror_dict tree left
                        )
                         (
                          binary_tree_mirror_dict tree right
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
        binary_tree_mirror binary_tree root
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                equal? (
                  _len binary_tree
                )
                 0
              )
               (
                begin (
                  panic "binary tree cannot be empty"
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
                  cond (
                    (
                      string? binary_tree
                    )
                     (
                      if (
                        string-contains binary_tree root
                      )
                       #t #f
                    )
                  )
                   (
                    (
                      hash-table? binary_tree
                    )
                     (
                      if (
                        hash-table-exists? binary_tree root
                      )
                       #t #f
                    )
                  )
                   (
                    else (
                      if (
                        member root binary_tree
                      )
                       #t #f
                    )
                  )
                )
              )
               (
                begin (
                  panic (
                    string-append (
                      string-append "root " (
                        to-str-space root
                      )
                    )
                     " is not present in the binary_tree"
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
                  tree_copy (
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
                      break4
                    )
                     (
                      letrec (
                        (
                          loop3 (
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
                                      hash-table-set! tree_copy k (
                                        hash-table-ref/default binary_tree k (
                                          quote (
                                            
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  loop3 (
                                    cdr xs
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                       (
                        loop3 (
                          hash-table-keys binary_tree
                        )
                      )
                    )
                  )
                )
                 (
                  binary_tree_mirror_dict tree_copy root
                )
                 (
                  ret2 tree_copy
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
            ret5
          )
           (
            let (
              (
                binary_tree (
                  alist->hash-table (
                    _list (
                      cons 1 (
                        _list 2 3
                      )
                    )
                     (
                      cons 2 (
                        _list 4 5
                      )
                    )
                     (
                      cons 3 (
                        _list 6 7
                      )
                    )
                     (
                      cons 7 (
                        _list 8 9
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
                      string-append "Binary tree: " (
                        to-str-space binary_tree
                      )
                    )
                  )
                   (
                    string-append "Binary tree: " (
                      to-str-space binary_tree
                    )
                  )
                   (
                    to-str (
                      string-append "Binary tree: " (
                        to-str-space binary_tree
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
                      binary_tree_mirror binary_tree 1
                    )
                  )
                )
                 (
                  begin (
                    _display (
                      if (
                        string? (
                          string-append "Binary tree mirror: " (
                            to-str-space mirrored
                          )
                        )
                      )
                       (
                        string-append "Binary tree mirror: " (
                          to-str-space mirrored
                        )
                      )
                       (
                        to-str (
                          string-append "Binary tree mirror: " (
                            to-str-space mirrored
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
     (
      let (
        (
          end7 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur8 (
              quotient (
                * (
                  - end7 start6
                )
                 1000000
              )
               jps9
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur8
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
