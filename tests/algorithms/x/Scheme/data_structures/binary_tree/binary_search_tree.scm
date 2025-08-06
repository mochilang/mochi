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
        create_node value
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              _list value (
                quote (
                  
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
     (
      define (
        insert node value
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                equal? node (
                  quote (
                    
                  )
                )
              )
               (
                begin (
                  ret2 (
                    create_node value
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
                _lt value (
                  list-ref node 0
                )
              )
               (
                begin (
                  list-set! node 1 (
                    insert (
                      list-ref node 1
                    )
                     value
                  )
                )
              )
               (
                if (
                  _gt value (
                    list-ref node 0
                  )
                )
                 (
                  begin (
                    list-set! node 2 (
                      insert (
                        list-ref node 2
                      )
                       value
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
              ret2 node
            )
          )
        )
      )
    )
     (
      define (
        search node value
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            begin (
              if (
                equal? node (
                  quote (
                    
                  )
                )
              )
               (
                begin (
                  ret3 #f
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                equal? value (
                  list-ref node 0
                )
              )
               (
                begin (
                  ret3 #t
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                _lt value (
                  list-ref node 0
                )
              )
               (
                begin (
                  ret3 (
                    search (
                      list-ref node 1
                    )
                     value
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
                search (
                  list-ref node 2
                )
                 value
              )
            )
          )
        )
      )
    )
     (
      define (
        inorder node acc
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                equal? node (
                  quote (
                    
                  )
                )
              )
               (
                begin (
                  ret4 acc
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
                  left_acc (
                    inorder (
                      list-ref node 1
                    )
                     acc
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      with_node (
                        append left_acc (
                          _list (
                            list-ref node 0
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      ret4 (
                        inorder (
                          list-ref node 2
                        )
                         with_node
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
        find_min node
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                current node
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
                              not (
                                equal? (
                                  list-ref current 1
                                )
                                 (
                                  quote (
                                    
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                set! current (
                                  list-ref current 1
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
                ret5 (
                  list-ref current 0
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        find_max node
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                current node
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
                              not (
                                equal? (
                                  list-ref current 2
                                )
                                 (
                                  quote (
                                    
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                set! current (
                                  list-ref current 2
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
                ret8 (
                  list-ref current 0
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        delete node value
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            begin (
              if (
                equal? node (
                  quote (
                    
                  )
                )
              )
               (
                begin (
                  ret11 (
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
                _lt value (
                  list-ref node 0
                )
              )
               (
                begin (
                  list-set! node 1 (
                    delete (
                      list-ref node 1
                    )
                     value
                  )
                )
              )
               (
                if (
                  _gt value (
                    list-ref node 0
                  )
                )
                 (
                  begin (
                    list-set! node 2 (
                      delete (
                        list-ref node 2
                      )
                       value
                    )
                  )
                )
                 (
                  begin (
                    if (
                      equal? (
                        list-ref node 1
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      begin (
                        ret11 (
                          list-ref node 2
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
                      equal? (
                        list-ref node 2
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      begin (
                        ret11 (
                          list-ref node 1
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
                        min_val (
                          find_min (
                            list-ref node 2
                          )
                        )
                      )
                    )
                     (
                      begin (
                        list-set! node 0 min_val
                      )
                       (
                        list-set! node 2 (
                          delete (
                            list-ref node 2
                          )
                           min_val
                        )
                      )
                    )
                  )
                )
              )
            )
             (
              ret11 node
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
            ret12
          )
           (
            let (
              (
                root (
                  quote (
                    
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    nums (
                      _list 8 3 6 1 10 14 13 4 7
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
                                        v (
                                          car xs
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! root (
                                          insert root v
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop13 (
                                      cdr xs
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          loop13 nums
                        )
                      )
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          to-str-space (
                            inorder root (
                              _list
                            )
                          )
                        )
                      )
                       (
                        to-str-space (
                          inorder root (
                            _list
                          )
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            inorder root (
                              _list
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
                          search root 6
                        )
                      )
                       (
                        search root 6
                      )
                       (
                        to-str (
                          search root 6
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
                          search root 20
                        )
                      )
                       (
                        search root 20
                      )
                       (
                        to-str (
                          search root 20
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
                          find_min root
                        )
                      )
                       (
                        find_min root
                      )
                       (
                        to-str (
                          find_min root
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
                          find_max root
                        )
                      )
                       (
                        find_max root
                      )
                       (
                        to-str (
                          find_max root
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! root (
                      delete root 6
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          to-str-space (
                            inorder root (
                              _list
                            )
                          )
                        )
                      )
                       (
                        to-str-space (
                          inorder root (
                            _list
                          )
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            inorder root (
                              _list
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
