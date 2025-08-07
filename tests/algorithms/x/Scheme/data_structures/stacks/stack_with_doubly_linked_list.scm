;; Generated on 2025-08-07 08:20 +0700
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
      start13 (
        current-jiffy
      )
    )
     (
      jps16 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        empty_stack
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              alist->hash-table (
                _list (
                  cons "nodes" (
                    _list
                  )
                )
                 (
                  cons "head" (
                    - 0 1
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
        push stack value
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                nodes (
                  hash-table-ref stack "nodes"
                )
              )
            )
             (
              begin (
                let (
                  (
                    idx (
                      _len nodes
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        new_node (
                          alist->hash-table (
                            _list (
                              cons "data" value
                            )
                             (
                              cons "next" (
                                hash-table-ref stack "head"
                              )
                            )
                             (
                              cons "prev" (
                                - 0 1
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        set! nodes (
                          append nodes (
                            _list new_node
                          )
                        )
                      )
                       (
                        if (
                          not (
                            equal? (
                              hash-table-ref stack "head"
                            )
                             (
                              - 0 1
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                head_node (
                                  list-ref nodes (
                                    hash-table-ref stack "head"
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                hash-table-set! head_node "prev" idx
                              )
                               (
                                list-set! nodes (
                                  hash-table-ref stack "head"
                                )
                                 head_node
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
                        ret2 (
                          alist->hash-table (
                            _list (
                              cons "nodes" nodes
                            )
                             (
                              cons "head" idx
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
        pop stack
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            begin (
              if (
                equal? (
                  hash-table-ref stack "head"
                )
                 (
                  - 0 1
                )
              )
               (
                begin (
                  ret3 (
                    alist->hash-table (
                      _list (
                        cons "stack" stack
                      )
                       (
                        cons "value" 0
                      )
                       (
                        cons "ok" #f
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
              let (
                (
                  nodes (
                    hash-table-ref stack "nodes"
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      head_node (
                        list-ref nodes (
                          hash-table-ref stack "head"
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          value (
                            hash-table-ref head_node "data"
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              next_idx (
                                hash-table-ref head_node "next"
                              )
                            )
                          )
                           (
                            begin (
                              if (
                                not (
                                  equal? next_idx (
                                    - 0 1
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      next_node (
                                        list-ref nodes next_idx
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      hash-table-set! next_node "prev" (
                                        - 0 1
                                      )
                                    )
                                     (
                                      list-set! nodes next_idx next_node
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
                                  new_stack (
                                    alist->hash-table (
                                      _list (
                                        cons "nodes" nodes
                                      )
                                       (
                                        cons "head" next_idx
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  ret3 (
                                    alist->hash-table (
                                      _list (
                                        cons "stack" new_stack
                                      )
                                       (
                                        cons "value" value
                                      )
                                       (
                                        cons "ok" #t
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
        top stack
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                equal? (
                  hash-table-ref stack "head"
                )
                 (
                  - 0 1
                )
              )
               (
                begin (
                  ret4 (
                    alist->hash-table (
                      _list (
                        cons "value" 0
                      )
                       (
                        cons "ok" #f
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
              let (
                (
                  node (
                    list-ref (
                      hash-table-ref stack "nodes"
                    )
                     (
                      hash-table-ref stack "head"
                    )
                  )
                )
              )
               (
                begin (
                  ret4 (
                    alist->hash-table (
                      _list (
                        cons "value" (
                          hash-table-ref node "data"
                        )
                      )
                       (
                        cons "ok" #t
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
        size stack
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                count 0
              )
            )
             (
              begin (
                let (
                  (
                    idx (
                      hash-table-ref stack "head"
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
                                  not (
                                    equal? idx (
                                      - 0 1
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
                                    let (
                                      (
                                        node (
                                          list-ref (
                                            hash-table-ref stack "nodes"
                                          )
                                           idx
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! idx (
                                          hash-table-ref node "next"
                                        )
                                      )
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
                    ret5 count
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
        is_empty stack
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            ret8 (
              equal? (
                hash-table-ref stack "head"
              )
               (
                - 0 1
              )
            )
          )
        )
      )
    )
     (
      define (
        print_stack stack
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            begin (
              _display (
                if (
                  string? "stack elements are:"
                )
                 "stack elements are:" (
                  to-str "stack elements are:"
                )
              )
            )
             (
              newline
            )
             (
              let (
                (
                  idx (
                    hash-table-ref stack "head"
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      s ""
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
                                    not (
                                      equal? idx (
                                        - 0 1
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          node (
                                            list-ref (
                                              hash-table-ref stack "nodes"
                                            )
                                             idx
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          set! s (
                                            string-append (
                                              string-append s (
                                                to-str-space (
                                                  hash-table-ref node "data"
                                                )
                                              )
                                            )
                                             "->"
                                          )
                                        )
                                         (
                                          set! idx (
                                            hash-table-ref node "next"
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
                      if (
                        > (
                          _len s
                        )
                         0
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? s
                            )
                             s (
                              to-str s
                            )
                          )
                        )
                         (
                          newline
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
                stack (
                  empty_stack
                )
              )
            )
             (
              begin (
                _display (
                  if (
                    string? "Stack operations using Doubly LinkedList"
                  )
                   "Stack operations using Doubly LinkedList" (
                    to-str "Stack operations using Doubly LinkedList"
                  )
                )
              )
               (
                newline
              )
               (
                set! stack (
                  push stack 4
                )
              )
               (
                set! stack (
                  push stack 5
                )
              )
               (
                set! stack (
                  push stack 6
                )
              )
               (
                set! stack (
                  push stack 7
                )
              )
               (
                print_stack stack
              )
               (
                let (
                  (
                    t (
                      top stack
                    )
                  )
                )
                 (
                  begin (
                    if (
                      hash-table-ref t "ok"
                    )
                     (
                      begin (
                        _display (
                          if (
                            string? (
                              string-append "Top element is " (
                                to-str-space (
                                  hash-table-ref t "value"
                                )
                              )
                            )
                          )
                           (
                            string-append "Top element is " (
                              to-str-space (
                                hash-table-ref t "value"
                              )
                            )
                          )
                           (
                            to-str (
                              string-append "Top element is " (
                                to-str-space (
                                  hash-table-ref t "value"
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
                     (
                      begin (
                        _display (
                          if (
                            string? "Top element is None"
                          )
                           "Top element is None" (
                            to-str "Top element is None"
                          )
                        )
                      )
                       (
                        newline
                      )
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          string-append "Size of the stack is " (
                            to-str-space (
                              size stack
                            )
                          )
                        )
                      )
                       (
                        string-append "Size of the stack is " (
                          to-str-space (
                            size stack
                          )
                        )
                      )
                       (
                        to-str (
                          string-append "Size of the stack is " (
                            to-str-space (
                              size stack
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
                        p (
                          pop stack
                        )
                      )
                    )
                     (
                      begin (
                        set! stack (
                          hash-table-ref p "stack"
                        )
                      )
                       (
                        set! p (
                          pop stack
                        )
                      )
                       (
                        set! stack (
                          hash-table-ref p "stack"
                        )
                      )
                       (
                        print_stack stack
                      )
                       (
                        _display (
                          if (
                            string? (
                              string-append "stack is empty: " (
                                to-str-space (
                                  is_empty stack
                                )
                              )
                            )
                          )
                           (
                            string-append "stack is empty: " (
                              to-str-space (
                                is_empty stack
                              )
                            )
                          )
                           (
                            to-str (
                              string-append "stack is empty: " (
                                to-str-space (
                                  is_empty stack
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
    )
     (
      main
    )
     (
      let (
        (
          end14 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur15 (
              quotient (
                * (
                  - end14 start13
                )
                 1000000
              )
               jps16
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur15
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
