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
      start32 (
        current-jiffy
      )
    )
     (
      jps35 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        new_node prefix is_leaf
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
                  cons "prefix" prefix
                )
                 (
                  cons "is_leaf" is_leaf
                )
                 (
                  cons "children" (
                    alist->hash-table (
                      _list
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
        new_tree
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
                  _list (
                    new_node "" #f
                  )
                )
              )
            )
             (
              begin (
                ret2 (
                  alist->hash-table (
                    _list (
                      cons "nodes" nodes
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
        match_prefix node word
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            let (
              (
                x 0
              )
            )
             (
              begin (
                let (
                  (
                    p (
                      hash-table-ref node "prefix"
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        w word
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            min_len (
                              _len p
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              < (
                                _len w
                              )
                               min_len
                            )
                             (
                              begin (
                                set! min_len (
                                  _len w
                                )
                              )
                            )
                             (
                              quote (
                                
                              )
                            )
                          )
                           (
                            call/cc (
                              lambda (
                                break5
                              )
                               (
                                letrec (
                                  (
                                    loop4 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < x min_len
                                        )
                                         (
                                          begin (
                                            if (
                                              not (
                                                string=? (
                                                  _substring p x (
                                                    + x 1
                                                  )
                                                )
                                                 (
                                                  _substring w x (
                                                    + x 1
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                break5 (
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
                                            set! x (
                                              + x 1
                                            )
                                          )
                                           (
                                            loop4
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
                                  loop4
                                )
                              )
                            )
                          )
                           (
                            let (
                              (
                                common (
                                  _substring p 0 x
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    rem_prefix (
                                      _substring p x (
                                        _len p
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        rem_word (
                                          _substring w x (
                                            _len w
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        ret3 (
                                          alist->hash-table (
                                            _list (
                                              cons "common" common
                                            )
                                             (
                                              cons "rem_prefix" rem_prefix
                                            )
                                             (
                                              cons "rem_word" rem_word
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
        insert_many tree words
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            call/cc (
              lambda (
                break8
              )
               (
                letrec (
                  (
                    loop7 (
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
                                w (
                                  car xs
                                )
                              )
                            )
                             (
                              begin (
                                insert tree 0 w
                              )
                            )
                          )
                           (
                            loop7 (
                              cdr xs
                            )
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  loop7 words
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        insert tree idx word
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                nodes (
                  hash-table-ref tree "nodes"
                )
              )
            )
             (
              begin (
                let (
                  (
                    node (
                      list-ref nodes idx
                    )
                  )
                )
                 (
                  begin (
                    if (
                      and (
                        string=? (
                          hash-table-ref node "prefix"
                        )
                         word
                      )
                       (
                        not (
                          hash-table-ref node "is_leaf"
                        )
                      )
                    )
                     (
                      begin (
                        hash-table-set! node "is_leaf" #t
                      )
                       (
                        list-set! nodes idx node
                      )
                       (
                        hash-table-set! tree "nodes" nodes
                      )
                       (
                        ret9 (
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
                        first (
                          _substring word 0 1
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            children (
                              hash-table-ref node "children"
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              not (
                                has_key children first
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    new_idx (
                                      _len nodes
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! nodes (
                                      append nodes (
                                        _list (
                                          new_node word #t
                                        )
                                      )
                                    )
                                  )
                                   (
                                    hash-table-set! children first new_idx
                                  )
                                   (
                                    hash-table-set! node "children" children
                                  )
                                   (
                                    list-set! nodes idx node
                                  )
                                   (
                                    hash-table-set! tree "nodes" nodes
                                  )
                                   (
                                    ret9 (
                                      quote (
                                        
                                      )
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
                                child_idx (
                                  hash-table-ref/default children first (
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
                                    child (
                                      list-ref nodes child_idx
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        res (
                                          match_prefix child word
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          string=? (
                                            hash-table-ref res "rem_prefix"
                                          )
                                           ""
                                        )
                                         (
                                          begin (
                                            insert tree child_idx (
                                              hash-table-ref res "rem_word"
                                            )
                                          )
                                           (
                                            ret9 (
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
                                        hash-table-set! child "prefix" (
                                          hash-table-ref res "rem_prefix"
                                        )
                                      )
                                       (
                                        list-set! nodes child_idx child
                                      )
                                       (
                                        let (
                                          (
                                            new_children (
                                              alist->hash-table (
                                                _list
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            hash-table-set! new_children (
                                              _substring (
                                                hash-table-ref res "rem_prefix"
                                              )
                                               0 1
                                            )
                                             child_idx
                                          )
                                           (
                                            let (
                                              (
                                                new_idx (
                                                  _len nodes
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! nodes (
                                                  append nodes (
                                                    _list (
                                                      new_node (
                                                        hash-table-ref res "common"
                                                      )
                                                       #f
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                hash-table-set! (
                                                  list-ref nodes new_idx
                                                )
                                                 "children" new_children
                                              )
                                               (
                                                if (
                                                  string=? (
                                                    hash-table-ref res "rem_word"
                                                  )
                                                   ""
                                                )
                                                 (
                                                  begin (
                                                    hash-table-set! (
                                                      list-ref nodes new_idx
                                                    )
                                                     "is_leaf" #t
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    insert tree new_idx (
                                                      hash-table-ref res "rem_word"
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                hash-table-set! children first new_idx
                                              )
                                               (
                                                hash-table-set! node "children" children
                                              )
                                               (
                                                list-set! nodes idx node
                                              )
                                               (
                                                hash-table-set! tree "nodes" nodes
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
    )
     (
      define (
        find tree idx word
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                nodes (
                  hash-table-ref tree "nodes"
                )
              )
            )
             (
              begin (
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
                        first (
                          _substring word 0 1
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            children (
                              hash-table-ref node "children"
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              not (
                                has_key children first
                              )
                            )
                             (
                              begin (
                                ret10 #f
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
                                child_idx (
                                  hash-table-ref/default children first (
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
                                    child (
                                      list-ref nodes child_idx
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        res (
                                          match_prefix child word
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          not (
                                            string=? (
                                              hash-table-ref res "rem_prefix"
                                            )
                                             ""
                                          )
                                        )
                                         (
                                          begin (
                                            ret10 #f
                                          )
                                        )
                                         (
                                          quote (
                                            
                                          )
                                        )
                                      )
                                       (
                                        if (
                                          string=? (
                                            hash-table-ref res "rem_word"
                                          )
                                           ""
                                        )
                                         (
                                          begin (
                                            ret10 (
                                              hash-table-ref child "is_leaf"
                                            )
                                          )
                                        )
                                         (
                                          quote (
                                            
                                          )
                                        )
                                      )
                                       (
                                        ret10 (
                                          find tree child_idx (
                                            hash-table-ref res "rem_word"
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
        remove_key m k
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            let (
              (
                out (
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
                                    key (
                                      car xs
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      not (
                                        string=? key k
                                      )
                                    )
                                     (
                                      begin (
                                        hash-table-set! out key (
                                          hash-table-ref/default m key (
                                            quote (
                                              
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
                        hash-table-keys m
                      )
                    )
                  )
                )
              )
               (
                ret11 out
              )
            )
          )
        )
      )
    )
     (
      define (
        has_key m k
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            begin (
              call/cc (
                lambda (
                  break16
                )
                 (
                  letrec (
                    (
                      loop15 (
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
                                  key (
                                    car xs
                                  )
                                )
                              )
                               (
                                begin (
                                  if (
                                    string=? key k
                                  )
                                   (
                                    begin (
                                      ret14 #t
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
                              loop15 (
                                cdr xs
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                   (
                    loop15 (
                      hash-table-keys m
                    )
                  )
                )
              )
            )
             (
              ret14 #f
            )
          )
        )
      )
    )
     (
      define (
        delete tree idx word
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            let (
              (
                nodes (
                  hash-table-ref tree "nodes"
                )
              )
            )
             (
              begin (
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
                        first (
                          _substring word 0 1
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            children (
                              hash-table-ref node "children"
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              not (
                                has_key children first
                              )
                            )
                             (
                              begin (
                                ret17 #f
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
                                child_idx (
                                  hash-table-ref/default children first (
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
                                    child (
                                      list-ref nodes child_idx
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        res (
                                          match_prefix child word
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          not (
                                            string=? (
                                              hash-table-ref res "rem_prefix"
                                            )
                                             ""
                                          )
                                        )
                                         (
                                          begin (
                                            ret17 #f
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
                                            string=? (
                                              hash-table-ref res "rem_word"
                                            )
                                             ""
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                deleted (
                                                  delete tree child_idx (
                                                    hash-table-ref res "rem_word"
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if deleted (
                                                  begin (
                                                    set! nodes (
                                                      hash-table-ref tree "nodes"
                                                    )
                                                  )
                                                   (
                                                    set! node (
                                                      list-ref nodes idx
                                                    )
                                                  )
                                                )
                                                 (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                               (
                                                ret17 deleted
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
                                          not (
                                            hash-table-ref child "is_leaf"
                                          )
                                        )
                                         (
                                          begin (
                                            ret17 #f
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
                                            _len (
                                              hash-table-ref child "children"
                                            )
                                          )
                                           0
                                        )
                                         (
                                          begin (
                                            set! children (
                                              remove_key children first
                                            )
                                          )
                                           (
                                            hash-table-set! node "children" children
                                          )
                                           (
                                            list-set! nodes idx node
                                          )
                                           (
                                            hash-table-set! tree "nodes" nodes
                                          )
                                           (
                                            if (
                                              and (
                                                equal? (
                                                  _len children
                                                )
                                                 1
                                              )
                                               (
                                                not (
                                                  hash-table-ref node "is_leaf"
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    only_key ""
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
                                                                        set! only_key k
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    loop18 (
                                                                      cdr xs
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          loop18 (
                                                            hash-table-keys children
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    let (
                                                      (
                                                        merge_idx (
                                                          hash-table-ref/default children only_key (
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
                                                            merge_node (
                                                              list-ref nodes merge_idx
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            hash-table-set! node "is_leaf" (
                                                              hash-table-ref merge_node "is_leaf"
                                                            )
                                                          )
                                                           (
                                                            hash-table-set! node "prefix" (
                                                              string-append (
                                                                hash-table-ref node "prefix"
                                                              )
                                                               (
                                                                hash-table-ref merge_node "prefix"
                                                              )
                                                            )
                                                          )
                                                           (
                                                            hash-table-set! node "children" (
                                                              hash-table-ref merge_node "children"
                                                            )
                                                          )
                                                           (
                                                            list-set! nodes idx node
                                                          )
                                                           (
                                                            hash-table-set! tree "nodes" nodes
                                                          )
                                                        )
                                                      )
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
                                        )
                                         (
                                          if (
                                            > (
                                              _len (
                                                hash-table-ref child "children"
                                              )
                                            )
                                             1
                                          )
                                           (
                                            begin (
                                              hash-table-set! child "is_leaf" #f
                                            )
                                             (
                                              list-set! nodes child_idx child
                                            )
                                             (
                                              hash-table-set! tree "nodes" nodes
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  only_key ""
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
                                                                      set! only_key k
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  loop20 (
                                                                    cdr xs
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        loop20 (
                                                          hash-table-keys (
                                                            hash-table-ref child "children"
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  let (
                                                    (
                                                      merge_idx (
                                                        hash-table-ref/default (
                                                          hash-table-ref child "children"
                                                        )
                                                         only_key (
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
                                                          merge_node (
                                                            list-ref nodes merge_idx
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          hash-table-set! child "is_leaf" (
                                                            hash-table-ref merge_node "is_leaf"
                                                          )
                                                        )
                                                         (
                                                          hash-table-set! child "prefix" (
                                                            string-append (
                                                              hash-table-ref child "prefix"
                                                            )
                                                             (
                                                              hash-table-ref merge_node "prefix"
                                                            )
                                                          )
                                                        )
                                                         (
                                                          hash-table-set! child "children" (
                                                            hash-table-ref merge_node "children"
                                                          )
                                                        )
                                                         (
                                                          list-set! nodes child_idx child
                                                        )
                                                         (
                                                          hash-table-set! tree "nodes" nodes
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
                                        ret17 #t
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
        print_tree tree idx height
      )
       (
        call/cc (
          lambda (
            ret22
          )
           (
            let (
              (
                nodes (
                  hash-table-ref tree "nodes"
                )
              )
            )
             (
              begin (
                let (
                  (
                    node (
                      list-ref nodes idx
                    )
                  )
                )
                 (
                  begin (
                    if (
                      not (
                        string=? (
                          hash-table-ref node "prefix"
                        )
                         ""
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            line ""
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
                                    break24
                                  )
                                   (
                                    letrec (
                                      (
                                        loop23 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i height
                                            )
                                             (
                                              begin (
                                                set! line (
                                                  string-append line "-"
                                                )
                                              )
                                               (
                                                set! i (
                                                  + i 1
                                                )
                                              )
                                               (
                                                loop23
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
                                      loop23
                                    )
                                  )
                                )
                              )
                               (
                                set! line (
                                  string-append (
                                    string-append line " "
                                  )
                                   (
                                    hash-table-ref node "prefix"
                                  )
                                )
                              )
                               (
                                if (
                                  hash-table-ref node "is_leaf"
                                )
                                 (
                                  begin (
                                    set! line (
                                      string-append line "  (leaf)"
                                    )
                                  )
                                )
                                 (
                                  quote (
                                    
                                  )
                                )
                              )
                               (
                                _display (
                                  if (
                                    string? line
                                  )
                                   line (
                                    to-str line
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
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
                    let (
                      (
                        children (
                          hash-table-ref node "children"
                        )
                      )
                    )
                     (
                      begin (
                        call/cc (
                          lambda (
                            break26
                          )
                           (
                            letrec (
                              (
                                loop25 (
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
                                            let (
                                              (
                                                child_idx (
                                                  hash-table-ref/default children k (
                                                    quote (
                                                      
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                print_tree tree child_idx (
                                                  + height 1
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop25 (
                                          cdr xs
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop25 (
                                hash-table-keys children
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
        test_trie
      )
       (
        call/cc (
          lambda (
            ret27
          )
           (
            let (
              (
                words (
                  _list "banana" "bananas" "bandana" "band" "apple" "all" "beast"
                )
              )
            )
             (
              begin (
                let (
                  (
                    tree (
                      new_tree
                    )
                  )
                )
                 (
                  begin (
                    insert_many tree words
                  )
                   (
                    let (
                      (
                        ok #t
                      )
                    )
                     (
                      begin (
                        call/cc (
                          lambda (
                            break29
                          )
                           (
                            letrec (
                              (
                                loop28 (
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
                                            w (
                                              car xs
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              not (
                                                find tree 0 w
                                              )
                                            )
                                             (
                                              begin (
                                                set! ok #f
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
                                        loop28 (
                                          cdr xs
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop28 words
                            )
                          )
                        )
                      )
                       (
                        if (
                          find tree 0 "bandanas"
                        )
                         (
                          begin (
                            set! ok #f
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        if (
                          find tree 0 "apps"
                        )
                         (
                          begin (
                            set! ok #f
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        delete tree 0 "all"
                      )
                       (
                        if (
                          find tree 0 "all"
                        )
                         (
                          begin (
                            set! ok #f
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        delete tree 0 "banana"
                      )
                       (
                        if (
                          find tree 0 "banana"
                        )
                         (
                          begin (
                            set! ok #f
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
                            find tree 0 "bananas"
                          )
                        )
                         (
                          begin (
                            set! ok #f
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        ret27 ok
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
        pytests
      )
       (
        call/cc (
          lambda (
            ret30
          )
           (
            if (
              not (
                test_trie
              )
            )
             (
              begin (
                panic "test failed"
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
      define (
        main
      )
       (
        call/cc (
          lambda (
            ret31
          )
           (
            let (
              (
                tree (
                  new_tree
                )
              )
            )
             (
              begin (
                let (
                  (
                    words (
                      _list "banana" "bananas" "bandanas" "bandana" "band" "apple" "all" "beast"
                    )
                  )
                )
                 (
                  begin (
                    insert_many tree words
                  )
                   (
                    _display (
                      if (
                        string? (
                          string-append "Words: " (
                            to-str-space words
                          )
                        )
                      )
                       (
                        string-append "Words: " (
                          to-str-space words
                        )
                      )
                       (
                        to-str (
                          string-append "Words: " (
                            to-str-space words
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
                        string? "Tree:"
                      )
                       "Tree:" (
                        to-str "Tree:"
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    print_tree tree 0 0
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
          end33 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur34 (
              quotient (
                * (
                  - end33 start32
                )
                 1000000
              )
               jps35
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur34
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
