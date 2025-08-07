;; Generated on 2025-08-07 08:56 +0700
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
        key p
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              string-append (
                string-append (
                  to-str-space (
                    hash-table-ref p "x"
                  )
                )
                 ","
              )
               (
                to-str-space (
                  hash-table-ref p "y"
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        path_to_string path
      )
       (
        call/cc (
          lambda (
            ret2
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
                                    _len path
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        pt (
                                          list-ref path i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! s (
                                          string-append (
                                            string-append (
                                              string-append (
                                                string-append (
                                                  string-append s "("
                                                )
                                                 (
                                                  to-str-space (
                                                    hash-table-ref pt "x"
                                                  )
                                                )
                                              )
                                               ", "
                                            )
                                             (
                                              to-str-space (
                                                hash-table-ref pt "y"
                                              )
                                            )
                                          )
                                           ")"
                                        )
                                      )
                                       (
                                        if (
                                          < i (
                                            - (
                                              _len path
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
                   (
                    set! s (
                      string-append s "]"
                    )
                  )
                   (
                    ret2 s
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
        dijkstra grid source destination allow_diagonal
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                rows (
                  _len grid
                )
              )
            )
             (
              begin (
                let (
                  (
                    cols (
                      _len (
                        list-ref grid 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        dx (
                          _list (
                            - 1
                          )
                           1 0 0
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            dy (
                              _list 0 0 (
                                - 1
                              )
                               1
                            )
                          )
                        )
                         (
                          begin (
                            if allow_diagonal (
                              begin (
                                set! dx (
                                  append dx (
                                    _list (
                                      - 1
                                    )
                                     (
                                      - 1
                                    )
                                     1 1
                                  )
                                )
                              )
                               (
                                set! dy (
                                  append dy (
                                    _list (
                                      - 1
                                    )
                                     1 (
                                      - 1
                                    )
                                     1
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
                                INF 1000000000000.0
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    queue (
                                      _list source
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        front 0
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            dist_map (
                                              alist->hash-table (
                                                _list (
                                                  cons (
                                                    key source
                                                  )
                                                   0.0
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                prev (
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
                                                              < front (
                                                                _len queue
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    current (
                                                                      list-ref queue front
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! front (
                                                                      + front 1
                                                                    )
                                                                  )
                                                                   (
                                                                    let (
                                                                      (
                                                                        cur_key (
                                                                          key current
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          and (
                                                                            equal? (
                                                                              hash-table-ref current "x"
                                                                            )
                                                                             (
                                                                              hash-table-ref destination "x"
                                                                            )
                                                                          )
                                                                           (
                                                                            equal? (
                                                                              hash-table-ref current "y"
                                                                            )
                                                                             (
                                                                              hash-table-ref destination "y"
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            break7 (
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
                                                                                            _len dx
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                nx (
                                                                                                  + (
                                                                                                    hash-table-ref current "x"
                                                                                                  )
                                                                                                   (
                                                                                                    list-ref dx i
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                let (
                                                                                                  (
                                                                                                    ny (
                                                                                                      + (
                                                                                                        hash-table-ref current "y"
                                                                                                      )
                                                                                                       (
                                                                                                        list-ref dy i
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    if (
                                                                                                      and (
                                                                                                        and (
                                                                                                          and (
                                                                                                            >= nx 0
                                                                                                          )
                                                                                                           (
                                                                                                            < nx rows
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          >= ny 0
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        < ny cols
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        if (
                                                                                                          equal? (
                                                                                                            cond (
                                                                                                              (
                                                                                                                string? (
                                                                                                                  list-ref grid nx
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                _substring (
                                                                                                                  list-ref grid nx
                                                                                                                )
                                                                                                                 ny (
                                                                                                                  + ny 1
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              (
                                                                                                                hash-table? (
                                                                                                                  list-ref grid nx
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                hash-table-ref (
                                                                                                                  list-ref grid nx
                                                                                                                )
                                                                                                                 ny
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              else (
                                                                                                                list-ref (
                                                                                                                  list-ref grid nx
                                                                                                                )
                                                                                                                 ny
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           1
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            let (
                                                                                                              (
                                                                                                                n_key (
                                                                                                                  string-append (
                                                                                                                    string-append (
                                                                                                                      to-str-space nx
                                                                                                                    )
                                                                                                                     ","
                                                                                                                  )
                                                                                                                   (
                                                                                                                    to-str-space ny
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                if (
                                                                                                                  not (
                                                                                                                    cond (
                                                                                                                      (
                                                                                                                        string? dist_map
                                                                                                                      )
                                                                                                                       (
                                                                                                                        if (
                                                                                                                          string-contains dist_map n_key
                                                                                                                        )
                                                                                                                         #t #f
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      (
                                                                                                                        hash-table? dist_map
                                                                                                                      )
                                                                                                                       (
                                                                                                                        if (
                                                                                                                          hash-table-exists? dist_map n_key
                                                                                                                        )
                                                                                                                         #t #f
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      else (
                                                                                                                        if (
                                                                                                                          member n_key dist_map
                                                                                                                        )
                                                                                                                         #t #f
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    hash-table-set! dist_map n_key (
                                                                                                                      + (
                                                                                                                        hash-table-ref/default dist_map cur_key (
                                                                                                                          quote (
                                                                                                                            
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       1.0
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    hash-table-set! prev n_key current
                                                                                                                  )
                                                                                                                   (
                                                                                                                    set! queue (
                                                                                                                      append queue (
                                                                                                                        _list (
                                                                                                                          alist->hash-table (
                                                                                                                            _list (
                                                                                                                              cons "x" nx
                                                                                                                            )
                                                                                                                             (
                                                                                                                              cons "y" ny
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
                                                                                                          )
                                                                                                        )
                                                                                                         (
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
                                                let (
                                                  (
                                                    dest_key (
                                                      key destination
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      cond (
                                                        (
                                                          string? dist_map
                                                        )
                                                         (
                                                          if (
                                                            string-contains dist_map dest_key
                                                          )
                                                           #t #f
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? dist_map
                                                        )
                                                         (
                                                          if (
                                                            hash-table-exists? dist_map dest_key
                                                          )
                                                           #t #f
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          if (
                                                            member dest_key dist_map
                                                          )
                                                           #t #f
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            path_rev (
                                                              _list destination
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                step_key dest_key
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    step_pt destination
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
                                                                                    equal? step_key (
                                                                                      key source
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! step_pt (
                                                                                      hash-table-ref/default prev step_key (
                                                                                        quote (
                                                                                          
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! step_key (
                                                                                      key step_pt
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! path_rev (
                                                                                      append path_rev (
                                                                                        _list step_pt
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
                                                                    let (
                                                                      (
                                                                        path (
                                                                          _list
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            k (
                                                                              - (
                                                                                _len path_rev
                                                                              )
                                                                               1
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
                                                                                        
                                                                                      )
                                                                                       (
                                                                                        if (
                                                                                          >= k 0
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! path (
                                                                                              append path (
                                                                                                _list (
                                                                                                  list-ref path_rev k
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! k (
                                                                                              - k 1
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            loop12
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
                                                                                  loop12
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            ret5 (
                                                                              alist->hash-table (
                                                                                _list (
                                                                                  cons "distance" (
                                                                                    hash-table-ref/default dist_map dest_key (
                                                                                      quote (
                                                                                        
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  cons "path" path
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
                                                      quote (
                                                        
                                                      )
                                                    )
                                                  )
                                                   (
                                                    ret5 (
                                                      alist->hash-table (
                                                        _list (
                                                          cons "distance" INF
                                                        )
                                                         (
                                                          cons "path" (
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
        print_result res
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            begin (
              _display (
                if (
                  string? (
                    string-append (
                      string-append (
                        to-str-space (
                          hash-table-ref res "distance"
                        )
                      )
                       ", "
                    )
                     (
                      path_to_string (
                        hash-table-ref res "path"
                      )
                    )
                  )
                )
                 (
                  string-append (
                    string-append (
                      to-str-space (
                        hash-table-ref res "distance"
                      )
                    )
                     ", "
                  )
                   (
                    path_to_string (
                      hash-table-ref res "path"
                    )
                  )
                )
                 (
                  to-str (
                    string-append (
                      string-append (
                        to-str-space (
                          hash-table-ref res "distance"
                        )
                      )
                       ", "
                    )
                     (
                      path_to_string (
                        hash-table-ref res "path"
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
     (
      let (
        (
          grid1 (
            _list (
              _list 1 1 1
            )
             (
              _list 0 1 0
            )
             (
              _list 0 1 1
            )
          )
        )
      )
       (
        begin (
          print_result (
            dijkstra grid1 (
              alist->hash-table (
                _list (
                  cons "x" 0
                )
                 (
                  cons "y" 0
                )
              )
            )
             (
              alist->hash-table (
                _list (
                  cons "x" 2
                )
                 (
                  cons "y" 2
                )
              )
            )
             #f
          )
        )
         (
          print_result (
            dijkstra grid1 (
              alist->hash-table (
                _list (
                  cons "x" 0
                )
                 (
                  cons "y" 0
                )
              )
            )
             (
              alist->hash-table (
                _list (
                  cons "x" 2
                )
                 (
                  cons "y" 2
                )
              )
            )
             #t
          )
        )
         (
          let (
            (
              grid2 (
                _list (
                  _list 1 1 1
                )
                 (
                  _list 0 0 1
                )
                 (
                  _list 0 1 1
                )
              )
            )
          )
           (
            begin (
              print_result (
                dijkstra grid2 (
                  alist->hash-table (
                    _list (
                      cons "x" 0
                    )
                     (
                      cons "y" 0
                    )
                  )
                )
                 (
                  alist->hash-table (
                    _list (
                      cons "x" 2
                    )
                     (
                      cons "y" 2
                    )
                  )
                )
                 #f
              )
            )
          )
        )
      )
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
