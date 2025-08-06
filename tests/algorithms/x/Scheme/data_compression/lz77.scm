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
      start17 (
        current-jiffy
      )
    )
     (
      jps20 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        token_to_string t
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
                  string-append (
                    string-append (
                      string-append (
                        string-append "(" (
                          to-str-space (
                            hash-table-ref t "offset"
                          )
                        )
                      )
                       ", "
                    )
                     (
                      to-str-space (
                        hash-table-ref t "length"
                      )
                    )
                  )
                   ", "
                )
                 (
                  hash-table-ref t "indicator"
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
        tokens_to_string ts
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                res "["
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
                                    _len ts
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      string-append res (
                                        token_to_string (
                                          list-ref ts i
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
                                        set! res (
                                          string-append res ", "
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
                      string-append res "]"
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
        match_length_from_index text window text_index window_index
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              if (
                or (
                  >= text_index (
                    _len text
                  )
                )
                 (
                  >= window_index (
                    _len window
                  )
                )
              )
               (
                begin (
                  ret5 0
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
                  tc (
                    _substring text text_index (
                      + text_index 1
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      wc (
                        _substring window window_index (
                          + window_index 1
                        )
                      )
                    )
                  )
                   (
                    begin (
                      if (
                        not (
                          string=? tc wc
                        )
                      )
                       (
                        begin (
                          ret5 0
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret5 (
                        _add 1 (
                          match_length_from_index text (
                            string-append window tc
                          )
                           (
                            + text_index 1
                          )
                           (
                            + window_index 1
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
        find_encoding_token text search_buffer
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            begin (
              if (
                equal? (
                  _len text
                )
                 0
              )
               (
                begin (
                  panic "We need some text to work with."
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
                  length 0
                )
              )
               (
                begin (
                  let (
                    (
                      offset 0
                    )
                  )
                   (
                    begin (
                      if (
                        equal? (
                          _len search_buffer
                        )
                         0
                      )
                       (
                        begin (
                          ret6 (
                            alist->hash-table (
                              _list (
                                cons "offset" offset
                              )
                               (
                                cons "length" length
                              )
                               (
                                cons "indicator" (
                                  _substring text 0 1
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
                          i 0
                        )
                      )
                       (
                        begin (
                          call/cc (
                            lambda (
                              break8
                            )
                             (
                              letrec (
                                (
                                  loop7 (
                                    lambda (
                                      
                                    )
                                     (
                                      if (
                                        < i (
                                          _len search_buffer
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              ch (
                                                _substring search_buffer i (
                                                  + i 1
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  found_offset (
                                                    - (
                                                      _len search_buffer
                                                    )
                                                     i
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  if (
                                                    string=? ch (
                                                      _substring text 0 1
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          found_length (
                                                            match_length_from_index text search_buffer 0 i
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          if (
                                                            _ge found_length length
                                                          )
                                                           (
                                                            begin (
                                                              set! offset found_offset
                                                            )
                                                             (
                                                              set! length found_length
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
                                          loop7
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
                                loop7
                              )
                            )
                          )
                        )
                         (
                          ret6 (
                            alist->hash-table (
                              _list (
                                cons "offset" offset
                              )
                               (
                                cons "length" length
                              )
                               (
                                cons "indicator" (
                                  _substring text length (
                                    + length 1
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
        lz77_compress text window_size lookahead
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                search_buffer_size (
                  - window_size lookahead
                )
              )
            )
             (
              begin (
                let (
                  (
                    output (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        search_buffer ""
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            remaining text
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
                                          > (
                                            _len remaining
                                          )
                                           0
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                token (
                                                  find_encoding_token remaining search_buffer
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    add_len (
                                                      _add (
                                                        hash-table-ref token "length"
                                                      )
                                                       1
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! search_buffer (
                                                      string-append search_buffer (
                                                        _substring remaining 0 add_len
                                                      )
                                                    )
                                                  )
                                                   (
                                                    if (
                                                      > (
                                                        _len search_buffer
                                                      )
                                                       search_buffer_size
                                                    )
                                                     (
                                                      begin (
                                                        set! search_buffer (
                                                          _substring search_buffer (
                                                            - (
                                                              _len search_buffer
                                                            )
                                                             search_buffer_size
                                                          )
                                                           (
                                                            _len search_buffer
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
                                                    set! remaining (
                                                      _substring remaining add_len (
                                                        _len remaining
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! output (
                                                      append output (
                                                        _list token
                                                      )
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
                            ret9 output
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
        lz77_decompress tokens
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                output ""
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
                                    t (
                                      car xs
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
                                            break16
                                          )
                                           (
                                            letrec (
                                              (
                                                loop15 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      _lt i (
                                                        hash-table-ref t "length"
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! output (
                                                          string-append output (
                                                            _substring output (
                                                              - (
                                                                _len output
                                                              )
                                                               (
                                                                hash-table-ref t "offset"
                                                              )
                                                            )
                                                             (
                                                              _add (
                                                                - (
                                                                  _len output
                                                                )
                                                                 (
                                                                  hash-table-ref t "offset"
                                                                )
                                                              )
                                                               1
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
                                                        loop15
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
                                              loop15
                                            )
                                          )
                                        )
                                      )
                                       (
                                        set! output (
                                          string-append output (
                                            hash-table-ref t "indicator"
                                          )
                                        )
                                      )
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
                      loop13 tokens
                    )
                  )
                )
              )
               (
                ret12 output
              )
            )
          )
        )
      )
    )
     (
      let (
        (
          c1 (
            lz77_compress "ababcbababaa" 13 6
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                tokens_to_string c1
              )
            )
             (
              tokens_to_string c1
            )
             (
              to-str (
                tokens_to_string c1
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
              c2 (
                lz77_compress "aacaacabcabaaac" 13 6
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    tokens_to_string c2
                  )
                )
                 (
                  tokens_to_string c2
                )
                 (
                  to-str (
                    tokens_to_string c2
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
                  tokens_example (
                    _list (
                      alist->hash-table (
                        _list (
                          cons "offset" 0
                        )
                         (
                          cons "length" 0
                        )
                         (
                          cons "indicator" "c"
                        )
                      )
                    )
                     (
                      alist->hash-table (
                        _list (
                          cons "offset" 0
                        )
                         (
                          cons "length" 0
                        )
                         (
                          cons "indicator" "a"
                        )
                      )
                    )
                     (
                      alist->hash-table (
                        _list (
                          cons "offset" 0
                        )
                         (
                          cons "length" 0
                        )
                         (
                          cons "indicator" "b"
                        )
                      )
                    )
                     (
                      alist->hash-table (
                        _list (
                          cons "offset" 0
                        )
                         (
                          cons "length" 0
                        )
                         (
                          cons "indicator" "r"
                        )
                      )
                    )
                     (
                      alist->hash-table (
                        _list (
                          cons "offset" 3
                        )
                         (
                          cons "length" 1
                        )
                         (
                          cons "indicator" "c"
                        )
                      )
                    )
                     (
                      alist->hash-table (
                        _list (
                          cons "offset" 2
                        )
                         (
                          cons "length" 1
                        )
                         (
                          cons "indicator" "d"
                        )
                      )
                    )
                     (
                      alist->hash-table (
                        _list (
                          cons "offset" 7
                        )
                         (
                          cons "length" 4
                        )
                         (
                          cons "indicator" "r"
                        )
                      )
                    )
                     (
                      alist->hash-table (
                        _list (
                          cons "offset" 3
                        )
                         (
                          cons "length" 5
                        )
                         (
                          cons "indicator" "d"
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
                        lz77_decompress tokens_example
                      )
                    )
                     (
                      lz77_decompress tokens_example
                    )
                     (
                      to-str (
                        lz77_decompress tokens_example
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
          end18 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur19 (
              quotient (
                * (
                  - end18 start17
                )
                 1000000
              )
               jps20
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur19
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
