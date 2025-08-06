;; Generated on 2025-08-06 23:15 +0700
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
      start31 (
        current-jiffy
      )
    )
     (
      jps34 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          triagrams (
            _list "111" "112" "113" "121" "122" "123" "131" "132" "133" "211" "212" "213" "221" "222" "223" "231" "232" "233" "311" "312" "313" "321" "322" "323" "331" "332" "333"
          )
        )
      )
       (
        begin (
          define (
            remove_spaces s
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                let (
                  (
                    res ""
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
                            break3
                          )
                           (
                            letrec (
                              (
                                loop2 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len s
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            c (
                                              _substring s i (
                                                + i 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              not (
                                                string=? c " "
                                              )
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  string-append res c
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
                                        loop2
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
                              loop2
                            )
                          )
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
         (
          define (
            char_to_trigram ch alphabet
          )
           (
            call/cc (
              lambda (
                ret4
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
                        break6
                      )
                       (
                        letrec (
                          (
                            loop5 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len alphabet
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      string=? (
                                        _substring alphabet i (
                                          + i 1
                                        )
                                      )
                                       ch
                                    )
                                     (
                                      begin (
                                        ret4 (
                                          list-ref triagrams i
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
                                    loop5
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
                          loop5
                        )
                      )
                    )
                  )
                   (
                    ret4 ""
                  )
                )
              )
            )
          )
        )
         (
          define (
            trigram_to_char tri alphabet
          )
           (
            call/cc (
              lambda (
                ret7
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
                                    _len triagrams
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      string=? (
                                        list-ref triagrams i
                                      )
                                       tri
                                    )
                                     (
                                      begin (
                                        ret7 (
                                          _substring alphabet i (
                                            + i 1
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
                   (
                    ret7 ""
                  )
                )
              )
            )
          )
        )
         (
          define (
            encrypt_part part alphabet
          )
           (
            call/cc (
              lambda (
                ret10
              )
               (
                let (
                  (
                    one ""
                  )
                )
                 (
                  begin (
                    let (
                      (
                        two ""
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            three ""
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
                                    break12
                                  )
                                   (
                                    letrec (
                                      (
                                        loop11 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i (
                                                _len part
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    tri (
                                                      char_to_trigram (
                                                        _substring part i (
                                                          + i 1
                                                        )
                                                      )
                                                       alphabet
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! one (
                                                      string-append one (
                                                        _substring tri 0 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! two (
                                                      string-append two (
                                                        _substring tri 1 2
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! three (
                                                      string-append three (
                                                        _substring tri 2 3
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
                                                loop11
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
                                      loop11
                                    )
                                  )
                                )
                              )
                               (
                                ret10 (
                                  string-append (
                                    string-append one two
                                  )
                                   three
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
            encrypt_message message alphabet period
          )
           (
            call/cc (
              lambda (
                ret13
              )
               (
                let (
                  (
                    msg (
                      remove_spaces message
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        alpha (
                          remove_spaces alphabet
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          not (
                            equal? (
                              _len alpha
                            )
                             27
                          )
                        )
                         (
                          begin (
                            ret13 ""
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
                            encrypted_numeric ""
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
                                    break15
                                  )
                                   (
                                    letrec (
                                      (
                                        loop14 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i (
                                                _len msg
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    end (
                                                      + i period
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      > end (
                                                        _len msg
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! end (
                                                          _len msg
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
                                                        part (
                                                          _substring msg i end
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! encrypted_numeric (
                                                          string-append encrypted_numeric (
                                                            encrypt_part part alpha
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! i (
                                                          + i period
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                loop14
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
                                      loop14
                                    )
                                  )
                                )
                              )
                               (
                                let (
                                  (
                                    encrypted ""
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        j 0
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
                                                      < j (
                                                        _len encrypted_numeric
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            tri (
                                                              _substring encrypted_numeric j (
                                                                + j 3
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! encrypted (
                                                              string-append encrypted (
                                                                trigram_to_char tri alpha
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! j (
                                                              + j 3
                                                            )
                                                          )
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
                                        ret13 encrypted
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
            decrypt_part part alphabet
          )
           (
            call/cc (
              lambda (
                ret18
              )
               (
                let (
                  (
                    converted ""
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
                            break20
                          )
                           (
                            letrec (
                              (
                                loop19 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len part
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            tri (
                                              char_to_trigram (
                                                _substring part i (
                                                  + i 1
                                                )
                                              )
                                               alphabet
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! converted (
                                              string-append converted tri
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
                                        loop19
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
                              loop19
                            )
                          )
                        )
                      )
                       (
                        let (
                          (
                            result (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                tmp ""
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    j 0
                                  )
                                )
                                 (
                                  begin (
                                    call/cc (
                                      lambda (
                                        break22
                                      )
                                       (
                                        letrec (
                                          (
                                            loop21 (
                                              lambda (
                                                
                                              )
                                               (
                                                if (
                                                  < j (
                                                    _len converted
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! tmp (
                                                      string-append tmp (
                                                        _substring converted j (
                                                          + j 1
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    if (
                                                      equal? (
                                                        _len tmp
                                                      )
                                                       (
                                                        _len part
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! result (
                                                          append result (
                                                            _list tmp
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! tmp ""
                                                      )
                                                    )
                                                     (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! j (
                                                      + j 1
                                                    )
                                                  )
                                                   (
                                                    loop21
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
                                          loop21
                                        )
                                      )
                                    )
                                  )
                                   (
                                    ret18 result
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
            decrypt_message message alphabet period
          )
           (
            call/cc (
              lambda (
                ret23
              )
               (
                let (
                  (
                    msg (
                      remove_spaces message
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        alpha (
                          remove_spaces alphabet
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          not (
                            equal? (
                              _len alpha
                            )
                             27
                          )
                        )
                         (
                          begin (
                            ret23 ""
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
                            decrypted_numeric (
                              _list
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
                                    break25
                                  )
                                   (
                                    letrec (
                                      (
                                        loop24 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i (
                                                _len msg
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    end (
                                                      + i period
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      > end (
                                                        _len msg
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! end (
                                                          _len msg
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
                                                        part (
                                                          _substring msg i end
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            groups (
                                                              decrypt_part part alpha
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                k 0
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                call/cc (
                                                                  lambda (
                                                                    break27
                                                                  )
                                                                   (
                                                                    letrec (
                                                                      (
                                                                        loop26 (
                                                                          lambda (
                                                                            
                                                                          )
                                                                           (
                                                                            if (
                                                                              < k (
                                                                                _len (
                                                                                  cond (
                                                                                    (
                                                                                      string? groups
                                                                                    )
                                                                                     (
                                                                                      _substring groups 0 (
                                                                                        + 0 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? groups
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref groups 0
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref groups 0
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    tri (
                                                                                      string-append (
                                                                                        string-append (
                                                                                          _substring (
                                                                                            cond (
                                                                                              (
                                                                                                string? groups
                                                                                              )
                                                                                               (
                                                                                                _substring groups 0 (
                                                                                                  + 0 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? groups
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref groups 0
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref groups 0
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           k (
                                                                                            + k 1
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          _substring (
                                                                                            cond (
                                                                                              (
                                                                                                string? groups
                                                                                              )
                                                                                               (
                                                                                                _substring groups 1 (
                                                                                                  + 1 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? groups
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref groups 1
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref groups 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           k (
                                                                                            + k 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        _substring (
                                                                                          cond (
                                                                                            (
                                                                                              string? groups
                                                                                            )
                                                                                             (
                                                                                              _substring groups 2 (
                                                                                                + 2 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? groups
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref groups 2
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref groups 2
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         k (
                                                                                          + k 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! decrypted_numeric (
                                                                                      append decrypted_numeric (
                                                                                        _list tri
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! k (
                                                                                      + k 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                loop26
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
                                                                      loop26
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! i (
                                                                  + i period
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
                                                loop24
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
                                      loop24
                                    )
                                  )
                                )
                              )
                               (
                                let (
                                  (
                                    decrypted ""
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        j 0
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
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < j (
                                                        _len decrypted_numeric
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! decrypted (
                                                          string-append decrypted (
                                                            trigram_to_char (
                                                              list-ref decrypted_numeric j
                                                            )
                                                             alpha
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! j (
                                                          + j 1
                                                        )
                                                      )
                                                       (
                                                        loop28
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
                                              loop28
                                            )
                                          )
                                        )
                                      )
                                       (
                                        ret23 decrypted
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
            main
          )
           (
            call/cc (
              lambda (
                ret30
              )
               (
                let (
                  (
                    msg "DEFEND THE EAST WALL OF THE CASTLE."
                  )
                )
                 (
                  begin (
                    let (
                      (
                        alphabet "EPSDUCVWYM.ZLKXNBTFGORIJHAQ"
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            encrypted (
                              encrypt_message msg alphabet 5
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                decrypted (
                                  decrypt_message encrypted alphabet 5
                                )
                              )
                            )
                             (
                              begin (
                                _display (
                                  if (
                                    string? (
                                      string-append "Encrypted: " encrypted
                                    )
                                  )
                                   (
                                    string-append "Encrypted: " encrypted
                                  )
                                   (
                                    to-str (
                                      string-append "Encrypted: " encrypted
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
                                      string-append "Decrypted: " decrypted
                                    )
                                  )
                                   (
                                    string-append "Decrypted: " decrypted
                                  )
                                   (
                                    to-str (
                                      string-append "Decrypted: " decrypted
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
          )
        )
         (
          main
        )
      )
    )
     (
      let (
        (
          end32 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur33 (
              quotient (
                * (
                  - end32 start31
                )
                 1000000
              )
               jps34
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur33
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
