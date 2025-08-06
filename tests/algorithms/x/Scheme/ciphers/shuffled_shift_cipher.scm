;; Generated on 2025-08-06 23:15 +0700
(import (scheme base))
(import (scheme time))
(import (chibi string))
(import (only (scheme char) string-upcase string-downcase))
(import (srfi 69))
(import (srfi 1))
(define _list list)
(import (chibi time) (srfi 98))
(define _now_seeded #f)
(define _now_seed 0)
(define (now)
  (when (not _now_seeded)
    (let ((s (get-environment-variable "MOCHI_NOW_SEED")))
      (when (and s (string->number s))
        (set! _now_seed (string->number s))
        (set! _now_seeded #t))))
  (if _now_seeded
      (begin
        (set! _now_seed (modulo (+ (* _now_seed 1664525) 1013904223) 2147483647))
        _now_seed)
      (exact (floor (* (current-second) 1000000000))))
)
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
      start44 (
        current-jiffy
      )
    )
     (
      jps47 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        ord ch
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                digits "0123456789"
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
                                    _len digits
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      string=? (
                                        _substring digits i (
                                          + i 1
                                        )
                                      )
                                       ch
                                    )
                                     (
                                      begin (
                                        ret1 (
                                          + 48 i
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
                    let (
                      (
                        upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                      )
                    )
                     (
                      begin (
                        set! i 0
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
                                      < i (
                                        _len upper
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          string=? (
                                            _substring upper i (
                                              + i 1
                                            )
                                          )
                                           ch
                                        )
                                         (
                                          begin (
                                            ret1 (
                                              + 65 i
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
                            lower "abcdefghijklmnopqrstuvwxyz"
                          )
                        )
                         (
                          begin (
                            set! i 0
                          )
                           (
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
                                          < i (
                                            _len lower
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              string=? (
                                                _substring lower i (
                                                  + i 1
                                                )
                                              )
                                               ch
                                            )
                                             (
                                              begin (
                                                ret1 (
                                                  + 97 i
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
                            ret1 0
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
        neg_pos iterlist
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                i 1
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
                              < i (
                                _len iterlist
                              )
                            )
                             (
                              begin (
                                list-set! iterlist i (
                                  - (
                                    list-ref iterlist i
                                  )
                                )
                              )
                               (
                                set! i (
                                  + i 2
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
                ret8 iterlist
              )
            )
          )
        )
      )
    )
     (
      define (
        passcode_creator
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            let (
              (
                choices "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
              )
            )
             (
              begin (
                let (
                  (
                    seed (
                      now
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        length (
                          + 10 (
                            _mod seed 11
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            password (
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
                                              < i length
                                            )
                                             (
                                              begin (
                                                set! seed (
                                                  _mod (
                                                    + (
                                                      * seed 1103515245
                                                    )
                                                     12345
                                                  )
                                                   2147483647
                                                )
                                              )
                                               (
                                                let (
                                                  (
                                                    idx (
                                                      _mod seed (
                                                        _len choices
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! password (
                                                      append password (
                                                        _list (
                                                          _substring choices idx (
                                                            + idx 1
                                                          )
                                                        )
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
                                ret11 password
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
        unique_sorted chars
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            let (
              (
                uniq (
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
                                  < i (
                                    _len chars
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        ch (
                                          list-ref chars i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          not (
                                            cond (
                                              (
                                                string? uniq
                                              )
                                               (
                                                if (
                                                  string-contains uniq ch
                                                )
                                                 #t #f
                                              )
                                            )
                                             (
                                              (
                                                hash-table? uniq
                                              )
                                               (
                                                if (
                                                  hash-table-exists? uniq ch
                                                )
                                                 #t #f
                                              )
                                            )
                                             (
                                              else (
                                                if (
                                                  member ch uniq
                                                )
                                                 #t #f
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! uniq (
                                              append uniq (
                                                _list ch
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
                    let (
                      (
                        j 0
                      )
                    )
                     (
                      begin (
                        call/cc (
                          lambda (
                            break18
                          )
                           (
                            letrec (
                              (
                                loop17 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < j (
                                        _len uniq
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            k (
                                              + j 1
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                min_idx j
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
                                                              < k (
                                                                _len uniq
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  string<? (
                                                                    list-ref uniq k
                                                                  )
                                                                   (
                                                                    list-ref uniq min_idx
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! min_idx k
                                                                  )
                                                                )
                                                                 (
                                                                  quote (
                                                                    
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! k (
                                                                  + k 1
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
                                                if (
                                                  not (
                                                    equal? min_idx j
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        tmp (
                                                          list-ref uniq j
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        list-set! uniq j (
                                                          list-ref uniq min_idx
                                                        )
                                                      )
                                                       (
                                                        list-set! uniq min_idx tmp
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
                                                set! j (
                                                  + j 1
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop17
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
                              loop17
                            )
                          )
                        )
                      )
                       (
                        ret14 uniq
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
        make_key_list passcode
      )
       (
        call/cc (
          lambda (
            ret21
          )
           (
            let (
              (
                key_list_options "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~ \t\n"
              )
            )
             (
              begin (
                let (
                  (
                    breakpoints (
                      unique_sorted passcode
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        keys_l (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            temp_list (
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
                                    break23
                                  )
                                   (
                                    letrec (
                                      (
                                        loop22 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i (
                                                _len key_list_options
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    ch (
                                                      _substring key_list_options i (
                                                        + i 1
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! temp_list (
                                                      append temp_list (
                                                        _list ch
                                                      )
                                                    )
                                                  )
                                                   (
                                                    if (
                                                      or (
                                                        cond (
                                                          (
                                                            string? breakpoints
                                                          )
                                                           (
                                                            if (
                                                              string-contains breakpoints ch
                                                            )
                                                             #t #f
                                                          )
                                                        )
                                                         (
                                                          (
                                                            hash-table? breakpoints
                                                          )
                                                           (
                                                            if (
                                                              hash-table-exists? breakpoints ch
                                                            )
                                                             #t #f
                                                          )
                                                        )
                                                         (
                                                          else (
                                                            if (
                                                              member ch breakpoints
                                                            )
                                                             #t #f
                                                          )
                                                        )
                                                      )
                                                       (
                                                        equal? i (
                                                          - (
                                                            _len key_list_options
                                                          )
                                                           1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            k (
                                                              - (
                                                                _len temp_list
                                                              )
                                                               1
                                                            )
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
                                                                          >= k 0
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! keys_l (
                                                                              append keys_l (
                                                                                _list (
                                                                                  list-ref temp_list k
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
                                                            set! temp_list (
                                                              _list
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
                                               (
                                                loop22
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
                                      loop22
                                    )
                                  )
                                )
                              )
                               (
                                ret21 keys_l
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
        make_shift_key passcode
      )
       (
        call/cc (
          lambda (
            ret26
          )
           (
            let (
              (
                codes (
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
                        break28
                      )
                       (
                        letrec (
                          (
                            loop27 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len passcode
                                  )
                                )
                                 (
                                  begin (
                                    set! codes (
                                      append codes (
                                        _list (
                                          ord (
                                            list-ref passcode i
                                          )
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
                                    loop27
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
                          loop27
                        )
                      )
                    )
                  )
                   (
                    set! codes (
                      neg_pos codes
                    )
                  )
                   (
                    let (
                      (
                        total 0
                      )
                    )
                     (
                      begin (
                        set! i 0
                      )
                       (
                        call/cc (
                          lambda (
                            break30
                          )
                           (
                            letrec (
                              (
                                loop29 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len codes
                                      )
                                    )
                                     (
                                      begin (
                                        set! total (
                                          + total (
                                            list-ref codes i
                                          )
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
                                        )
                                      )
                                       (
                                        loop29
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
                              loop29
                            )
                          )
                        )
                      )
                       (
                        if (
                          > total 0
                        )
                         (
                          begin (
                            ret26 total
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        ret26 (
                          _len passcode
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
        new_cipher passcode_str
      )
       (
        call/cc (
          lambda (
            ret31
          )
           (
            let (
              (
                passcode (
                  _list
                )
              )
            )
             (
              begin (
                if (
                  equal? (
                    _len passcode_str
                  )
                   0
                )
                 (
                  begin (
                    set! passcode (
                      passcode_creator
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
                            break33
                          )
                           (
                            letrec (
                              (
                                loop32 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len passcode_str
                                      )
                                    )
                                     (
                                      begin (
                                        set! passcode (
                                          append passcode (
                                            _list (
                                              _substring passcode_str i (
                                                + i 1
                                              )
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
                                        loop32
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
                              loop32
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
                    key_list (
                      make_key_list passcode
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        shift_key (
                          make_shift_key passcode
                        )
                      )
                    )
                     (
                      begin (
                        ret31 (
                          alist->hash-table (
                            _list (
                              cons "passcode" passcode
                            )
                             (
                              cons "key_list" key_list
                            )
                             (
                              cons "shift_key" shift_key
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
        index_of lst ch
      )
       (
        call/cc (
          lambda (
            ret34
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
                    break36
                  )
                   (
                    letrec (
                      (
                        loop35 (
                          lambda (
                            
                          )
                           (
                            if (
                              < i (
                                _len lst
                              )
                            )
                             (
                              begin (
                                if (
                                  string=? (
                                    list-ref lst i
                                  )
                                   ch
                                )
                                 (
                                  begin (
                                    ret34 i
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
                                loop35
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
                      loop35
                    )
                  )
                )
              )
               (
                ret34 (
                  - 1
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        encrypt c plaintext
      )
       (
        call/cc (
          lambda (
            ret37
          )
           (
            let (
              (
                encoded ""
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
                    let (
                      (
                        n (
                          _len (
                            hash-table-ref c "key_list"
                          )
                        )
                      )
                    )
                     (
                      begin (
                        call/cc (
                          lambda (
                            break39
                          )
                           (
                            letrec (
                              (
                                loop38 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len plaintext
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            ch (
                                              _substring plaintext i (
                                                + i 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                position (
                                                  index_of (
                                                    hash-table-ref c "key_list"
                                                  )
                                                   ch
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    new_pos (
                                                      _mod (
                                                        _add position (
                                                          hash-table-ref c "shift_key"
                                                        )
                                                      )
                                                       n
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! encoded (
                                                      string-append encoded (
                                                        list-ref (
                                                          hash-table-ref c "key_list"
                                                        )
                                                         new_pos
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
                                        )
                                      )
                                       (
                                        loop38
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
                              loop38
                            )
                          )
                        )
                      )
                       (
                        ret37 encoded
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
        decrypt c encoded_message
      )
       (
        call/cc (
          lambda (
            ret40
          )
           (
            let (
              (
                decoded ""
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
                    let (
                      (
                        n (
                          _len (
                            hash-table-ref c "key_list"
                          )
                        )
                      )
                    )
                     (
                      begin (
                        call/cc (
                          lambda (
                            break42
                          )
                           (
                            letrec (
                              (
                                loop41 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len encoded_message
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            ch (
                                              _substring encoded_message i (
                                                + i 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                position (
                                                  index_of (
                                                    hash-table-ref c "key_list"
                                                  )
                                                   ch
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    new_pos (
                                                      _mod (
                                                        - position (
                                                          hash-table-ref c "shift_key"
                                                        )
                                                      )
                                                       n
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      _lt new_pos 0
                                                    )
                                                     (
                                                      begin (
                                                        set! new_pos (
                                                          _add new_pos n
                                                        )
                                                      )
                                                    )
                                                     (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! decoded (
                                                      string-append decoded (
                                                        list-ref (
                                                          hash-table-ref c "key_list"
                                                        )
                                                         new_pos
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
                                        )
                                      )
                                       (
                                        loop41
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
                              loop41
                            )
                          )
                        )
                      )
                       (
                        ret40 decoded
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
        test_end_to_end
      )
       (
        call/cc (
          lambda (
            ret43
          )
           (
            let (
              (
                msg "Hello, this is a modified Caesar cipher"
              )
            )
             (
              begin (
                let (
                  (
                    cip (
                      new_cipher ""
                    )
                  )
                )
                 (
                  begin (
                    ret43 (
                      decrypt cip (
                        encrypt cip msg
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
          ssc (
            new_cipher "4PYIXyqeQZr44"
          )
        )
      )
       (
        begin (
          let (
            (
              encoded (
                encrypt ssc "Hello, this is a modified Caesar cipher"
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? encoded
                )
                 encoded (
                  to-str encoded
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
                    decrypt ssc encoded
                  )
                )
                 (
                  decrypt ssc encoded
                )
                 (
                  to-str (
                    decrypt ssc encoded
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
          end45 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur46 (
              quotient (
                * (
                  - end45 start44
                )
                 1000000
              )
               jps47
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur46
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
