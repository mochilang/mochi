;; Generated on 2025-08-06 22:04 +0700
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
      start16 (
        current-jiffy
      )
    )
     (
      jps19 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        default_alphabet
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              _list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
            )
          )
        )
      )
    )
     (
      define (
        default_frequencies
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            ret2 (
              alist->hash-table (
                _list (
                  cons "a" 0.08497
                )
                 (
                  cons "b" 0.01492
                )
                 (
                  cons "c" 0.02202
                )
                 (
                  cons "d" 0.04253
                )
                 (
                  cons "e" 0.11162
                )
                 (
                  cons "f" 0.02228
                )
                 (
                  cons "g" 0.02015
                )
                 (
                  cons "h" 0.06094
                )
                 (
                  cons "i" 0.07546
                )
                 (
                  cons "j" 0.00153
                )
                 (
                  cons "k" 0.01292
                )
                 (
                  cons "l" 0.04025
                )
                 (
                  cons "m" 0.02406
                )
                 (
                  cons "n" 0.06749
                )
                 (
                  cons "o" 0.07507
                )
                 (
                  cons "p" 0.01929
                )
                 (
                  cons "q" 0.00095
                )
                 (
                  cons "r" 0.07587
                )
                 (
                  cons "s" 0.06327
                )
                 (
                  cons "t" 0.09356
                )
                 (
                  cons "u" 0.02758
                )
                 (
                  cons "v" 0.00978
                )
                 (
                  cons "w" 0.0256
                )
                 (
                  cons "x" 0.0015
                )
                 (
                  cons "y" 0.01994
                )
                 (
                  cons "z" 0.00077
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        index_of xs ch
      )
       (
        call/cc (
          lambda (
            ret3
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
                                _len xs
                              )
                            )
                             (
                              begin (
                                if (
                                  string=? (
                                    list-ref xs i
                                  )
                                   ch
                                )
                                 (
                                  begin (
                                    ret3 i
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
                ret3 (
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
        count_char s ch
      )
       (
        call/cc (
          lambda (
            ret6
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
                                    _len s
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      string=? (
                                        _substring s i (
                                          + i 1
                                        )
                                      )
                                       ch
                                    )
                                     (
                                      begin (
                                        set! count (
                                          + count 1
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
                    ret6 count
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
        decrypt_caesar_with_chi_squared ciphertext cipher_alphabet frequencies_dict case_sensitive
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                alphabet_letters cipher_alphabet
              )
            )
             (
              begin (
                if (
                  equal? (
                    _len alphabet_letters
                  )
                   0
                )
                 (
                  begin (
                    set! alphabet_letters (
                      default_alphabet
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
                    frequencies frequencies_dict
                  )
                )
                 (
                  begin (
                    if (
                      equal? (
                        _len frequencies
                      )
                       0
                    )
                     (
                      begin (
                        set! frequencies (
                          default_frequencies
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
                      not case_sensitive
                    )
                     (
                      begin (
                        set! ciphertext (
                          lower ciphertext
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
                        best_shift 0
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            best_chi 0.0
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                best_text ""
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    shift 0
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
                                                  < shift (
                                                    _len alphabet_letters
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        decrypted ""
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
                                                                          < i (
                                                                            _len ciphertext
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                ch (
                                                                                  _substring ciphertext i (
                                                                                    + i 1
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    idx (
                                                                                      index_of alphabet_letters (
                                                                                        lower ch
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    if (
                                                                                      _ge idx 0
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            m (
                                                                                              _len alphabet_letters
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                new_idx (
                                                                                                  fmod (
                                                                                                    - idx shift
                                                                                                  )
                                                                                                   m
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                if (
                                                                                                  _lt new_idx 0
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    set! new_idx (
                                                                                                      _add new_idx m
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
                                                                                                    new_char (
                                                                                                      list-ref alphabet_letters new_idx
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    if (
                                                                                                      and case_sensitive (
                                                                                                        not (
                                                                                                          string=? ch (
                                                                                                            lower ch
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        set! decrypted (
                                                                                                          string-append decrypted (
                                                                                                            upper new_char
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        set! decrypted (
                                                                                                          string-append decrypted new_char
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
                                                                                      begin (
                                                                                        set! decrypted (
                                                                                          string-append decrypted ch
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
                                                            let (
                                                              (
                                                                chi 0.0
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    lowered (
                                                                      if case_sensitive (
                                                                        lower decrypted
                                                                      )
                                                                       decrypted
                                                                    )
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
                                                                                      < j (
                                                                                        _len alphabet_letters
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            letter (
                                                                                              list-ref alphabet_letters j
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                occ (
                                                                                                  count_char lowered letter
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                if (
                                                                                                  _gt occ 0
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    let (
                                                                                                      (
                                                                                                        occf (
                                                                                                          + 0.0 occ
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        let (
                                                                                                          (
                                                                                                            expected (
                                                                                                              * (
                                                                                                                hash-table-ref/default frequencies letter (
                                                                                                                  quote (
                                                                                                                    
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               occf
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            let (
                                                                                                              (
                                                                                                                diff (
                                                                                                                  - occf expected
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                set! chi (
                                                                                                                  _add chi (
                                                                                                                    * (
                                                                                                                      / (
                                                                                                                        * diff diff
                                                                                                                      )
                                                                                                                       expected
                                                                                                                    )
                                                                                                                     occf
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
                                                                                                set! j (
                                                                                                  + j 1
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
                                                                        if (
                                                                          or (
                                                                            equal? shift 0
                                                                          )
                                                                           (
                                                                            < chi best_chi
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! best_shift shift
                                                                          )
                                                                           (
                                                                            set! best_chi chi
                                                                          )
                                                                           (
                                                                            set! best_text decrypted
                                                                          )
                                                                        )
                                                                         (
                                                                          quote (
                                                                            
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        set! shift (
                                                                          + shift 1
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
                                    ret9 (
                                      alist->hash-table (
                                        _list (
                                          cons "shift" best_shift
                                        )
                                         (
                                          cons "chi" best_chi
                                        )
                                         (
                                          cons "decoded" best_text
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
      let (
        (
          r1 (
            decrypt_caesar_with_chi_squared "dof pz aol jhlzhy jpwoly zv wvwbshy? pa pz avv lhzf av jyhjr!" (
              _list
            )
             (
              alist->hash-table (
                _list
              )
            )
             #f
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
                    string-append (
                      string-append (
                        to-str-space (
                          hash-table-ref r1 "shift"
                        )
                      )
                       ", "
                    )
                     (
                      to-str-space (
                        hash-table-ref r1 "chi"
                      )
                    )
                  )
                   ", "
                )
                 (
                  hash-table-ref r1 "decoded"
                )
              )
            )
             (
              string-append (
                string-append (
                  string-append (
                    string-append (
                      to-str-space (
                        hash-table-ref r1 "shift"
                      )
                    )
                     ", "
                  )
                   (
                    to-str-space (
                      hash-table-ref r1 "chi"
                    )
                  )
                )
                 ", "
              )
               (
                hash-table-ref r1 "decoded"
              )
            )
             (
              to-str (
                string-append (
                  string-append (
                    string-append (
                      string-append (
                        to-str-space (
                          hash-table-ref r1 "shift"
                        )
                      )
                       ", "
                    )
                     (
                      to-str-space (
                        hash-table-ref r1 "chi"
                      )
                    )
                  )
                   ", "
                )
                 (
                  hash-table-ref r1 "decoded"
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
              r2 (
                decrypt_caesar_with_chi_squared "crybd cdbsxq" (
                  _list
                )
                 (
                  alist->hash-table (
                    _list
                  )
                )
                 #f
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
                        string-append (
                          string-append (
                            to-str-space (
                              hash-table-ref r2 "shift"
                            )
                          )
                           ", "
                        )
                         (
                          to-str-space (
                            hash-table-ref r2 "chi"
                          )
                        )
                      )
                       ", "
                    )
                     (
                      hash-table-ref r2 "decoded"
                    )
                  )
                )
                 (
                  string-append (
                    string-append (
                      string-append (
                        string-append (
                          to-str-space (
                            hash-table-ref r2 "shift"
                          )
                        )
                         ", "
                      )
                       (
                        to-str-space (
                          hash-table-ref r2 "chi"
                        )
                      )
                    )
                     ", "
                  )
                   (
                    hash-table-ref r2 "decoded"
                  )
                )
                 (
                  to-str (
                    string-append (
                      string-append (
                        string-append (
                          string-append (
                            to-str-space (
                              hash-table-ref r2 "shift"
                            )
                          )
                           ", "
                        )
                         (
                          to-str-space (
                            hash-table-ref r2 "chi"
                          )
                        )
                      )
                       ", "
                    )
                     (
                      hash-table-ref r2 "decoded"
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
                  r3 (
                    decrypt_caesar_with_chi_squared "Crybd Cdbsxq" (
                      _list
                    )
                     (
                      alist->hash-table (
                        _list
                      )
                    )
                     #t
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
                            string-append (
                              string-append (
                                to-str-space (
                                  hash-table-ref r3 "shift"
                                )
                              )
                               ", "
                            )
                             (
                              to-str-space (
                                hash-table-ref r3 "chi"
                              )
                            )
                          )
                           ", "
                        )
                         (
                          hash-table-ref r3 "decoded"
                        )
                      )
                    )
                     (
                      string-append (
                        string-append (
                          string-append (
                            string-append (
                              to-str-space (
                                hash-table-ref r3 "shift"
                              )
                            )
                             ", "
                          )
                           (
                            to-str-space (
                              hash-table-ref r3 "chi"
                            )
                          )
                        )
                         ", "
                      )
                       (
                        hash-table-ref r3 "decoded"
                      )
                    )
                     (
                      to-str (
                        string-append (
                          string-append (
                            string-append (
                              string-append (
                                to-str-space (
                                  hash-table-ref r3 "shift"
                                )
                              )
                               ", "
                            )
                             (
                              to-str-space (
                                hash-table-ref r3 "chi"
                              )
                            )
                          )
                           ", "
                        )
                         (
                          hash-table-ref r3 "decoded"
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
     (
      let (
        (
          end17 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur18 (
              quotient (
                * (
                  - end17 start16
                )
                 1000000
              )
               jps19
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur18
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
