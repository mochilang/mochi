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
      start9 (
        current-jiffy
      )
    )
     (
      jps12 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        normalize value
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                > value 255
              )
               (
                begin (
                  ret1 255
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret1 value
            )
          )
        )
      )
    )
     (
      define (
        to_grayscale blue green red
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                gs (
                  _add (
                    _add (
                      * 0.2126 (
                        + 0.0 red
                      )
                    )
                     (
                      * 0.587 (
                        + 0.0 green
                      )
                    )
                  )
                   (
                    * 0.114 (
                      + 0.0 blue
                    )
                  )
                )
              )
            )
             (
              begin (
                ret2 (
                  let (
                    (
                      v3 gs
                    )
                  )
                   (
                    cond (
                      (
                        string? v3
                      )
                       (
                        exact (
                          floor (
                            string->number v3
                          )
                        )
                      )
                    )
                     (
                      (
                        boolean? v3
                      )
                       (
                        if v3 1 0
                      )
                    )
                     (
                      else (
                        exact (
                          floor v3
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
        make_sepia img factor
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                pixel_h (
                  _len img
                )
              )
            )
             (
              begin (
                let (
                  (
                    pixel_v (
                      _len (
                        list-ref img 0
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
                                      < i pixel_h
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
                                                          < j pixel_v
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                pixel (
                                                                  cond (
                                                                    (
                                                                      string? (
                                                                        list-ref img i
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref img i
                                                                      )
                                                                       j (
                                                                        + j 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        list-ref img i
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref img i
                                                                      )
                                                                       j
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref (
                                                                        list-ref img i
                                                                      )
                                                                       j
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    grey (
                                                                      to_grayscale (
                                                                        list-ref pixel 0
                                                                      )
                                                                       (
                                                                        list-ref pixel 1
                                                                      )
                                                                       (
                                                                        list-ref pixel 2
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    list-set! (
                                                                      list-ref img i
                                                                    )
                                                                     j (
                                                                      _list (
                                                                        normalize grey
                                                                      )
                                                                       (
                                                                        normalize (
                                                                          + grey factor
                                                                        )
                                                                      )
                                                                       (
                                                                        normalize (
                                                                          + grey (
                                                                            * 2 factor
                                                                          )
                                                                        )
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
                                            set! i (
                                              + i 1
                                            )
                                          )
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
                        ret4 img
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
          image (
            _list (
              _list (
                _list 10 20 30
              )
               (
                _list 40 50 60
              )
            )
             (
              _list (
                _list 70 80 90
              )
               (
                _list 200 150 100
              )
            )
          )
        )
      )
       (
        begin (
          let (
            (
              sepia (
                make_sepia image 20
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    to-str-space sepia
                  )
                )
                 (
                  to-str-space sepia
                )
                 (
                  to-str (
                    to-str-space sepia
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
          end10 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur11 (
              quotient (
                * (
                  - end10 start9
                )
                 1000000
              )
               jps12
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur11
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
