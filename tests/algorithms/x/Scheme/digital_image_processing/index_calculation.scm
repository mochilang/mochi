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
      start11 (
        current-jiffy
      )
    )
     (
      jps14 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        ndvi red nir
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              _div (
                - nir red
              )
               (
                + nir red
              )
            )
          )
        )
      )
    )
     (
      define (
        bndvi blue nir
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            ret2 (
              _div (
                - nir blue
              )
               (
                + nir blue
              )
            )
          )
        )
      )
    )
     (
      define (
        gndvi green nir
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            ret3 (
              _div (
                - nir green
              )
               (
                + nir green
              )
            )
          )
        )
      )
    )
     (
      define (
        ndre redEdge nir
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            ret4 (
              _div (
                - nir redEdge
              )
               (
                + nir redEdge
              )
            )
          )
        )
      )
    )
     (
      define (
        ccci red redEdge nir
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            ret5 (
              _div (
                ndre redEdge nir
              )
               (
                ndvi red nir
              )
            )
          )
        )
      )
    )
     (
      define (
        cvi red green nir
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            ret6 (
              _div (
                * nir red
              )
               (
                * green green
              )
            )
          )
        )
      )
    )
     (
      define (
        gli red green blue
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            ret7 (
              _div (
                - (
                  - (
                    * 2.0 green
                  )
                   red
                )
                 blue
              )
               (
                _add (
                  _add (
                    * 2.0 green
                  )
                   red
                )
                 blue
              )
            )
          )
        )
      )
    )
     (
      define (
        dvi red nir
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            ret8 (
              _div nir red
            )
          )
        )
      )
    )
     (
      define (
        calc index red green blue redEdge nir
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            begin (
              if (
                string=? index "NDVI"
              )
               (
                begin (
                  ret9 (
                    ndvi red nir
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
                string=? index "BNDVI"
              )
               (
                begin (
                  ret9 (
                    bndvi blue nir
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
                string=? index "GNDVI"
              )
               (
                begin (
                  ret9 (
                    gndvi green nir
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
                string=? index "NDRE"
              )
               (
                begin (
                  ret9 (
                    ndre redEdge nir
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
                string=? index "CCCI"
              )
               (
                begin (
                  ret9 (
                    ccci red redEdge nir
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
                string=? index "CVI"
              )
               (
                begin (
                  ret9 (
                    cvi red green nir
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
                string=? index "GLI"
              )
               (
                begin (
                  ret9 (
                    gli red green blue
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
                string=? index "DVI"
              )
               (
                begin (
                  ret9 (
                    dvi red nir
                  )
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret9 0.0
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
            ret10
          )
           (
            let (
              (
                red 50.0
              )
            )
             (
              begin (
                let (
                  (
                    green 30.0
                  )
                )
                 (
                  begin (
                    let (
                      (
                        blue 10.0
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            redEdge 40.0
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                nir 100.0
                              )
                            )
                             (
                              begin (
                                _display (
                                  if (
                                    string? (
                                      string-append "NDVI=" (
                                        to-str-space (
                                          ndvi red nir
                                        )
                                      )
                                    )
                                  )
                                   (
                                    string-append "NDVI=" (
                                      to-str-space (
                                        ndvi red nir
                                      )
                                    )
                                  )
                                   (
                                    to-str (
                                      string-append "NDVI=" (
                                        to-str-space (
                                          ndvi red nir
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
                                      string-append "CCCI=" (
                                        to-str-space (
                                          ccci red redEdge nir
                                        )
                                      )
                                    )
                                  )
                                   (
                                    string-append "CCCI=" (
                                      to-str-space (
                                        ccci red redEdge nir
                                      )
                                    )
                                  )
                                   (
                                    to-str (
                                      string-append "CCCI=" (
                                        to-str-space (
                                          ccci red redEdge nir
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
                                      string-append "CVI=" (
                                        to-str-space (
                                          cvi red green nir
                                        )
                                      )
                                    )
                                  )
                                   (
                                    string-append "CVI=" (
                                      to-str-space (
                                        cvi red green nir
                                      )
                                    )
                                  )
                                   (
                                    to-str (
                                      string-append "CVI=" (
                                        to-str-space (
                                          cvi red green nir
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
          end12 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur13 (
              quotient (
                * (
                  - end12 start11
                )
                 1000000
              )
               jps14
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur13
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
