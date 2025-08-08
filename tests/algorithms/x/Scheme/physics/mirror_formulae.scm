;; Generated on 2025-08-08 16:57 +0700
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
        ((number? x)
         (if (integer? x)
             (number->string (inexact->exact x))
             (number->string x)))
        (else (number->string x))))
(define (to-str-space x)
  (cond ((pair? x)
         (string-append "[" (string-join (map to-str-space x) " ") "]"))
        ((string? x) x)
        (else (to-str x))))
(define (upper s) (string-upcase s))
(define (lower s) (string-downcase s))
(define _floor floor)
(define (fmod a b) (- a (* (_floor (/ a b)) b)))
(define (_mod a b) (if (and (integer? a) (integer? b)) (modulo a b) (fmod a b)))
(define (_div a b) (if (and (integer? a) (integer? b) (exact? a) (exact? b)) (quotient a b) (/ a b)))
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
(define (list-ref-safe lst idx) (if (and (integer? idx) (>= idx 0) (< idx (length lst))) (list-ref lst idx) '()))
(
  let (
    (
      start2 (
        current-jiffy
      )
    )
     (
      jps5 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        abs_float x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                < x 0.0
              )
               (
                begin (
                  ret1 (
                    - x
                  )
                )
              )
               '(
                
              )
            )
             (
              ret1 x
            )
          )
        )
      )
    )
     (
      define (
        isclose a b tolerance
      )
       (
        _lt (
          abs_float (
            - a b
          )
        )
         tolerance
      )
    )
     (
      define (
        focal_length distance_of_object distance_of_image
      )
       (
        begin (
          if (
            or (
              equal? distance_of_object 0.0
            )
             (
              equal? distance_of_image 0.0
            )
          )
           (
            begin (
              panic "Invalid inputs. Enter non zero values with respect to the sign convention."
            )
          )
           '(
            
          )
        )
         (
          _div 1.0 (
            + (
              _div 1.0 distance_of_object
            )
             (
              _div 1.0 distance_of_image
            )
          )
        )
      )
    )
     (
      define (
        object_distance focal_length distance_of_image
      )
       (
        begin (
          if (
            or (
              equal? distance_of_image 0.0
            )
             (
              equal? focal_length 0.0
            )
          )
           (
            begin (
              panic "Invalid inputs. Enter non zero values with respect to the sign convention."
            )
          )
           '(
            
          )
        )
         (
          _div 1.0 (
            - (
              _div 1.0 focal_length
            )
             (
              _div 1.0 distance_of_image
            )
          )
        )
      )
    )
     (
      define (
        image_distance focal_length distance_of_object
      )
       (
        begin (
          if (
            or (
              equal? distance_of_object 0.0
            )
             (
              equal? focal_length 0.0
            )
          )
           (
            begin (
              panic "Invalid inputs. Enter non zero values with respect to the sign convention."
            )
          )
           '(
            
          )
        )
         (
          _div 1.0 (
            - (
              _div 1.0 focal_length
            )
             (
              _div 1.0 distance_of_object
            )
          )
        )
      )
    )
     (
      define (
        test_focal_length
      )
       (
        let (
          (
            f1 (
              focal_length 10.0 20.0
            )
          )
        )
         (
          begin (
            if (
              not (
                isclose f1 6.66666666666666 1e-08
              )
            )
             (
              begin (
                panic "focal_length test1 failed"
              )
            )
             '(
              
            )
          )
           (
            let (
              (
                f2 (
                  focal_length 9.5 6.7
                )
              )
            )
             (
              begin (
                if (
                  not (
                    isclose f2 3.929012346 1e-08
                  )
                )
                 (
                  begin (
                    panic "focal_length test2 failed"
                  )
                )
                 '(
                  
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        test_object_distance
      )
       (
        let (
          (
            u1 (
              object_distance 30.0 20.0
            )
          )
        )
         (
          begin (
            if (
              not (
                isclose u1 (
                  - 60.0
                )
                 1e-08
              )
            )
             (
              begin (
                panic "object_distance test1 failed"
              )
            )
             '(
              
            )
          )
           (
            let (
              (
                u2 (
                  object_distance 10.5 11.7
                )
              )
            )
             (
              begin (
                if (
                  not (
                    isclose u2 102.375 1e-08
                  )
                )
                 (
                  begin (
                    panic "object_distance test2 failed"
                  )
                )
                 '(
                  
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        test_image_distance
      )
       (
        let (
          (
            v1 (
              image_distance 10.0 40.0
            )
          )
        )
         (
          begin (
            if (
              not (
                isclose v1 13.33333333 1e-08
              )
            )
             (
              begin (
                panic "image_distance test1 failed"
              )
            )
             '(
              
            )
          )
           (
            let (
              (
                v2 (
                  image_distance 1.5 6.7
                )
              )
            )
             (
              begin (
                if (
                  not (
                    isclose v2 1.932692308 1e-08
                  )
                )
                 (
                  begin (
                    panic "image_distance test2 failed"
                  )
                )
                 '(
                  
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
        begin (
          test_focal_length
        )
         (
          test_object_distance
        )
         (
          test_image_distance
        )
         (
          _display (
            if (
              string? (
                to-str-space (
                  focal_length 10.0 20.0
                )
              )
            )
             (
              to-str-space (
                focal_length 10.0 20.0
              )
            )
             (
              to-str (
                to-str-space (
                  focal_length 10.0 20.0
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
                to-str-space (
                  object_distance 30.0 20.0
                )
              )
            )
             (
              to-str-space (
                object_distance 30.0 20.0
              )
            )
             (
              to-str (
                to-str-space (
                  object_distance 30.0 20.0
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
                to-str-space (
                  image_distance 10.0 40.0
                )
              )
            )
             (
              to-str-space (
                image_distance 10.0 40.0
              )
            )
             (
              to-str (
                to-str-space (
                  image_distance 10.0 40.0
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
     (
      main
    )
     (
      let (
        (
          end3 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur4 (
              quotient (
                * (
                  - end3 start2
                )
                 1000000
              )
               jps5
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur4
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
