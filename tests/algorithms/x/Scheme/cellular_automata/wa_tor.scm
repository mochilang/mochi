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
      start42 (
        current-jiffy
      )
    )
     (
      jps45 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          WIDTH 10
        )
      )
       (
        begin (
          let (
            (
              HEIGHT 10
            )
          )
           (
            begin (
              let (
                (
                  PREY_INITIAL_COUNT 20
                )
              )
               (
                begin (
                  let (
                    (
                      PREY_REPRODUCTION_TIME 5
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          PREDATOR_INITIAL_COUNT 5
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              PREDATOR_REPRODUCTION_TIME 20
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  PREDATOR_INITIAL_ENERGY 15
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      PREDATOR_FOOD_VALUE 5
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          TYPE_PREY 0
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              TYPE_PREDATOR 1
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  seed 123456789
                                                )
                                              )
                                               (
                                                begin (
                                                  define (
                                                    rand
                                                  )
                                                   (
                                                    call/cc (
                                                      lambda (
                                                        ret1
                                                      )
                                                       (
                                                        begin (
                                                          set! seed (
                                                            modulo (
                                                              + (
                                                                * seed 1103515245
                                                              )
                                                               12345
                                                            )
                                                             2147483648
                                                          )
                                                        )
                                                         (
                                                          ret1 seed
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  define (
                                                    rand_range max
                                                  )
                                                   (
                                                    call/cc (
                                                      lambda (
                                                        ret2
                                                      )
                                                       (
                                                        ret2 (
                                                          fmod (
                                                            rand
                                                          )
                                                           max
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  define (
                                                    shuffle list_int
                                                  )
                                                   (
                                                    call/cc (
                                                      lambda (
                                                        ret3
                                                      )
                                                       (
                                                        let (
                                                          (
                                                            i (
                                                              - (
                                                                _len list_int
                                                              )
                                                               1
                                                            )
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
                                                                          > i 0
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                j (
                                                                                  rand_range (
                                                                                    + i 1
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    tmp (
                                                                                      list-ref list_int i
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    list-set! list_int i (
                                                                                      list-ref list_int j
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    list-set! list_int j tmp
                                                                                  )
                                                                                   (
                                                                                    set! i (
                                                                                      - i 1
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
                                                            ret3 list_int
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  define (
                                                    create_board
                                                  )
                                                   (
                                                    call/cc (
                                                      lambda (
                                                        ret6
                                                      )
                                                       (
                                                        let (
                                                          (
                                                            board (
                                                              _list
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                r 0
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
                                                                              < r HEIGHT
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    row (
                                                                                      _list
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        c 0
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
                                                                                                      < c WIDTH
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        set! row (
                                                                                                          append row (
                                                                                                            _list 0
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        set! c (
                                                                                                          + c 1
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
                                                                                        set! board (
                                                                                          append board (
                                                                                            _list row
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        set! r (
                                                                                          + r 1
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
                                                                ret6 board
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
                                                    create_prey r c
                                                  )
                                                   (
                                                    call/cc (
                                                      lambda (
                                                        ret11
                                                      )
                                                       (
                                                        ret11 (
                                                          _list TYPE_PREY r c PREY_REPRODUCTION_TIME 0 1
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  define (
                                                    create_predator r c
                                                  )
                                                   (
                                                    call/cc (
                                                      lambda (
                                                        ret12
                                                      )
                                                       (
                                                        ret12 (
                                                          _list TYPE_PREDATOR r c PREDATOR_REPRODUCTION_TIME PREDATOR_INITIAL_ENERGY 1
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  let (
                                                    (
                                                      board (
                                                        create_board
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          entities (
                                                            _list
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          define (
                                                            empty_cell r c
                                                          )
                                                           (
                                                            call/cc (
                                                              lambda (
                                                                ret13
                                                              )
                                                               (
                                                                ret13 (
                                                                  equal? (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          cond (
                                                                            (
                                                                              string? board
                                                                            )
                                                                             (
                                                                              _substring board r (
                                                                                + r 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? board
                                                                            )
                                                                             (
                                                                              hash-table-ref board r
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref board r
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          cond (
                                                                            (
                                                                              string? board
                                                                            )
                                                                             (
                                                                              _substring board r (
                                                                                + r 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? board
                                                                            )
                                                                             (
                                                                              hash-table-ref board r
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref board r
                                                                            )
                                                                          )
                                                                        )
                                                                         c (
                                                                          + c 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          cond (
                                                                            (
                                                                              string? board
                                                                            )
                                                                             (
                                                                              _substring board r (
                                                                                + r 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? board
                                                                            )
                                                                             (
                                                                              hash-table-ref board r
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref board r
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          cond (
                                                                            (
                                                                              string? board
                                                                            )
                                                                             (
                                                                              _substring board r (
                                                                                + r 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? board
                                                                            )
                                                                             (
                                                                              hash-table-ref board r
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref board r
                                                                            )
                                                                          )
                                                                        )
                                                                         c
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          cond (
                                                                            (
                                                                              string? board
                                                                            )
                                                                             (
                                                                              _substring board r (
                                                                                + r 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? board
                                                                            )
                                                                             (
                                                                              hash-table-ref board r
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref board r
                                                                            )
                                                                          )
                                                                        )
                                                                         c
                                                                      )
                                                                    )
                                                                  )
                                                                   0
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          define (
                                                            add_entity typ
                                                          )
                                                           (
                                                            call/cc (
                                                              lambda (
                                                                ret14
                                                              )
                                                               (
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
                                                                            if #t (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    r (
                                                                                      rand_range HEIGHT
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        c (
                                                                                          rand_range WIDTH
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        if (
                                                                                          empty_cell r c
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            if (
                                                                                              equal? typ TYPE_PREY
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                list-set! (
                                                                                                  list-ref board r
                                                                                                )
                                                                                                 c 1
                                                                                              )
                                                                                               (
                                                                                                set! entities (
                                                                                                  append entities (
                                                                                                    _list (
                                                                                                      create_prey r c
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                list-set! (
                                                                                                  list-ref board r
                                                                                                )
                                                                                                 c 2
                                                                                              )
                                                                                               (
                                                                                                set! entities (
                                                                                                  append entities (
                                                                                                    _list (
                                                                                                      create_predator r c
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            ret14 (
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
                                                            )
                                                          )
                                                        )
                                                         (
                                                          define (
                                                            setup
                                                          )
                                                           (
                                                            call/cc (
                                                              lambda (
                                                                ret17
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
                                                                        break19
                                                                      )
                                                                       (
                                                                        letrec (
                                                                          (
                                                                            loop18 (
                                                                              lambda (
                                                                                
                                                                              )
                                                                               (
                                                                                if (
                                                                                  < i PREY_INITIAL_COUNT
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    add_entity TYPE_PREY
                                                                                  )
                                                                                   (
                                                                                    set! i (
                                                                                      + i 1
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    loop18
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
                                                                          loop18
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! i 0
                                                                  )
                                                                   (
                                                                    call/cc (
                                                                      lambda (
                                                                        break21
                                                                      )
                                                                       (
                                                                        letrec (
                                                                          (
                                                                            loop20 (
                                                                              lambda (
                                                                                
                                                                              )
                                                                               (
                                                                                if (
                                                                                  < i PREDATOR_INITIAL_COUNT
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    add_entity TYPE_PREDATOR
                                                                                  )
                                                                                   (
                                                                                    set! i (
                                                                                      + i 1
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    loop20
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
                                                                          loop20
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
                                                              dr (
                                                                _list (
                                                                  - 1
                                                                )
                                                                 0 1 0
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  dc (
                                                                    _list 0 1 0 (
                                                                      - 1
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  define (
                                                                    inside r c
                                                                  )
                                                                   (
                                                                    call/cc (
                                                                      lambda (
                                                                        ret22
                                                                      )
                                                                       (
                                                                        ret22 (
                                                                          and (
                                                                            and (
                                                                              and (
                                                                                >= r 0
                                                                              )
                                                                               (
                                                                                < r HEIGHT
                                                                              )
                                                                            )
                                                                             (
                                                                              >= c 0
                                                                            )
                                                                          )
                                                                           (
                                                                            < c WIDTH
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  define (
                                                                    find_prey r c
                                                                  )
                                                                   (
                                                                    call/cc (
                                                                      lambda (
                                                                        ret23
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
                                                                                            _len entities
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                e (
                                                                                                  list-ref entities i
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                if (
                                                                                                  and (
                                                                                                    and (
                                                                                                      and (
                                                                                                        equal? (
                                                                                                          list-ref e 5
                                                                                                        )
                                                                                                         1
                                                                                                      )
                                                                                                       (
                                                                                                        equal? (
                                                                                                          list-ref e 0
                                                                                                        )
                                                                                                         TYPE_PREY
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      equal? (
                                                                                                        list-ref e 1
                                                                                                      )
                                                                                                       r
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    equal? (
                                                                                                      list-ref e 2
                                                                                                    )
                                                                                                     c
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    ret23 i
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
                                                                            ret23 (
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
                                                                    step_world
                                                                  )
                                                                   (
                                                                    call/cc (
                                                                      lambda (
                                                                        ret26
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
                                                                                            _len entities
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                e (
                                                                                                  list-ref entities i
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                if (
                                                                                                  equal? (
                                                                                                    list-ref e 5
                                                                                                  )
                                                                                                   0
                                                                                                )
                                                                                                 (
                                                                                                  begin (
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
                                                                                               (
                                                                                                let (
                                                                                                  (
                                                                                                    typ (
                                                                                                      list-ref e 0
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    let (
                                                                                                      (
                                                                                                        row (
                                                                                                          list-ref e 1
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        let (
                                                                                                          (
                                                                                                            col (
                                                                                                              list-ref e 2
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            let (
                                                                                                              (
                                                                                                                repro (
                                                                                                                  list-ref e 3
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                let (
                                                                                                                  (
                                                                                                                    energy (
                                                                                                                      list-ref e 4
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    let (
                                                                                                                      (
                                                                                                                        dirs (
                                                                                                                          _list 0 1 2 3
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        set! dirs (
                                                                                                                          shuffle dirs
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        let (
                                                                                                                          (
                                                                                                                            moved #f
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            let (
                                                                                                                              (
                                                                                                                                old_r row
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                let (
                                                                                                                                  (
                                                                                                                                    old_c col
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  begin (
                                                                                                                                    if (
                                                                                                                                      equal? typ TYPE_PREDATOR
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
                                                                                                                                            let (
                                                                                                                                              (
                                                                                                                                                ate #f
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              begin (
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
                                                                                                                                                              < j 4
                                                                                                                                                            )
                                                                                                                                                             (
                                                                                                                                                              begin (
                                                                                                                                                                let (
                                                                                                                                                                  (
                                                                                                                                                                    d (
                                                                                                                                                                      list-ref dirs j
                                                                                                                                                                    )
                                                                                                                                                                  )
                                                                                                                                                                )
                                                                                                                                                                 (
                                                                                                                                                                  begin (
                                                                                                                                                                    let (
                                                                                                                                                                      (
                                                                                                                                                                        nr (
                                                                                                                                                                          + row (
                                                                                                                                                                            list-ref dr d
                                                                                                                                                                          )
                                                                                                                                                                        )
                                                                                                                                                                      )
                                                                                                                                                                    )
                                                                                                                                                                     (
                                                                                                                                                                      begin (
                                                                                                                                                                        let (
                                                                                                                                                                          (
                                                                                                                                                                            nc (
                                                                                                                                                                              + col (
                                                                                                                                                                                list-ref dc d
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                          )
                                                                                                                                                                        )
                                                                                                                                                                         (
                                                                                                                                                                          begin (
                                                                                                                                                                            if (
                                                                                                                                                                              and (
                                                                                                                                                                                inside nr nc
                                                                                                                                                                              )
                                                                                                                                                                               (
                                                                                                                                                                                equal? (
                                                                                                                                                                                  cond (
                                                                                                                                                                                    (
                                                                                                                                                                                      string? (
                                                                                                                                                                                        cond (
                                                                                                                                                                                          (
                                                                                                                                                                                            string? board
                                                                                                                                                                                          )
                                                                                                                                                                                           (
                                                                                                                                                                                            _substring board nr (
                                                                                                                                                                                              + nr 1
                                                                                                                                                                                            )
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                         (
                                                                                                                                                                                          (
                                                                                                                                                                                            hash-table? board
                                                                                                                                                                                          )
                                                                                                                                                                                           (
                                                                                                                                                                                            hash-table-ref board nr
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                         (
                                                                                                                                                                                          else (
                                                                                                                                                                                            list-ref board nr
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                     (
                                                                                                                                                                                      _substring (
                                                                                                                                                                                        cond (
                                                                                                                                                                                          (
                                                                                                                                                                                            string? board
                                                                                                                                                                                          )
                                                                                                                                                                                           (
                                                                                                                                                                                            _substring board nr (
                                                                                                                                                                                              + nr 1
                                                                                                                                                                                            )
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                         (
                                                                                                                                                                                          (
                                                                                                                                                                                            hash-table? board
                                                                                                                                                                                          )
                                                                                                                                                                                           (
                                                                                                                                                                                            hash-table-ref board nr
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                         (
                                                                                                                                                                                          else (
                                                                                                                                                                                            list-ref board nr
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                      )
                                                                                                                                                                                       nc (
                                                                                                                                                                                        + nc 1
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    (
                                                                                                                                                                                      hash-table? (
                                                                                                                                                                                        cond (
                                                                                                                                                                                          (
                                                                                                                                                                                            string? board
                                                                                                                                                                                          )
                                                                                                                                                                                           (
                                                                                                                                                                                            _substring board nr (
                                                                                                                                                                                              + nr 1
                                                                                                                                                                                            )
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                         (
                                                                                                                                                                                          (
                                                                                                                                                                                            hash-table? board
                                                                                                                                                                                          )
                                                                                                                                                                                           (
                                                                                                                                                                                            hash-table-ref board nr
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                         (
                                                                                                                                                                                          else (
                                                                                                                                                                                            list-ref board nr
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                     (
                                                                                                                                                                                      hash-table-ref (
                                                                                                                                                                                        cond (
                                                                                                                                                                                          (
                                                                                                                                                                                            string? board
                                                                                                                                                                                          )
                                                                                                                                                                                           (
                                                                                                                                                                                            _substring board nr (
                                                                                                                                                                                              + nr 1
                                                                                                                                                                                            )
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                         (
                                                                                                                                                                                          (
                                                                                                                                                                                            hash-table? board
                                                                                                                                                                                          )
                                                                                                                                                                                           (
                                                                                                                                                                                            hash-table-ref board nr
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                         (
                                                                                                                                                                                          else (
                                                                                                                                                                                            list-ref board nr
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                      )
                                                                                                                                                                                       nc
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    else (
                                                                                                                                                                                      list-ref (
                                                                                                                                                                                        cond (
                                                                                                                                                                                          (
                                                                                                                                                                                            string? board
                                                                                                                                                                                          )
                                                                                                                                                                                           (
                                                                                                                                                                                            _substring board nr (
                                                                                                                                                                                              + nr 1
                                                                                                                                                                                            )
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                         (
                                                                                                                                                                                          (
                                                                                                                                                                                            hash-table? board
                                                                                                                                                                                          )
                                                                                                                                                                                           (
                                                                                                                                                                                            hash-table-ref board nr
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                         (
                                                                                                                                                                                          else (
                                                                                                                                                                                            list-ref board nr
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                      )
                                                                                                                                                                                       nc
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                )
                                                                                                                                                                                 1
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                             (
                                                                                                                                                                              begin (
                                                                                                                                                                                let (
                                                                                                                                                                                  (
                                                                                                                                                                                    prey_index (
                                                                                                                                                                                      find_prey nr nc
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                )
                                                                                                                                                                                 (
                                                                                                                                                                                  begin (
                                                                                                                                                                                    if (
                                                                                                                                                                                      _ge prey_index 0
                                                                                                                                                                                    )
                                                                                                                                                                                     (
                                                                                                                                                                                      begin (
                                                                                                                                                                                        list-set! (
                                                                                                                                                                                          list-ref entities prey_index
                                                                                                                                                                                        )
                                                                                                                                                                                         5 0
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                     (
                                                                                                                                                                                      quote (
                                                                                                                                                                                        
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    list-set! (
                                                                                                                                                                                      list-ref board nr
                                                                                                                                                                                    )
                                                                                                                                                                                     nc 2
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    list-set! (
                                                                                                                                                                                      list-ref board row
                                                                                                                                                                                    )
                                                                                                                                                                                     col 0
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    list-set! e 1 nr
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    list-set! e 2 nc
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    list-set! e 4 (
                                                                                                                                                                                      - (
                                                                                                                                                                                        + energy PREDATOR_FOOD_VALUE
                                                                                                                                                                                      )
                                                                                                                                                                                       1
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    set! moved #t
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    set! ate #t
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    break30 (
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
                                                                                                                                                                            set! j (
                                                                                                                                                                              + j 1
                                                                                                                                                                            )
                                                                                                                                                                          )
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
                                                                                                                                                  not ate
                                                                                                                                                )
                                                                                                                                                 (
                                                                                                                                                  begin (
                                                                                                                                                    set! j 0
                                                                                                                                                  )
                                                                                                                                                   (
                                                                                                                                                    call/cc (
                                                                                                                                                      lambda (
                                                                                                                                                        break32
                                                                                                                                                      )
                                                                                                                                                       (
                                                                                                                                                        letrec (
                                                                                                                                                          (
                                                                                                                                                            loop31 (
                                                                                                                                                              lambda (
                                                                                                                                                                
                                                                                                                                                              )
                                                                                                                                                               (
                                                                                                                                                                if (
                                                                                                                                                                  < j 4
                                                                                                                                                                )
                                                                                                                                                                 (
                                                                                                                                                                  begin (
                                                                                                                                                                    let (
                                                                                                                                                                      (
                                                                                                                                                                        d (
                                                                                                                                                                          list-ref dirs j
                                                                                                                                                                        )
                                                                                                                                                                      )
                                                                                                                                                                    )
                                                                                                                                                                     (
                                                                                                                                                                      begin (
                                                                                                                                                                        let (
                                                                                                                                                                          (
                                                                                                                                                                            nr (
                                                                                                                                                                              + row (
                                                                                                                                                                                list-ref dr d
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                          )
                                                                                                                                                                        )
                                                                                                                                                                         (
                                                                                                                                                                          begin (
                                                                                                                                                                            let (
                                                                                                                                                                              (
                                                                                                                                                                                nc (
                                                                                                                                                                                  + col (
                                                                                                                                                                                    list-ref dc d
                                                                                                                                                                                  )
                                                                                                                                                                                )
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                             (
                                                                                                                                                                              begin (
                                                                                                                                                                                if (
                                                                                                                                                                                  and (
                                                                                                                                                                                    inside nr nc
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    equal? (
                                                                                                                                                                                      cond (
                                                                                                                                                                                        (
                                                                                                                                                                                          string? (
                                                                                                                                                                                            cond (
                                                                                                                                                                                              (
                                                                                                                                                                                                string? board
                                                                                                                                                                                              )
                                                                                                                                                                                               (
                                                                                                                                                                                                _substring board nr (
                                                                                                                                                                                                  + nr 1
                                                                                                                                                                                                )
                                                                                                                                                                                              )
                                                                                                                                                                                            )
                                                                                                                                                                                             (
                                                                                                                                                                                              (
                                                                                                                                                                                                hash-table? board
                                                                                                                                                                                              )
                                                                                                                                                                                               (
                                                                                                                                                                                                hash-table-ref board nr
                                                                                                                                                                                              )
                                                                                                                                                                                            )
                                                                                                                                                                                             (
                                                                                                                                                                                              else (
                                                                                                                                                                                                list-ref board nr
                                                                                                                                                                                              )
                                                                                                                                                                                            )
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                         (
                                                                                                                                                                                          _substring (
                                                                                                                                                                                            cond (
                                                                                                                                                                                              (
                                                                                                                                                                                                string? board
                                                                                                                                                                                              )
                                                                                                                                                                                               (
                                                                                                                                                                                                _substring board nr (
                                                                                                                                                                                                  + nr 1
                                                                                                                                                                                                )
                                                                                                                                                                                              )
                                                                                                                                                                                            )
                                                                                                                                                                                             (
                                                                                                                                                                                              (
                                                                                                                                                                                                hash-table? board
                                                                                                                                                                                              )
                                                                                                                                                                                               (
                                                                                                                                                                                                hash-table-ref board nr
                                                                                                                                                                                              )
                                                                                                                                                                                            )
                                                                                                                                                                                             (
                                                                                                                                                                                              else (
                                                                                                                                                                                                list-ref board nr
                                                                                                                                                                                              )
                                                                                                                                                                                            )
                                                                                                                                                                                          )
                                                                                                                                                                                           nc (
                                                                                                                                                                                            + nc 1
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                      )
                                                                                                                                                                                       (
                                                                                                                                                                                        (
                                                                                                                                                                                          hash-table? (
                                                                                                                                                                                            cond (
                                                                                                                                                                                              (
                                                                                                                                                                                                string? board
                                                                                                                                                                                              )
                                                                                                                                                                                               (
                                                                                                                                                                                                _substring board nr (
                                                                                                                                                                                                  + nr 1
                                                                                                                                                                                                )
                                                                                                                                                                                              )
                                                                                                                                                                                            )
                                                                                                                                                                                             (
                                                                                                                                                                                              (
                                                                                                                                                                                                hash-table? board
                                                                                                                                                                                              )
                                                                                                                                                                                               (
                                                                                                                                                                                                hash-table-ref board nr
                                                                                                                                                                                              )
                                                                                                                                                                                            )
                                                                                                                                                                                             (
                                                                                                                                                                                              else (
                                                                                                                                                                                                list-ref board nr
                                                                                                                                                                                              )
                                                                                                                                                                                            )
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                         (
                                                                                                                                                                                          hash-table-ref (
                                                                                                                                                                                            cond (
                                                                                                                                                                                              (
                                                                                                                                                                                                string? board
                                                                                                                                                                                              )
                                                                                                                                                                                               (
                                                                                                                                                                                                _substring board nr (
                                                                                                                                                                                                  + nr 1
                                                                                                                                                                                                )
                                                                                                                                                                                              )
                                                                                                                                                                                            )
                                                                                                                                                                                             (
                                                                                                                                                                                              (
                                                                                                                                                                                                hash-table? board
                                                                                                                                                                                              )
                                                                                                                                                                                               (
                                                                                                                                                                                                hash-table-ref board nr
                                                                                                                                                                                              )
                                                                                                                                                                                            )
                                                                                                                                                                                             (
                                                                                                                                                                                              else (
                                                                                                                                                                                                list-ref board nr
                                                                                                                                                                                              )
                                                                                                                                                                                            )
                                                                                                                                                                                          )
                                                                                                                                                                                           nc
                                                                                                                                                                                        )
                                                                                                                                                                                      )
                                                                                                                                                                                       (
                                                                                                                                                                                        else (
                                                                                                                                                                                          list-ref (
                                                                                                                                                                                            cond (
                                                                                                                                                                                              (
                                                                                                                                                                                                string? board
                                                                                                                                                                                              )
                                                                                                                                                                                               (
                                                                                                                                                                                                _substring board nr (
                                                                                                                                                                                                  + nr 1
                                                                                                                                                                                                )
                                                                                                                                                                                              )
                                                                                                                                                                                            )
                                                                                                                                                                                             (
                                                                                                                                                                                              (
                                                                                                                                                                                                hash-table? board
                                                                                                                                                                                              )
                                                                                                                                                                                               (
                                                                                                                                                                                                hash-table-ref board nr
                                                                                                                                                                                              )
                                                                                                                                                                                            )
                                                                                                                                                                                             (
                                                                                                                                                                                              else (
                                                                                                                                                                                                list-ref board nr
                                                                                                                                                                                              )
                                                                                                                                                                                            )
                                                                                                                                                                                          )
                                                                                                                                                                                           nc
                                                                                                                                                                                        )
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                     0
                                                                                                                                                                                  )
                                                                                                                                                                                )
                                                                                                                                                                                 (
                                                                                                                                                                                  begin (
                                                                                                                                                                                    list-set! (
                                                                                                                                                                                      list-ref board nr
                                                                                                                                                                                    )
                                                                                                                                                                                     nc 2
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    list-set! (
                                                                                                                                                                                      list-ref board row
                                                                                                                                                                                    )
                                                                                                                                                                                     col 0
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    list-set! e 1 nr
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    list-set! e 2 nc
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    set! moved #t
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    break32 (
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
                                                                                                                                                                                set! j (
                                                                                                                                                                                  + j 1
                                                                                                                                                                                )
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                          )
                                                                                                                                                                        )
                                                                                                                                                                      )
                                                                                                                                                                    )
                                                                                                                                                                  )
                                                                                                                                                                   (
                                                                                                                                                                    loop31
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
                                                                                                                                                          loop31
                                                                                                                                                        )
                                                                                                                                                      )
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                   (
                                                                                                                                                    list-set! e 4 (
                                                                                                                                                      - energy 1
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
                                                                                                                                                  <= (
                                                                                                                                                    list-ref e 4
                                                                                                                                                  )
                                                                                                                                                   0
                                                                                                                                                )
                                                                                                                                                 (
                                                                                                                                                  begin (
                                                                                                                                                    list-set! e 5 0
                                                                                                                                                  )
                                                                                                                                                   (
                                                                                                                                                    list-set! (
                                                                                                                                                      list-ref board (
                                                                                                                                                        list-ref e 1
                                                                                                                                                      )
                                                                                                                                                    )
                                                                                                                                                     (
                                                                                                                                                      list-ref e 2
                                                                                                                                                    )
                                                                                                                                                     0
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
                                                                                                                                                break34
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                letrec (
                                                                                                                                                  (
                                                                                                                                                    loop33 (
                                                                                                                                                      lambda (
                                                                                                                                                        
                                                                                                                                                      )
                                                                                                                                                       (
                                                                                                                                                        if (
                                                                                                                                                          < j 4
                                                                                                                                                        )
                                                                                                                                                         (
                                                                                                                                                          begin (
                                                                                                                                                            let (
                                                                                                                                                              (
                                                                                                                                                                d (
                                                                                                                                                                  list-ref dirs j
                                                                                                                                                                )
                                                                                                                                                              )
                                                                                                                                                            )
                                                                                                                                                             (
                                                                                                                                                              begin (
                                                                                                                                                                let (
                                                                                                                                                                  (
                                                                                                                                                                    nr (
                                                                                                                                                                      + row (
                                                                                                                                                                        list-ref dr d
                                                                                                                                                                      )
                                                                                                                                                                    )
                                                                                                                                                                  )
                                                                                                                                                                )
                                                                                                                                                                 (
                                                                                                                                                                  begin (
                                                                                                                                                                    let (
                                                                                                                                                                      (
                                                                                                                                                                        nc (
                                                                                                                                                                          + col (
                                                                                                                                                                            list-ref dc d
                                                                                                                                                                          )
                                                                                                                                                                        )
                                                                                                                                                                      )
                                                                                                                                                                    )
                                                                                                                                                                     (
                                                                                                                                                                      begin (
                                                                                                                                                                        if (
                                                                                                                                                                          and (
                                                                                                                                                                            inside nr nc
                                                                                                                                                                          )
                                                                                                                                                                           (
                                                                                                                                                                            equal? (
                                                                                                                                                                              cond (
                                                                                                                                                                                (
                                                                                                                                                                                  string? (
                                                                                                                                                                                    cond (
                                                                                                                                                                                      (
                                                                                                                                                                                        string? board
                                                                                                                                                                                      )
                                                                                                                                                                                       (
                                                                                                                                                                                        _substring board nr (
                                                                                                                                                                                          + nr 1
                                                                                                                                                                                        )
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                     (
                                                                                                                                                                                      (
                                                                                                                                                                                        hash-table? board
                                                                                                                                                                                      )
                                                                                                                                                                                       (
                                                                                                                                                                                        hash-table-ref board nr
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                     (
                                                                                                                                                                                      else (
                                                                                                                                                                                        list-ref board nr
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                )
                                                                                                                                                                                 (
                                                                                                                                                                                  _substring (
                                                                                                                                                                                    cond (
                                                                                                                                                                                      (
                                                                                                                                                                                        string? board
                                                                                                                                                                                      )
                                                                                                                                                                                       (
                                                                                                                                                                                        _substring board nr (
                                                                                                                                                                                          + nr 1
                                                                                                                                                                                        )
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                     (
                                                                                                                                                                                      (
                                                                                                                                                                                        hash-table? board
                                                                                                                                                                                      )
                                                                                                                                                                                       (
                                                                                                                                                                                        hash-table-ref board nr
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                     (
                                                                                                                                                                                      else (
                                                                                                                                                                                        list-ref board nr
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                   nc (
                                                                                                                                                                                    + nc 1
                                                                                                                                                                                  )
                                                                                                                                                                                )
                                                                                                                                                                              )
                                                                                                                                                                               (
                                                                                                                                                                                (
                                                                                                                                                                                  hash-table? (
                                                                                                                                                                                    cond (
                                                                                                                                                                                      (
                                                                                                                                                                                        string? board
                                                                                                                                                                                      )
                                                                                                                                                                                       (
                                                                                                                                                                                        _substring board nr (
                                                                                                                                                                                          + nr 1
                                                                                                                                                                                        )
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                     (
                                                                                                                                                                                      (
                                                                                                                                                                                        hash-table? board
                                                                                                                                                                                      )
                                                                                                                                                                                       (
                                                                                                                                                                                        hash-table-ref board nr
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                     (
                                                                                                                                                                                      else (
                                                                                                                                                                                        list-ref board nr
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                )
                                                                                                                                                                                 (
                                                                                                                                                                                  hash-table-ref (
                                                                                                                                                                                    cond (
                                                                                                                                                                                      (
                                                                                                                                                                                        string? board
                                                                                                                                                                                      )
                                                                                                                                                                                       (
                                                                                                                                                                                        _substring board nr (
                                                                                                                                                                                          + nr 1
                                                                                                                                                                                        )
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                     (
                                                                                                                                                                                      (
                                                                                                                                                                                        hash-table? board
                                                                                                                                                                                      )
                                                                                                                                                                                       (
                                                                                                                                                                                        hash-table-ref board nr
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                     (
                                                                                                                                                                                      else (
                                                                                                                                                                                        list-ref board nr
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                   nc
                                                                                                                                                                                )
                                                                                                                                                                              )
                                                                                                                                                                               (
                                                                                                                                                                                else (
                                                                                                                                                                                  list-ref (
                                                                                                                                                                                    cond (
                                                                                                                                                                                      (
                                                                                                                                                                                        string? board
                                                                                                                                                                                      )
                                                                                                                                                                                       (
                                                                                                                                                                                        _substring board nr (
                                                                                                                                                                                          + nr 1
                                                                                                                                                                                        )
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                     (
                                                                                                                                                                                      (
                                                                                                                                                                                        hash-table? board
                                                                                                                                                                                      )
                                                                                                                                                                                       (
                                                                                                                                                                                        hash-table-ref board nr
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                     (
                                                                                                                                                                                      else (
                                                                                                                                                                                        list-ref board nr
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                   nc
                                                                                                                                                                                )
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                             0
                                                                                                                                                                          )
                                                                                                                                                                        )
                                                                                                                                                                         (
                                                                                                                                                                          begin (
                                                                                                                                                                            list-set! (
                                                                                                                                                                              list-ref board nr
                                                                                                                                                                            )
                                                                                                                                                                             nc 1
                                                                                                                                                                          )
                                                                                                                                                                           (
                                                                                                                                                                            list-set! (
                                                                                                                                                                              list-ref board row
                                                                                                                                                                            )
                                                                                                                                                                             col 0
                                                                                                                                                                          )
                                                                                                                                                                           (
                                                                                                                                                                            list-set! e 1 nr
                                                                                                                                                                          )
                                                                                                                                                                           (
                                                                                                                                                                            list-set! e 2 nc
                                                                                                                                                                          )
                                                                                                                                                                           (
                                                                                                                                                                            set! moved #t
                                                                                                                                                                          )
                                                                                                                                                                           (
                                                                                                                                                                            break34 (
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
                                                                                                                                                                        set! j (
                                                                                                                                                                          + j 1
                                                                                                                                                                        )
                                                                                                                                                                      )
                                                                                                                                                                    )
                                                                                                                                                                  )
                                                                                                                                                                )
                                                                                                                                                              )
                                                                                                                                                            )
                                                                                                                                                          )
                                                                                                                                                           (
                                                                                                                                                            loop33
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
                                                                                                                                                  loop33
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    if (
                                                                                                                                      equal? (
                                                                                                                                        list-ref e 5
                                                                                                                                      )
                                                                                                                                       1
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      begin (
                                                                                                                                        if (
                                                                                                                                          and moved (
                                                                                                                                            <= repro 0
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          begin (
                                                                                                                                            if (
                                                                                                                                              equal? typ TYPE_PREY
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              begin (
                                                                                                                                                list-set! (
                                                                                                                                                  list-ref board old_r
                                                                                                                                                )
                                                                                                                                                 old_c 1
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                set! entities (
                                                                                                                                                  append entities (
                                                                                                                                                    _list (
                                                                                                                                                      create_prey old_r old_c
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                list-set! e 3 PREY_REPRODUCTION_TIME
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              begin (
                                                                                                                                                list-set! (
                                                                                                                                                  list-ref board old_r
                                                                                                                                                )
                                                                                                                                                 old_c 2
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                set! entities (
                                                                                                                                                  append entities (
                                                                                                                                                    _list (
                                                                                                                                                      create_predator old_r old_c
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                list-set! e 3 PREDATOR_REPRODUCTION_TIME
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          begin (
                                                                                                                                            list-set! e 3 (
                                                                                                                                              - repro 1
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
                                                                            let (
                                                                              (
                                                                                alive (
                                                                                  _list
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
                                                                                                  < k (
                                                                                                    _len entities
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    let (
                                                                                                      (
                                                                                                        e2 (
                                                                                                          list-ref entities k
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        if (
                                                                                                          equal? (
                                                                                                            list-ref e2 5
                                                                                                          )
                                                                                                           1
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            set! alive (
                                                                                                              append alive (
                                                                                                                _list e2
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
                                                                                                        set! k (
                                                                                                          + k 1
                                                                                                        )
                                                                                                      )
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
                                                                                    set! entities alive
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
                                                                    count_entities typ
                                                                  )
                                                                   (
                                                                    call/cc (
                                                                      lambda (
                                                                        ret37
                                                                      )
                                                                       (
                                                                        let (
                                                                          (
                                                                            cnt 0
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
                                                                                                _len entities
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                if (
                                                                                                  and (
                                                                                                    equal? (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref entities i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref entities i
                                                                                                          )
                                                                                                           0 (
                                                                                                            + 0 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref entities i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref entities i
                                                                                                          )
                                                                                                           0
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref entities i
                                                                                                          )
                                                                                                           0
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     typ
                                                                                                  )
                                                                                                   (
                                                                                                    equal? (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref entities i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref entities i
                                                                                                          )
                                                                                                           5 (
                                                                                                            + 5 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref entities i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref entities i
                                                                                                          )
                                                                                                           5
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref entities i
                                                                                                          )
                                                                                                           5
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     1
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    set! cnt (
                                                                                                      + cnt 1
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
                                                                                ret37 cnt
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  setup
                                                                )
                                                                 (
                                                                  let (
                                                                    (
                                                                      t 0
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      call/cc (
                                                                        lambda (
                                                                          break41
                                                                        )
                                                                         (
                                                                          letrec (
                                                                            (
                                                                              loop40 (
                                                                                lambda (
                                                                                  
                                                                                )
                                                                                 (
                                                                                  if (
                                                                                    < t 10
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      step_world
                                                                                    )
                                                                                     (
                                                                                      set! t (
                                                                                        + t 1
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      loop40
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
                                                                            loop40
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      _display (
                                                                        if (
                                                                          string? (
                                                                            string-append "Prey: " (
                                                                              to-str-space (
                                                                                count_entities TYPE_PREY
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          string-append "Prey: " (
                                                                            to-str-space (
                                                                              count_entities TYPE_PREY
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          to-str (
                                                                            string-append "Prey: " (
                                                                              to-str-space (
                                                                                count_entities TYPE_PREY
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
                                                                            string-append "Predators: " (
                                                                              to-str-space (
                                                                                count_entities TYPE_PREDATOR
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          string-append "Predators: " (
                                                                            to-str-space (
                                                                              count_entities TYPE_PREDATOR
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          to-str (
                                                                            string-append "Predators: " (
                                                                              to-str-space (
                                                                                count_entities TYPE_PREDATOR
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
          end43 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur44 (
              quotient (
                * (
                  - end43 start42
                )
                 1000000
              )
               jps45
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur44
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
