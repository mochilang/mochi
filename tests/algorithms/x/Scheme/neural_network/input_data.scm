;; Generated on 2025-08-07 16:11 +0700
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
(define (fmod a b) (- a (* (floor (/ a b)) b)))
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
      start10 (
        current-jiffy
      )
    )
     (
      jps13 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        dense_to_one_hot labels num_classes
      )
       (
        call/cc (
          lambda (
            ret1
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
                                    _len labels
                                  )
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
                                            j 0
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
                                                          < j num_classes
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              equal? j (
                                                                list-ref-safe labels i
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list 1
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list 0
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
                                                           (
                                                            loop4
                                                          )
                                                        )
                                                         '(
                                                          
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
                                            set! result (
                                              append result (
                                                _list row
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
                                    loop2
                                  )
                                )
                                 '(
                                  
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
                    ret1 result
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
        new_dataset images labels
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            ret6 (
              alist->hash-table (
                _list (
                  cons "images" images
                )
                 (
                  cons "labels" labels
                )
                 (
                  cons "num_examples" (
                    _len images
                  )
                )
                 (
                  cons "index_in_epoch" 0
                )
                 (
                  cons "epochs_completed" 0
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        next_batch ds batch_size
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                start (
                  hash-table-ref ds "index_in_epoch"
                )
              )
            )
             (
              begin (
                if (
                  > (
                    + start batch_size
                  )
                   (
                    hash-table-ref ds "num_examples"
                  )
                )
                 (
                  begin (
                    let (
                      (
                        rest (
                          - (
                            hash-table-ref ds "num_examples"
                          )
                           start
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            images_rest (
                              take (
                                drop (
                                  hash-table-ref ds "images"
                                )
                                 start
                              )
                               (
                                - (
                                  hash-table-ref ds "num_examples"
                                )
                                 start
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                labels_rest (
                                  take (
                                    drop (
                                      hash-table-ref ds "labels"
                                    )
                                     start
                                  )
                                   (
                                    - (
                                      hash-table-ref ds "num_examples"
                                    )
                                     start
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    new_index (
                                      - batch_size rest
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        images_new (
                                          take (
                                            drop (
                                              hash-table-ref ds "images"
                                            )
                                             0
                                          )
                                           (
                                            - new_index 0
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            labels_new (
                                              take (
                                                drop (
                                                  hash-table-ref ds "labels"
                                                )
                                                 0
                                              )
                                               (
                                                - new_index 0
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                batch_images (
                                                  append images_rest images_new
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    batch_labels (
                                                      append labels_rest labels_new
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        new_ds (
                                                          alist->hash-table (
                                                            _list (
                                                              cons "images" (
                                                                hash-table-ref ds "images"
                                                              )
                                                            )
                                                             (
                                                              cons "labels" (
                                                                hash-table-ref ds "labels"
                                                              )
                                                            )
                                                             (
                                                              cons "num_examples" (
                                                                hash-table-ref ds "num_examples"
                                                              )
                                                            )
                                                             (
                                                              cons "index_in_epoch" new_index
                                                            )
                                                             (
                                                              cons "epochs_completed" (
                                                                + (
                                                                  hash-table-ref ds "epochs_completed"
                                                                )
                                                                 1
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        ret7 (
                                                          alist->hash-table (
                                                            _list (
                                                              cons "dataset" new_ds
                                                            )
                                                             (
                                                              cons "images" batch_images
                                                            )
                                                             (
                                                              cons "labels" batch_labels
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
                  begin (
                    let (
                      (
                        end (
                          + start batch_size
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            batch_images (
                              take (
                                drop (
                                  hash-table-ref ds "images"
                                )
                                 start
                              )
                               (
                                - end start
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                batch_labels (
                                  take (
                                    drop (
                                      hash-table-ref ds "labels"
                                    )
                                     start
                                  )
                                   (
                                    - end start
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    new_ds (
                                      alist->hash-table (
                                        _list (
                                          cons "images" (
                                            hash-table-ref ds "images"
                                          )
                                        )
                                         (
                                          cons "labels" (
                                            hash-table-ref ds "labels"
                                          )
                                        )
                                         (
                                          cons "num_examples" (
                                            hash-table-ref ds "num_examples"
                                          )
                                        )
                                         (
                                          cons "index_in_epoch" end
                                        )
                                         (
                                          cons "epochs_completed" (
                                            hash-table-ref ds "epochs_completed"
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    ret7 (
                                      alist->hash-table (
                                        _list (
                                          cons "dataset" new_ds
                                        )
                                         (
                                          cons "images" batch_images
                                        )
                                         (
                                          cons "labels" batch_labels
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
        read_data_sets train_images train_labels_raw test_images test_labels_raw validation_size num_classes
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                train_labels (
                  dense_to_one_hot train_labels_raw num_classes
                )
              )
            )
             (
              begin (
                let (
                  (
                    test_labels (
                      dense_to_one_hot test_labels_raw num_classes
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        validation_images (
                          take (
                            drop train_images 0
                          )
                           (
                            - validation_size 0
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            validation_labels (
                              if (
                                string? train_labels
                              )
                               (
                                _substring train_labels 0 validation_size
                              )
                               (
                                take (
                                  drop train_labels 0
                                )
                                 (
                                  - validation_size 0
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                train_images_rest (
                                  take (
                                    drop train_images validation_size
                                  )
                                   (
                                    - (
                                      _len train_images
                                    )
                                     validation_size
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    train_labels_rest (
                                      if (
                                        string? train_labels
                                      )
                                       (
                                        _substring train_labels validation_size (
                                          _len train_labels
                                        )
                                      )
                                       (
                                        take (
                                          drop train_labels validation_size
                                        )
                                         (
                                          - (
                                            _len train_labels
                                          )
                                           validation_size
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        train (
                                          new_dataset train_images_rest train_labels_rest
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            validation (
                                              new_dataset validation_images validation_labels
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                testset (
                                                  new_dataset test_images test_labels
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                ret8 (
                                                  alist->hash-table (
                                                    _list (
                                                      cons "train" train
                                                    )
                                                     (
                                                      cons "validation" validation
                                                    )
                                                     (
                                                      cons "test_ds" testset
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
        main
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                train_images (
                  _list (
                    _list 0 1
                  )
                   (
                    _list 1 2
                  )
                   (
                    _list 2 3
                  )
                   (
                    _list 3 4
                  )
                   (
                    _list 4 5
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    train_labels_raw (
                      _list 0 1 2 3 4
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        test_images (
                          _list (
                            _list 5 6
                          )
                           (
                            _list 6 7
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            test_labels_raw (
                              _list 5 6
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                data (
                                  read_data_sets train_images train_labels_raw test_images test_labels_raw 2 10
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    ds (
                                      hash-table-ref data "train"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        res (
                                          next_batch ds 2
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! ds (
                                          hash-table-ref res "dataset"
                                        )
                                      )
                                       (
                                        _display (
                                          if (
                                            string? (
                                              to-str-space (
                                                hash-table-ref res "images"
                                              )
                                            )
                                          )
                                           (
                                            to-str-space (
                                              hash-table-ref res "images"
                                            )
                                          )
                                           (
                                            to-str (
                                              to-str-space (
                                                hash-table-ref res "images"
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
                                                hash-table-ref res "labels"
                                              )
                                            )
                                          )
                                           (
                                            to-str-space (
                                              hash-table-ref res "labels"
                                            )
                                          )
                                           (
                                            to-str (
                                              to-str-space (
                                                hash-table-ref res "labels"
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        newline
                                      )
                                       (
                                        set! res (
                                          next_batch ds 2
                                        )
                                      )
                                       (
                                        set! ds (
                                          hash-table-ref res "dataset"
                                        )
                                      )
                                       (
                                        _display (
                                          if (
                                            string? (
                                              to-str-space (
                                                hash-table-ref res "images"
                                              )
                                            )
                                          )
                                           (
                                            to-str-space (
                                              hash-table-ref res "images"
                                            )
                                          )
                                           (
                                            to-str (
                                              to-str-space (
                                                hash-table-ref res "images"
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
                                                hash-table-ref res "labels"
                                              )
                                            )
                                          )
                                           (
                                            to-str-space (
                                              hash-table-ref res "labels"
                                            )
                                          )
                                           (
                                            to-str (
                                              to-str-space (
                                                hash-table-ref res "labels"
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        newline
                                      )
                                       (
                                        set! res (
                                          next_batch ds 2
                                        )
                                      )
                                       (
                                        set! ds (
                                          hash-table-ref res "dataset"
                                        )
                                      )
                                       (
                                        _display (
                                          if (
                                            string? (
                                              to-str-space (
                                                hash-table-ref res "images"
                                              )
                                            )
                                          )
                                           (
                                            to-str-space (
                                              hash-table-ref res "images"
                                            )
                                          )
                                           (
                                            to-str (
                                              to-str-space (
                                                hash-table-ref res "images"
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
                                                hash-table-ref res "labels"
                                              )
                                            )
                                          )
                                           (
                                            to-str-space (
                                              hash-table-ref res "labels"
                                            )
                                          )
                                           (
                                            to-str (
                                              to-str-space (
                                                hash-table-ref res "labels"
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
     (
      main
    )
     (
      let (
        (
          end11 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur12 (
              quotient (
                * (
                  - end11 start10
                )
                 1000000
              )
               jps13
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur12
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
