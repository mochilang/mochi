;; Generated on 2025-08-07 08:40 +0700
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
      start18 (
        current-jiffy
      )
    )
     (
      jps21 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        make_conn_mock
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              alist->hash-table (
                _list (
                  cons "recv_called" 0
                )
                 (
                  cons "send_called" 0
                )
                 (
                  cons "close_called" 0
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        conn_recv conn size
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              hash-table-set! conn "recv_called" (
                + (
                  hash-table-ref conn "recv_called"
                )
                 1
              )
            )
             (
              ret2 0
            )
          )
        )
      )
    )
     (
      define (
        conn_send conn data
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            hash-table-set! conn "send_called" (
              + (
                hash-table-ref conn "send_called"
              )
               1
            )
          )
        )
      )
    )
     (
      define (
        conn_close conn
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            hash-table-set! conn "close_called" (
              + (
                hash-table-ref conn "close_called"
              )
               1
            )
          )
        )
      )
    )
     (
      define (
        make_socket_mock conn
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            ret5 (
              alist->hash-table (
                _list (
                  cons "bind_called" 0
                )
                 (
                  cons "listen_called" 0
                )
                 (
                  cons "accept_called" 0
                )
                 (
                  cons "shutdown_called" 0
                )
                 (
                  cons "close_called" 0
                )
                 (
                  cons "conn" conn
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        socket_bind sock
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            hash-table-set! sock "bind_called" (
              + (
                hash-table-ref sock "bind_called"
              )
               1
            )
          )
        )
      )
    )
     (
      define (
        socket_listen sock
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            hash-table-set! sock "listen_called" (
              + (
                hash-table-ref sock "listen_called"
              )
               1
            )
          )
        )
      )
    )
     (
      define (
        socket_accept sock
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            begin (
              hash-table-set! sock "accept_called" (
                + (
                  hash-table-ref sock "accept_called"
                )
                 1
              )
            )
             (
              ret8 (
                hash-table-ref sock "conn"
              )
            )
          )
        )
      )
    )
     (
      define (
        socket_shutdown sock
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            hash-table-set! sock "shutdown_called" (
              + (
                hash-table-ref sock "shutdown_called"
              )
               1
            )
          )
        )
      )
    )
     (
      define (
        socket_close sock
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            hash-table-set! sock "close_called" (
              + (
                hash-table-ref sock "close_called"
              )
               1
            )
          )
        )
      )
    )
     (
      define (
        make_file_mock values
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            ret11 (
              alist->hash-table (
                _list (
                  cons "read_called" 0
                )
                 (
                  cons "data" values
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        file_read f size
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            begin (
              if (
                < (
                  hash-table-ref f "read_called"
                )
                 (
                  _len (
                    hash-table-ref f "data"
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      value (
                        list-ref (
                          hash-table-ref f "data"
                        )
                         (
                          hash-table-ref f "read_called"
                        )
                      )
                    )
                  )
                   (
                    begin (
                      hash-table-set! f "read_called" (
                        + (
                          hash-table-ref f "read_called"
                        )
                         1
                      )
                    )
                     (
                      ret12 value
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
              hash-table-set! f "read_called" (
                + (
                  hash-table-ref f "read_called"
                )
                 1
              )
            )
             (
              ret12 0
            )
          )
        )
      )
    )
     (
      define (
        file_open
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            ret13 (
              make_file_mock (
                _list 1 0
              )
            )
          )
        )
      )
    )
     (
      define (
        send_file sock f
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            begin (
              socket_bind sock
            )
             (
              socket_listen sock
            )
             (
              let (
                (
                  conn (
                    socket_accept sock
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      _ (
                        conn_recv conn 1024
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          data (
                            file_read f 1024
                          )
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
                                        not (
                                          equal? data 0
                                        )
                                      )
                                       (
                                        begin (
                                          conn_send conn data
                                        )
                                         (
                                          set! data (
                                            file_read f 1024
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
                          conn_close conn
                        )
                         (
                          socket_shutdown sock
                        )
                         (
                          socket_close sock
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
        test_send_file_running_as_expected
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            let (
              (
                conn (
                  make_conn_mock
                )
              )
            )
             (
              begin (
                let (
                  (
                    sock (
                      make_socket_mock conn
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        f (
                          file_open
                        )
                      )
                    )
                     (
                      begin (
                        send_file sock f
                      )
                       (
                        if (
                          and (
                            and (
                              and (
                                and (
                                  and (
                                    and (
                                      and (
                                        and (
                                          equal? (
                                            hash-table-ref sock "bind_called"
                                          )
                                           1
                                        )
                                         (
                                          equal? (
                                            hash-table-ref sock "listen_called"
                                          )
                                           1
                                        )
                                      )
                                       (
                                        equal? (
                                          hash-table-ref sock "accept_called"
                                        )
                                         1
                                      )
                                    )
                                     (
                                      equal? (
                                        hash-table-ref conn "recv_called"
                                      )
                                       1
                                    )
                                  )
                                   (
                                    _ge (
                                      hash-table-ref f "read_called"
                                    )
                                     1
                                  )
                                )
                                 (
                                  equal? (
                                    hash-table-ref conn "send_called"
                                  )
                                   1
                                )
                              )
                               (
                                equal? (
                                  hash-table-ref conn "close_called"
                                )
                                 1
                              )
                            )
                             (
                              equal? (
                                hash-table-ref sock "shutdown_called"
                              )
                               1
                            )
                          )
                           (
                            equal? (
                              hash-table-ref sock "close_called"
                            )
                             1
                          )
                        )
                         (
                          begin (
                            ret17 "pass"
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        ret17 "fail"
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
      _display (
        if (
          string? (
            test_send_file_running_as_expected
          )
        )
         (
          test_send_file_running_as_expected
        )
         (
          to-str (
            test_send_file_running_as_expected
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
          end19 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur20 (
              quotient (
                * (
                  - end19 start18
                )
                 1000000
              )
               jps21
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur20
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
