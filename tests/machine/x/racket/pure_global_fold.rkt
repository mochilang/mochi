#lang racket
(define k 2)
(define (inc x)
  (let/ec return
(return (+ x k))
  ))
(displayln (inc 3))
