#lang racket
(define (triple x)
  (let/ec return
(return (* x 3))
  ))
(displayln (triple (+ 1 2)))
