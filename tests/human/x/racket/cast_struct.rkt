#lang racket
(struct Todo (title) #:transparent)
(define todo (Todo "hi"))
(displayln (Todo-title todo))
