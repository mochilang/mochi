#lang racket
(define math (hash 'pi 3.141592653589793 'e 2.718281828459045 'sqrt sqrt 'pow expt 'sin sin 'log log))
(displayln ((hash-ref math 'sqrt) 16))
(displayln (hash-ref math 'pi))
