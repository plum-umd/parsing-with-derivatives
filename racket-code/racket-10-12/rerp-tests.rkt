#lang racket

(require "rerp-core.rkt")

; Examples

(define ab* (★ (∪ (token 'a) (token 'b))))


(recognizes? '(a b a b a) ab*)

(recognizes? '(a b a c) ab*)

