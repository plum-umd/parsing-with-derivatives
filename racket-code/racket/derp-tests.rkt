#lang racket

(require "derivative-parsers-core.rkt")

; Examples

(define ab* (∪ (∘ ab* (∪ (token 'a) (token 'b)))
               (ε (set '()))))

(define ab*2 (∪ (∘ (∪ (token 'a) (token 'b))
                   ab*2)
                (ε (set '()))))


(parse '(a b b a) ab*)

(parse '(a b b a) ab*2)
                 
