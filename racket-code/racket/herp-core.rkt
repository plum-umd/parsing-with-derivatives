(module herp-core
  racket
  
  ; Herp Core implements the minimal core 
  ; of a context-free language recognizer.
  
  (require "memoization.rkt")
  (require "fixed-points.rkt")
  (require "lazy-structs.rkt")

  (provide (all-defined-out))
  
  ; Atomic languages:
  (define-struct ∅     {})       ; empty set
  (define-struct ε     {})       ; empty string
  (define-struct token {value})  ; exact terminal
  
  ; Compound languages:
  (define-lazy-struct δ {lang})       ; nullability
  (define-lazy-struct ∪ {this that})  ; union
  (define-lazy-struct ∘ {left right}) ; concatenation
  (define-lazy-struct ★ {lang})       ; repetition
  
  ; Derivative:
  (define/memoize (D c L)
    #:order ([L #:eq] [c #:equal])
    (match L
      [(∅)           (∅)]
      [(ε)           (∅)]
      [(δ _)         (∅)]
      [(token a)     (cond [(eqv? a c) (ε)]
                           [else       (∅)])]
      
      [(∪ L1 L2)     (∪ (D c L1) 
                        (D c L2))]
      [(★ L1)        (∘ (D c L1) L)]
      [(∘ L1 L2)     (∪ (∘ (δ L1) (D c L2))
                        (∘ (D c L1) L2))]))
  
  ; Nullability:
  (define/fix (nullable? L)
    #:bottom #f
    (match L
      [(∅)           #f]
      [(ε)           #t]
      [(token _)     #f]
      [(★ _)         #t]
      [(δ L1)        (nullable? L1)]
      [(∪ L1 L2)     (or (nullable? L1) (nullable? L2))]
      [(∘ L1 L2)     (and (nullable? L1) (nullable? L2))]))
      
  
  ; Parse a list of tokens:
  (define (recognizes? w L)
    (if (null? w)
        (nullable? L)
        (recognizes? (cdr w) (D (car w) L)))))
