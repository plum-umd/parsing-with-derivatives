(module rerp-core
  racket
  
  ; Rerp Core implements the minimal core 
  ; of a regular language recognizer.
  
  (provide (all-defined-out))
  
  ; Atomic languages:
  (define-struct ∅     {})       ; empty set
  (define-struct ε     {})       ; empty string
  (define-struct token {value})  ; exact terminal
  
  ; Compound languages:
  (define-struct ∪ {this that})  ; union
  (define-struct ∘ {left right}) ; concatenation
  (define-struct ★ {lang})       ; repetition
  (define-struct δ {lang})       ; nullability
  
  ; Derivative:
  (define (D c L)
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
  (define (nullable? L)
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
